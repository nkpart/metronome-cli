{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CLI where

import Options.Applicative
import UI
import MyLib
import System.Exit
import System.Posix.Signals
import Control.Concurrent
import qualified Control.Exception as E
import Metronome
import qualified Q
import Data.IORef
import System.Console.ANSI
import System.IO (hFlush, stdout)
import Data.Void (absurd)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Toml
import Data.Foldable (for_)
import System.FSNotify
import qualified Data.Text.IO as TIO
import System.Directory (canonicalizePath)

data Args = TUI
          | CLI (Maybe FilePath) Int String

cliArgs :: Parser Args
cliArgs = 
  CLI <$> optional (strOption (long "watch" <> help "Write config to here for hot reloading")) <*> argument auto (metavar "BPM") <*> argument str (metavar "BEAT")

args :: Parser Args
args = subparser (
              command "tui" (info (pure TUI) (progDesc "Terminal UI"))
           <> command "cli" (info cliArgs (progDesc "Supply a pattern"))
           )

argsMain :: IO ()
argsMain = run =<< execParser opts
  where
    opts = info (args <**> helper)
      ( fullDesc
     <> progDesc "Plays beats, perhaps in time."
     <> header "metronome, my metronome." )

run :: Args -> IO ()
run TUI = uiMain
run (CLI watchMe bpm pattern) = cli watchMe bpm pattern

patternToMetronome :: MonadFail m => Int -> String -> m (Metronome [])
patternToMetronome bpm s =
  Metronome bpm <$> traverse handleWord (words s) <*> pure False

toBeat :: MonadFail m => [BeatSoundNoCompound] -> m (Q.Q BeatSound)
toBeat [Accent] = pure $ Q.Always Accent
toBeat [Beat] = pure $ Q.Always Beat
toBeat (x:xs) = pure $ Q.Always (E $ x :| xs)
toBeat [] = fail "empty pattern word"

handleWord :: MonadFail m => [Char] -> m (Q.Q BeatSound)
handleWord xs = do
  toBeat =<< traverse charToBeat xs

charToBeat :: MonadFail m => Char -> m BeatSoundNoCompound
charToBeat 'A' = pure Accent
charToBeat 'B' = pure Beat
charToBeat 'x' = pure Rest
charToBeat _ = fail "unknown char in pattern"

cli :: Maybe FilePath -> Int -> String -> IO ()
cli watchConfig bpm pattern = withManager $ \mgr -> do
  tid <- myThreadId
  playback <- initPlayback
  ref <- newIORef =<< patternToMetronome bpm pattern

  putStrLn "Starting"

  for_ watchConfig $ \fp -> do
     initWatchFile fp bpm pattern
     _ <- runWatch mgr ref =<< canonicalizePath fp
     pure ()

  _stop <- startMetronome playback ref $ \(bs, idx) -> do 
       if idx == 0
          then do
            clearFromCursorToLineBeginning
            setCursorColumn 0
            putStr $ case Q.qOption bs of
              Accent -> "A "
              Beat -> "B "
              Rest -> "x "
              E v -> absurd v
            hFlush stdout
          else do
            putStr $ case Q.qOption bs of
              Accent -> "A "
              Beat -> "B "
              Rest -> "x "
              E v -> absurd v
            hFlush stdout
       
  _ <- installHandler keyboardSignal (Catch (do
        _stop
        quitPlayback playback
        E.throwTo tid ExitSuccess)) Nothing 

  threadDelay 1000000000
  quitPlayback playback

runWatch :: WatchManager -> IORef (Metronome []) -> FilePath -> IO ()
runWatch mgr ref watchFile =
      let isThis (Modified fp _ _) = fp == watchFile
          isThis _ = False
     in do
          _ <- watchDir mgr "." isThis (readAndUpdate ref)
          pure ()


readAndUpdate :: IORef (Metronome []) -> Event -> IO ()
readAndUpdate ref (Modified fp _ _) =
  do tomlRes <- decodeFileEither metCodec fp
     case tomlRes of
        Left errs      -> TIO.putStrLn $ Toml.prettyTomlDecodeErrors errs
        Right (newBpm, newPattern) -> 
          do met <- patternToMetronome newBpm newPattern
             writeIORef ref met
readAndUpdate _ _ = pure ()


initWatchFile :: FilePath -> Int -> String -> IO ()
initWatchFile fp a b = do
  _ <- encodeToFile metCodec fp (a, b)
  pure ()

metCodec :: TomlCodec (Int, String)
metCodec = (,) <$> Toml.int "bpm" .= fst <*> Toml.string "pattern" .= snd
