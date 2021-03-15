{-# LANGUAGE TupleSections #-}
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

data Args = TUI
          | CLI Int String

cliArgs :: Parser Args
cliArgs = 
  CLI <$> argument auto (metavar "BPM") <*> argument str (metavar "BEAT")

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
run (CLI bpm pattern) = cli bpm pattern

patternToMetronome :: MonadFail m => Int -> String -> m (Metronome [])
patternToMetronome bpm s =
  Metronome bpm <$> traverse handleWord (words s) <*> pure False

toBeat :: [BeatSoundNoCompound] -> Q.Q BeatSound
toBeat [Accent] = Q.Always Accent
toBeat [Beat] = Q.Always Beat
toBeat (x:xs) = Q.Always (E $ x :| xs)
toBeat [] = error "empty pattern word"

handleWord :: MonadFail m => [Char] -> m (Q.Q BeatSound, Bool)
handleWord xs = do
  beats <- traverse charToBeat xs
  pure (toBeat beats, False)

charToBeat :: MonadFail m => Char -> m BeatSoundNoCompound
charToBeat 'A' = pure Accent
charToBeat 'B' = pure Beat
charToBeat 'x' = pure Rest
charToBeat _ = fail "unknown char in pattern"

cli :: Int -> String -> IO ()
cli bpm pattern = do
  tid <- myThreadId
  playback <- initPlayback
  ref <- newIORef =<< patternToMetronome bpm pattern
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

