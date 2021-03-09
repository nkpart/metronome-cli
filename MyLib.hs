{-# LANGUAGE PartialTypeSignatures #-}
module MyLib where

import Control.Monad (forever )
import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async (cancel,  async )
import System.Random.MWC ( createSystemRandom )
import Q (Q, runQ )
import Control.Applicative (Alternative(empty))
import Data.IORef (IORef, readIORef)
import Control.Lens
import Brick.Widgets.List (listElementsL)
import Metronome

import qualified SDL.Mixer as Mixer

startMetronome :: Mixer.Chunk -> IORef (Metronome n) -> (Int -> IO ()) -> IO (IO ())
startMetronome clickTrack ref beeping = do
     g <- createSystemRandom
     xs <-
        async $ forever $ do
           loopBeatsRef ref $ \n qb -> do
              bpm <- view metronomeBpm <$> readIORef ref
              let gap = 60000 `div` bpm
              threadDelay (gap * 1000)
              _ <- async (runQ g (maybe empty (play clickTrack)) qb)
              _ <- beeping n
              pure ()
     pure $ cancel xs

loopBeatsRef :: IORef (Metronome n) -> (Int -> Q BeatSound -> IO ()) -> IO ()
loopBeatsRef ref f = go 0
  where go n =
             do xs <- view metronomeBeats <$> readIORef ref
                if n < length xs 
                   then do f n (xs ^?! listElementsL . ix n. _1)
                           go (succ n)
                   else pure ()

play :: Mixer.Chunk -> BeatSound -> IO ()
play clickTrack _bs = 
   Mixer.play clickTrack
