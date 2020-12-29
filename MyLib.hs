{-# LANGUAGE PartialTypeSignatures, OverloadedLists #-}
module MyLib where

import Sound.ProteaAudio (Sound, Sample,  soundPlay )
import Control.Monad (forever )
import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async (cancel,  async )
import System.Random.MWC ( createSystemRandom )
import Q ((++=),  Q(Always), runQ )
import Control.Applicative (Alternative(empty))
import Data.IORef (modifyIORef, IORef, readIORef)
import Control.Lens
import Brick.Widgets.List (listRemove, listReverse, listElementsL, List)

data Metronome = Metronome  {
     metronomeBpm :: Int
   , metronomeBeats :: List () (Q BeatSound, Bool)
   }  deriving Show

modifyBpm :: (Int -> Int) -> IORef Metronome -> IO ()
modifyBpm f ref =
   modifyIORef ref (\met -> met { metronomeBpm = f (metronomeBpm met)})

setAccent :: Int -> IORef Metronome -> IO ()
setAccent n ref = 
   modifyIORef ref (\met -> met { 
      metronomeBeats = metronomeBeats met & listElementsL . ix n . _1 . mapped %~ toggleAccent
    })

changeProb :: Int -> Float -> IORef Metronome -> IO ()
changeProb n prob ref =
   modifyIORef ref (\met -> met { 
      metronomeBeats = metronomeBeats met & listElementsL . ix n . _1 %~ (++= prob)
    })

addBeat :: IORef Metronome -> IO ()
addBeat ref =
   modifyIORef ref (\met -> met { 
      metronomeBeats = metronomeBeats met & listElementsL %~ (<> [(Always Beat, False)])
    })

removeBeat :: IORef Metronome -> IO ()
removeBeat ref =
   modifyIORef ref (\met -> met { 
      metronomeBeats = metronomeBeats met & listReverse . listRemove 0 . listReverse 
    })

setPlayed :: Int -> IORef Metronome -> IO ()
setPlayed n ref = 
   modifyIORef ref (\met -> met { 
      metronomeBeats = metronomeBeats met & listElementsL . traverse . _2 .~ False
                                          & listElementsL . ix n . _2 .~ True
    })

toggleAccent Accent = Beat
toggleAccent Beat = Accent

startMetronome :: Sample -> IORef Metronome -> (Int -> IO ()) -> IO (IO ())
startMetronome clickTrack met beeping = do
     g <- createSystemRandom
     playBeats clickTrack met beeping g

playBeats clickTrack ref beeping g = do
     xs <-
        async $ forever $ do
           loopBeatsRef ref $ \n qb -> do
              bpm <- metronomeBpm <$> readIORef ref
              let gap = 60000 `div` bpm
              threadDelay (gap * 1000)
              _ <- async (runQ g (maybe empty (play clickTrack)) qb)
              _ <- beeping n
              pure ()
     pure $ cancel xs

loopBeatsRef :: IORef Metronome -> (Int -> Q BeatSound -> IO ()) -> IO ()
loopBeatsRef ref f = go 0
  where go n =
             do xs <- metronomeBeats <$> readIORef ref
                if n < length xs 
                   then do f n (xs ^?! listElementsL . ix n. _1)
                           go (succ n)
                   else pure ()

accent :: Q BeatSound
accent = Always Accent

beat :: Q BeatSound
beat = Always Beat

data BeatSound = Accent | Beat deriving (Eq, Show)

play :: Sample -> BeatSound -> IO Sound
play clickTrack bs = 
       let vol = case bs of
                   Accent -> 2.0
                   Beat -> 0.5
           beep = clickTrack
           pitchFactor = case bs of
                           Accent -> 2.5
                           Beat -> 1.0
           leftRightSkew = 0
       in soundPlay beep vol vol leftRightSkew pitchFactor 
