{-# LANGUAGE PartialTypeSignatures #-}
module MyLib where

import Control.Monad (forever )
import Control.Concurrent.Async (cancel,  async )
import System.Random.MWC ( createSystemRandom )
import Q (Q, runQ )
import Control.Applicative (Alternative(empty))
import Data.IORef (IORef, readIORef, newIORef, writeIORef)
import Metronome
import Lens.Micro.Platform
import Paths_metronome_cli (getDataFileName)
import qualified SDL.Mixer as Mixer
import qualified SDL
import Data.Foldable (find, for_)

data LoopState = LoopState {
     loopStatePatternStartTicks :: Int,
     loopStateLastTicks :: Int
  } deriving (Eq,Show)

data Playback = Playback {
    playbackBeat :: Mixer.Chunk
  , playbackAccent :: Mixer.Chunk
  }

initPlayback :: IO Playback
initPlayback = do
  SDL.initialize ([SDL.InitAudio] :: [SDL.InitFlag])
  Mixer.initialize ([Mixer.InitMP3]  :: [ Mixer.InitFlag ])
  Mixer.openAudio Mixer.defaultAudio 256
  beatTrack <- Mixer.load =<< getDataFileName "Low Seiko SQ50.wav" 
  accentTrack <- Mixer.load =<< getDataFileName "High Seiko SQ50.wav" 
  pure $ Playback beatTrack accentTrack

quitPlayback :: Playback -> IO ()
quitPlayback (Playback beatTrack accentTrack) = do
  Mixer.free accentTrack
  Mixer.free beatTrack
  Mixer.closeAudio
  Mixer.quit
  SDL.quit

window :: Int
window = 10 -- What should this be, guessing 10ms is okay


startMetronome :: Traversable n => Playback -> IORef (Metronome n) -> ((Q BeatSound, Int) -> IO ()) -> IO (IO ())
startMetronome pb ref beeping = do
     g <- createSystemRandom
     startTicks <- fromIntegral <$> SDL.ticks
     -- TODO, might need an InitLoopState which has no last ticks
     tickVar <- newIORef (LoopState startTicks (startTicks - 100))
     xs <-
        async $ forever $ do
           met <- readIORef ref
           LoopState patternStartTicks' lastTicks' <- readIORef tickVar
           thisTicks' <- fromIntegral <$> SDL.ticks
           newPatternStartTicks <-
                 -- this needs to acocunt for window
                 -- If we are at 1993ms, and the next pattern starts at 2000, that's legit
                 if thisTicks' - patternStartTicks' >= (patternSizeMillis met - window)
                  then -- Increment the pattern start by the size of a whole pattern
                       pure (patternStartTicks' + patternSizeMillis met)
                  else pure patternStartTicks'

           let thisPatternOffset = thisTicks' - newPatternStartTicks
               lastPatternOffset = lastTicks' - newPatternStartTicks

               thisBeat = findBeatAt thisPatternOffset met
               lastBeat = findBeatAt lastPatternOffset met
           if thisBeat /= lastBeat
              then do
                   for_ thisBeat $ \(x,n) -> do
                      _ <- async (runQ g (maybe empty (play pb)) x)
                      beeping (x,n)

              else pure () -- Do nothing if we are still in the same beat window

           writeIORef tickVar (LoopState newPatternStartTicks thisTicks')

           SDL.delay 5 -- This needs to kind of line up with the window, to make sure we don't tick over a beat
           pure ()
     pure $ cancel xs

-- work in ints so negative numbers can happen
findBeatAt :: Traversable n => Int -> Metronome n -> Maybe (Q BeatSound, Int)
findBeatAt offset met =
   let bts = beatTimes met
       thisGuy (_, (_, x)) = offset > (fromIntegral x - fromIntegral window) && offset < fromIntegral x + window
    in (\(a,(b,_)) -> (b,a)) <$> find thisGuy (zip [0..] bts) 

patternSizeMillis :: Foldable n => Metronome n -> Int
patternSizeMillis m = 
   let bpm = view metronomeBpm m
       numBeats = view metronomeBeats m & length
    in fromIntegral $ (60000 `div` bpm) * numBeats

beatTimes :: Traversable n => Metronome n -> [(Q BeatSound, Int)]
beatTimes m =
  let bpm = view metronomeBpm m
      beatMillis = 60000 `div` bpm
      beats = m ^.. metronomeBeats . traverse
      numBeats = beats & length
      beatTimes' = fmap (fromIntegral . (* beatMillis)) [0 .. (numBeats - 1)]
   in if numBeats == 0 
         then error "No beats?" 
         else zip (fmap fst beats) beatTimes'

play :: Playback -> BeatSound -> IO ()
play (Playback beatTrack accentTrack) b = 
   case b of
        Accent -> Mixer.play accentTrack
        Beat -> Mixer.play beatTrack

