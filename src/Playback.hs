{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Playback where

import Control.Monad (forever )
import Control.Concurrent.Async (cancel,  async )
import System.Random.MWC ( createSystemRandom )
import Q (Q, runQ, qOption )
import Control.Applicative (Alternative(empty))
import Data.IORef (IORef, readIORef, newIORef, writeIORef)
import Metronome
import Lens.Micro.Platform
import Paths_metronome_cli (getDataFileName)
import qualified SDL.Mixer as Mixer
import qualified SDL
import Data.Foldable (find, for_)
import Data.List.NonEmpty (toList, NonEmpty)
import Control.Monad.IO.Class (MonadIO)

data LoopState = LoopState {
     loopStatePatternStartTicks :: Ticks,
     loopStateLastTicks :: Ticks
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

window :: Ticks
window = 10 -- What should this be, guessing 10ms is okay

newtype Ticks = Ticks Int deriving (Eq, Show, Ord, Num, Real, Integral, Enum)

startMetronome :: Traversable n => Playback -> IORef (Metronome n) -> ((Q BeatSoundNoCompound, Int) -> IO ()) -> IO (IO ())
startMetronome pb ref beeping = do
     g <- createSystemRandom
     startMetronome2 ref $ \(a,_,b) -> do
       _ <- async (runQ g (maybe empty (play pb)) a)
       beeping (a,b)

startMetronome2 :: Traversable n => IORef (Metronome n) -> ((Q BeatSoundNoCompound, Ticks, Int) -> IO ()) -> IO (IO ())
startMetronome2 metronomeDefinition onBeep = do
     startTicks <- Ticks . fromIntegral <$> SDL.ticks
     -- TODO, might need an InitLoopState which has no last ticks
     tickVar <- newIORef (LoopState startTicks (startTicks - 100))
     xs <-
        async $ 
          forever $ do
           met <- readIORef metronomeDefinition
           LoopState patternStartTicks' lastTicks' <- readIORef tickVar
           thisTicks' <- fromIntegral <$> SDL.ticks
           -- Check if we are starting the pattern over again
           newPatternStartTicks <-
                 -- this needs to account for the window
                 -- If we are at 1993ms, and the next pattern starts at 2000, that's a legit reset of the pattern
                 if thisTicks' - patternStartTicks' >= (metronomePatternSize met - window)
                  then -- Increment the pattern start by the size of a whole pattern
                       pure (patternStartTicks' + metronomePatternSize met)
                  else pure patternStartTicks'

           let thisPatternOffset = thisTicks' - newPatternStartTicks
               lastPatternOffset = lastTicks' - newPatternStartTicks

               thisBeat = findBeatAt thisPatternOffset met
               lastBeat = findBeatAt lastPatternOffset met

           if thisBeat /= lastBeat
              then for_ thisBeat $ \(n,(a,x)) -> onBeep (x,a,n)
              else pure () -- Do nothing if we are still in the same beat window

           writeIORef tickVar (LoopState newPatternStartTicks thisTicks')
           SDL.delay 5 -- This needs to kind of line up with the window, to make sure we don't tick over a beat
           pure ()
     pure $ cancel xs

-- work in ints so negative numbers can happen
findBeatAt :: (Num a, Enum a, Traversable n) => Ticks -> Metronome n -> Maybe (a, (Ticks, Q BeatSoundNoCompound))
findBeatAt offset =
   let isThisBeat x = 
         isNearTo offset (fst . snd $ x)
    in find isThisBeat . indexed . beatTimes

isNearTo :: Ticks -> Ticks -> Bool
isNearTo offset pos =  
  let lb = pos - window
      ub = pos + window
   in offset > lb && offset < ub

metronomePatternSize :: Foldable n => Metronome n -> Ticks
metronomePatternSize m = 
   let bpm = view metronomeBpm m
       numBeats = view metronomeBeats m & length
    in Ticks . fromIntegral $ beatMillis bpm * numBeats


beatMillis :: BPM -> Int
beatMillis (BPM n) = 60000 `div` n

beatTimes :: Traversable n => Metronome n -> [(Ticks, Q BeatSoundNoCompound)]
beatTimes m =
  let bpm = view metronomeBpm m
      beats = m ^.. metronomeBeats . traverse
      beatTimes' = fmap (* Ticks (beatMillis bpm)) [0 .. ]
   in if null beats
         then error "No beats?" 
         -- Produce each outer beat, then expand any compounds
         else zip beatTimes' beats >>= expandCompound (Ticks (beatMillis bpm))

expandCompound :: Ticks -> (Ticks, Q (B (NonEmpty (B x)))) -> [(Ticks, Q (B x))]
expandCompound bm (bt, q) = fmap (over _2 (<$ q)) $ case qOption q of
                          Beat -> pure (bt, Beat)
                          Accent -> pure (bt, Accent)
                          Rest -> pure (bt, Rest)
                          E xs -> fmap (over _1 (\x -> bt + x * (bm `div` Ticks (length xs)))) . indexed . toList $ xs

play :: MonadIO m => Playback -> B x -> m ()
play (Playback beatTrack accentTrack) b = 
   case b of
        Accent -> Mixer.play accentTrack
        Beat -> Mixer.play beatTrack
        Rest -> pure ()
        E _ -> pure ()

indexed :: (Num a, Enum a) => [b] -> [(a, b)]
indexed = zip [0..]
