{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
module MyLib where

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
import Data.Void (absurd)
import Data.List.NonEmpty (toList)

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
     startTicks <- Ticks . fromIntegral <$> SDL.ticks
     -- TODO, might need an InitLoopState which has no last ticks
     tickVar <- newIORef (LoopState startTicks (startTicks - 100))
     xs <-
        async $ forever $ do
           met <- readIORef ref
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
              then for_ thisBeat $ \(x,n) -> do
                      _ <- async (runQ g (maybe empty (play pb)) x)
                      beeping (x,n)
              else pure () -- Do nothing if we are still in the same beat window

           writeIORef tickVar (LoopState newPatternStartTicks thisTicks')

           SDL.delay 5 -- This needs to kind of line up with the window, to make sure we don't tick over a beat
           pure ()
     pure $ cancel xs

-- work in ints so negative numbers can happen
findBeatAt :: Traversable n => Ticks -> Metronome n -> Maybe (Q BeatSoundNoCompound, Int)
findBeatAt offset met =
   let bts = beatTimes met
       isThisBeat x = 
         isNearTo offset (snd . snd $ x)
    in (\(a,(b, _)) -> (b,a)) <$> find isThisBeat (indexed bts) 

indexed :: [b] -> [(Int, b)]
indexed = zip [0..]

isNearTo :: Ticks -> Ticks -> Bool
isNearTo offset pos =  
         offset > (pos - window) && 
         offset < pos + window

metronomePatternSize :: Foldable n => Metronome n -> Ticks
metronomePatternSize m = 
   let bpm = view metronomeBpm m
       numBeats = view metronomeBeats m & length
    in Ticks . fromIntegral $ (60000 `div` bpm) * numBeats

beatTimes :: Traversable n => Metronome n -> [(Q BeatSoundNoCompound, Ticks)]
beatTimes m =
  let bpm = view metronomeBpm m
      beatMillis = Ticks $ 60000 `div` bpm
      beats = m ^.. metronomeBeats . traverse
      numBeats = beats & length
      beatTimes' = fmap ((* beatMillis) . fromIntegral) [0 .. (numBeats - 1)]
   in if numBeats == 0 
         then error "No beats?" 
         -- Produce each outer beat, then expand any compounds
         else zip beats beatTimes' >>= expandCompound beatMillis

expandCompound :: Ticks -> (Q BeatSound, Ticks) -> [(Q BeatSoundNoCompound, Ticks)]
expandCompound beatMillis (q,bt) = case qOption q of
                          Beat -> [(Beat <$ q,bt)]
                          Accent -> [(Accent <$ q,bt)]
                          Rest -> [(Rest <$ q,bt)]
                          E xs -> fmap (xxx bt q (length xs) beatMillis) $ zip [(0::Int)..] $ toList xs

xxx :: Ticks -> Q BeatSound -> Int -> Ticks -> (Int, BeatSoundNoCompound) -> (Q BeatSoundNoCompound, Ticks)
xxx baseTime baseQ beatBeats beatMillis (idx, newSound) =
  (newSound <$ baseQ, baseTime + (Ticks idx * (beatMillis `div` Ticks beatBeats)) )

play :: Playback -> BeatSoundNoCompound -> IO ()
play (Playback beatTrack accentTrack) b = 
   case b of
        Accent -> Mixer.play accentTrack
        Beat -> Mixer.play beatTrack
        Rest -> pure ()
        E v -> absurd v

