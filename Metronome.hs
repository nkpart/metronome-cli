{-# language OverloadedLists #-}
module Metronome where

import Lens.Micro.Platform
import Brick.Widgets.List (GenericList(listSelected), listRemove, listReverse, listElementsL, List)
import Q (adjustChance, Q(Always))

data Metronome n = Metronome {
     _metronomeBpm :: Int
   , _metronomeBeats :: List n (Q BeatSound, Bool)
   , _metronomeShouldQuit :: Bool
   }  deriving Show

data BeatSound = Accent | Beat deriving (Eq, Show, Read)

metronomeBpm :: Lens' (Metronome n) Int
metronomeBpm = lens _metronomeBpm (\m b -> m { _metronomeBpm = b})

metronomeBeats :: Lens' (Metronome n) (List n (Q BeatSound, Bool))
metronomeBeats = lens _metronomeBeats (\m b -> m { _metronomeBeats = b})

metronomeShouldQuit :: Lens' (Metronome n) Bool
metronomeShouldQuit = lens _metronomeShouldQuit (\m b -> m { _metronomeShouldQuit = b})

modifyBpm :: (Int -> Int) -> Metronome n -> Metronome n
modifyBpm = over metronomeBpm

setAccent :: Int -> Metronome n -> Metronome n
setAccent n = metronomeBeats . listElementsL . ix n . _1 . mapped %~ toggleAccent

toggleAccentOnSelected :: Metronome n -> Metronome n
toggleAccentOnSelected m =
   do case listSelected (m^.metronomeBeats) of
        Just idx -> setAccent idx m
        Nothing -> m

changeProb :: Int -> Float -> Metronome n -> Metronome n
changeProb n prob = metronomeBeats . listElementsL . ix n . _1 %~ adjustChance prob

changeProbSelected :: Float -> Metronome n -> Metronome n
changeProbSelected prob m = 
   do case listSelected (m^.metronomeBeats) of
        Just idx -> changeProb idx prob m
        Nothing -> m

addBeat :: Metronome n -> Metronome n
addBeat = metronomeBeats . listElementsL %~ (<> [(Always Beat, False)])

removeBeat :: Metronome n -> Metronome n
removeBeat = metronomeBeats %~ listReverse . listRemove 0 . listReverse 

setPlayed :: Int -> Metronome n -> Metronome n
setPlayed n = 
  (metronomeBeats . listElementsL . ix n . _2 .~ True) .
   (metronomeBeats . listElementsL . traverse . _2 .~ False)

setShouldQuit :: Metronome n -> Metronome n
setShouldQuit = set metronomeShouldQuit True

toggleAccent :: BeatSound -> BeatSound
toggleAccent Accent = Beat
toggleAccent Beat = Accent

accent :: Q BeatSound
accent = Always Accent

beat :: Q BeatSound
beat = Always Beat

