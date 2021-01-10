{-# language OverloadedLists #-}
module Metronome where

import Control.Lens
import Brick.Widgets.List (GenericList(listSelected), listRemove, listReverse, listElementsL, List)
import Q ((++=), Q(Always))

data Metronome = Metronome  {
     _metronomeBpm :: Int
   , _metronomeBeats :: List () (Q BeatSound, Bool)
   }  deriving Show

data BeatSound = Accent | Beat deriving (Eq, Show)

metronomeBpm :: Lens' Metronome Int
metronomeBpm = lens _metronomeBpm (\m b -> m { _metronomeBpm = b})

metronomeBeats :: Lens' Metronome (List () (Q BeatSound, Bool))
metronomeBeats = lens _metronomeBeats (\m b -> m { _metronomeBeats = b})

modifyBpm :: (Int -> Int) -> Metronome -> Metronome
modifyBpm = over metronomeBpm

setAccent :: Int -> Metronome -> Metronome
setAccent n = metronomeBeats . listElementsL . ix n . _1 . mapped %~ toggleAccent

toggleAccentOnSelected :: Metronome -> Metronome
toggleAccentOnSelected m =
   do case listSelected (m^.metronomeBeats) of
        Just idx -> setAccent idx m
        Nothing -> m

changeProb :: Int -> Float -> Metronome -> Metronome
changeProb n prob = metronomeBeats . listElementsL . ix n . _1 %~ (++= prob)

changeProbSelected :: Float -> Metronome -> Metronome
changeProbSelected prob m = 
   do case listSelected (m^.metronomeBeats) of
        Just idx -> changeProb idx prob m
        Nothing -> m

addBeat :: Metronome -> Metronome
addBeat = metronomeBeats . listElementsL %~ (<> [(Always Beat, False)])

removeBeat :: Metronome -> Metronome
removeBeat = metronomeBeats %~ listReverse . listRemove 0 . listReverse 

setPlayed :: Int -> Metronome -> Metronome
setPlayed n = 
  (metronomeBeats . listElementsL . ix n . _2 .~ True) .
   (metronomeBeats . listElementsL . traverse . _2 .~ False)

toggleAccent :: BeatSound -> BeatSound
toggleAccent Accent = Beat
toggleAccent Beat = Accent

accent :: Q BeatSound
accent = Always Accent

beat :: Q BeatSound
beat = Always Beat


