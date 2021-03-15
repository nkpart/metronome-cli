{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# language OverloadedLists #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Metronome where

import Lens.Micro.Platform
import Lens.Micro.Internal
import Brick.Widgets.List (GenericList(listSelected), listRemove, listReverse, listElementsL, List)
import Q (adjustChance, Q(Always))
import Data.Vector as V

data Metronome f = Metronome {
     _metronomeBpm :: Int
   , _metronomeBeats :: f (Q BeatSound, Bool) -- Bool is isPlayed
   , _metronomeShouldQuit :: Bool
   }

deriving instance (Show (f (Q BeatSound, Bool))) => Show (Metronome f)

data BeatSound = Accent | Beat deriving (Eq, Show, Read)

metronomeBpm :: Lens' (Metronome n) Int
metronomeBpm = lens _metronomeBpm (\m b -> m { _metronomeBpm = b})

metronomeBeats :: Lens' (Metronome f) (f (Q BeatSound, Bool))
metronomeBeats = lens _metronomeBeats (\m b -> m { _metronomeBeats = b})

metronomeShouldQuit :: Lens' (Metronome n) Bool
metronomeShouldQuit = lens _metronomeShouldQuit (\m b -> m { _metronomeShouldQuit = b})

modifyBpm :: (Int -> Int) -> Metronome n -> Metronome n
modifyBpm = over metronomeBpm

setAccent :: Int -> Metronome (GenericList n Vector) -> Metronome (GenericList n Vector)
setAccent n = metronomeBeats . ix n . _1 . mapped %~ toggleAccent

toggleAccentOnSelected :: Metronome (GenericList n Vector) -> Metronome (GenericList n Vector)
toggleAccentOnSelected m =
   do case listSelected (m^.metronomeBeats) of
        Just idx -> setAccent idx m
        Nothing -> m

changeProb :: Int -> Float -> Metronome (GenericList n Vector) -> Metronome (GenericList n Vector)
changeProb n prob = metronomeBeats . ix n . _1 %~ adjustChance prob

changeProbSelected :: Float -> Metronome (GenericList n Vector) -> Metronome (GenericList n Vector)
changeProbSelected prob m = 
   do case listSelected (m^.metronomeBeats) of
        Just idx -> changeProb idx prob m
        Nothing -> m

addBeat :: Metronome (GenericList n Vector) -> Metronome (GenericList n Vector)
addBeat = metronomeBeats . listElementsL %~ (<> [(Always Beat, False)])

removeBeat :: Metronome (GenericList n Vector) -> Metronome (GenericList n Vector)
removeBeat = metronomeBeats %~ listReverse . listRemove 0 . listReverse 

setPlayed :: Int -> Metronome (GenericList n Vector) -> Metronome (GenericList n Vector)
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

type instance Index (List n e) = Int
type instance IxValue (List n e) = e

instance Ixed (List n e) where
   ix n = listElementsL . ix n
