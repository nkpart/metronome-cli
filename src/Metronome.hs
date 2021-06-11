{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# language OverloadedLists #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Metronome where

import Lens.Micro.Platform
import Lens.Micro.Internal
import Brick.Widgets.List (GenericList(listSelected), listRemove, listReverse, listElementsL, List)
import Q (adjustChance, Q(Always))
import Data.Vector as V
import Data.List.NonEmpty
import Data.Void (Void, absurd)
import Data.Functor.Compose (Compose(..))
import GHC.Generics (Generic, Generic1)

newtype BPM = BPM { unBPM :: Int } 
 deriving newtype Read
 deriving (Enum, Ord, Eq, Num, Show)

data Metronome f = Metronome {
     _metronomeBpm :: BPM
   , _metronomeBeats :: f (Q BeatSound) 
   , _metronomeShouldQuit :: Bool
   }

type WithBool = (,) Bool

-- Bool is isPlayed
type UIMetronome n = Metronome (Compose (GenericList n Vector) WithBool)

deriving instance (Show (f (Q BeatSound))) => Show (Metronome f)

deriving instance (Generic1 f) => Generic (Metronome f)

data B x = Accent | Beat | Rest | E x deriving (Eq, Show, Read)
type BeatSoundNoCompound = B Void 
type BeatSound = B (NonEmpty BeatSoundNoCompound)

metronomeBpm :: Lens' (Metronome n) BPM
metronomeBpm = lens _metronomeBpm (\m b -> m { _metronomeBpm = b})

metronomeBeats :: Lens' (Metronome f) (f (Q BeatSound))
metronomeBeats = lens _metronomeBeats (\m b -> m { _metronomeBeats = b})

metronomeShouldQuit :: Lens' (Metronome n) Bool
metronomeShouldQuit = lens _metronomeShouldQuit (\m b -> m { _metronomeShouldQuit = b})

modifyBpm :: (BPM -> BPM) -> Metronome n -> Metronome n
modifyBpm = over metronomeBpm

setAccent :: Int -> UIMetronome n -> UIMetronome n
setAccent n = metronomeBeats . compose . ix n . _2 . mapped %~ toggleAccent

toggleAccentOnSelected :: UIMetronome n -> UIMetronome n
toggleAccentOnSelected m =
   do case listSelected (getCompose $ m^.metronomeBeats) of
        Just idx -> setAccent idx m
        Nothing -> m

changeProb :: Int -> Float -> UIMetronome n -> UIMetronome n
changeProb n prob = metronomeBeats . compose . ix n . _2 %~ adjustChance prob

changeProbSelected :: Float -> UIMetronome n -> UIMetronome n
changeProbSelected prob m = 
   do case listSelected (m^.metronomeBeats . compose) of
        Just idx -> changeProb idx prob m
        Nothing -> m

addBeat :: UIMetronome n -> UIMetronome n
addBeat = metronomeBeats . compose . listElementsL %~ (<> [(False, Always Beat)])

removeBeat :: UIMetronome n -> UIMetronome n
removeBeat = metronomeBeats . compose %~ listReverse . listRemove 0 . listReverse 

setPlayed :: Int -> UIMetronome n -> UIMetronome n
setPlayed n = 
  (metronomeBeats . compose . listElementsL . ix n . _1 .~ True) .
   (metronomeBeats . compose . listElementsL . traverse . _1 .~ False)

setShouldQuit :: Metronome n -> Metronome n
setShouldQuit = set metronomeShouldQuit True

compose :: Lens' (Compose f g a) (f (g a))
compose = lens getCompose (\(Compose _) b -> Compose b)

toggleAccent :: BeatSound -> BeatSound
toggleAccent Accent = Beat
toggleAccent Beat = Accent
toggleAccent Rest = Rest
toggleAccent (E (Accent:|_)) = Beat
toggleAccent (E (Beat:|_)) = Accent
toggleAccent (E (Rest:|_)) = Rest
toggleAccent (E (E v :| _ )) = absurd v

accent :: Q BeatSound
accent = Always Accent

beat :: Q BeatSound
beat = Always Beat

type instance Index (List n e) = Int
type instance IxValue (List n e) = e

instance Ixed (List n e) where
   ix n = listElementsL . ix n
