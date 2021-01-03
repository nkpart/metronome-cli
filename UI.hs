{-# language OverloadedStrings, OverloadedLists #-}
module UI where

import Brick
    ((<+>), (<=>), customMain,  attrMap,
      continue,
      halt,
      txt,
      App(App, appDraw, appChooseCursor, appHandleEvent, appStartEvent,
          appAttrMap),
      EventM,
      Widget,
      BrickEvent(AppEvent, VtyEvent), Next )
import Graphics.Vty
    (defaultConfig, mkVty,  defAttr, Event(EvKey), Key(KChar), Modifier(MCtrl) )
import MyLib
    (BeatSound(Accent), setPlayed, changeProb, removeBeat, addBeat, setAccent,  Metronome(metronomeBeats, Metronome, metronomeBpm),
      startMetronome,
      beat,
      modifyBpm )
import Sound.ProteaAudio
    ( finishAudio, initAudio, loaderAvailable, sampleFromFile ) 
import Data.String (IsString(fromString))
import Data.IORef (writeIORef, newIORef)
import Brick.BChan ( newBChan, writeBChan )
import Brick.Widgets.List (renderList, list)
import Q (Q(Sometimes, Always))
import Paths_metronome_cli ( getDataFileName )
import Text.Printf ( printf )
import Brick.Widgets.Border (border)
import Control.Monad.IO.Class (MonadIO(liftIO))

newtype AppEvent = 
  Beep Int

clickTrackFile :: IO FilePath
clickTrackFile = getDataFileName "157-click1.wav"

uiMain :: IO ()
uiMain = do
  let initialState = Metronome 114 (list () [(Always Accent, False), (beat, False), (beat, False), (beat, False)] 10)
  rr <- newIORef initialState 
  let app = App {
              appDraw = drawUI,
              appChooseCursor = \_s _locs -> Nothing,
              appHandleEvent = \s' e -> do
                  let (f, s) = handleEvent s' e
                  liftIO $ writeIORef rr s
                  f s,
              appStartEvent = pure,
              appAttrMap = \_s -> attrMap defAttr mempty
             }
  True <- initAudio 100 48000 512
  True <- loaderAvailable "wav"
  clickTrack <- flip sampleFromFile 1.0 =<< clickTrackFile 
  eventChan <- Brick.BChan.newBChan 10
  _stop <- startMetronome clickTrack rr (writeBChan eventChan . Beep)
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  finalState <- customMain initialVty buildVty
                    (Just eventChan) app initialState
  finishAudio
  print finalState

(~>) :: a -> b -> (a, b)
a ~> b = (a, b)

handleEvent :: Metronome -> BrickEvent n1 AppEvent -> (s -> EventM n2 (Next s), Metronome)
handleEvent s e = case e of
   (VtyEvent e2) -> case e2 of
      -- Quit
      EvKey k [m] | k == KChar 'c' && m == MCtrl -> halt ~> s
      EvKey k _ | k == KChar 'q' -> halt ~> s
      -- Metronome modifications 
      EvKey k _ -> continue ~>
        case lookup k actions of
          Nothing -> s
          Just f -> f s
      _ -> continue ~> s
   AppEvent (Beep n) ->
     continue ~> setPlayed n s
   _ -> continue ~> s

actions :: [(Key, Metronome -> Metronome)]
actions = 
  [
    -- Change bpm
    KChar '.' ~> modifyBpm succ
  , KChar ',' ~> modifyBpm pred

  , KChar '=' ~> addBeat
  , KChar '-' ~> removeBeat

    -- Change accents
  , KChar '1' ~> setAccent 0
  , KChar '2' ~> setAccent 1
  , KChar '3' ~> setAccent 2
  , KChar '4' ~> setAccent 3
  , KChar '5' ~> setAccent 4
  , KChar '6' ~> setAccent 5
  , KChar '7' ~> setAccent 6
  , KChar '8' ~> setAccent 7
  , KChar '9' ~> setAccent 8

    -- Change probs
  , KChar 'a' ~> changeProb 0 0.1
  , KChar 'z' ~> changeProb 0 (-0.1)
  , KChar 's' ~> changeProb 1 0.1
  , KChar 'x' ~> changeProb 1 (-0.1)
  , KChar 'd' ~> changeProb 2 0.1
  , KChar 'c' ~> changeProb 2 (-0.1)
  , KChar 'f' ~> changeProb 3 0.1
  , KChar 'v' ~> changeProb 3 (-0.1)
  , KChar 'g' ~> changeProb 4 0.1
  , KChar 'b' ~> changeProb 4 (-0.1)
  , KChar 'h' ~> changeProb 5 0.1
  , KChar 'n' ~> changeProb 5 (-0.1)
  , KChar 'j' ~> changeProb 6 0.1
  , KChar 'm' ~> changeProb 6 (-0.1)
  ]

drawUI :: Metronome -> [Widget ()]
drawUI s = 
  [ 
        border (txt ("BPM: " <> fromString (show (metronomeBpm s))))
    <=> border (renderList renderItem True (metronomeBeats s))
  ]
    where renderItem _s (b, thisClick) = (if thisClick then txt "> " else txt "  ") <+> txt (showBeat b)
          showBeat (Always b) = fromString . show $ b
          showBeat (Sometimes f b) = fromString $ show b <> " ?? " <> printf "%2.2f" f

