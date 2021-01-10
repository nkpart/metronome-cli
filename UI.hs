{-# language OverloadedStrings, OverloadedLists, TupleSections #-}
module UI where

import Brick
    (clickable, AttrName, str, handleEventLensed, (<+>), (<=>), customMain,  attrMap,
      continue,
      halt,
      txt,
      App(App, appDraw, appChooseCursor, appHandleEvent, appStartEvent,
          appAttrMap),
      EventM,
      Widget,
      BrickEvent(MouseUp, MouseDown, AppEvent, VtyEvent), Next )
import Graphics.Vty
    (Button(BLeft), Output(supportsMode, setMode), Mode(Mouse), defaultConfig, mkVty, outputIface,   Event(EvKey), Key(KChar), Modifier(MCtrl) )
import Sound.ProteaAudio
    ( finishAudio, initAudio, loaderAvailable, sampleFromFile ) 
import Data.IORef (writeIORef, newIORef)
import Brick.BChan ( newBChan, writeBChan )
import Brick.Widgets.List (renderListWithIndex, listSelectedAttr, handleListEvent, renderList, list)
import Q (Q(Sometimes, Always))
import Paths_metronome_cli ( getDataFileName )
import Text.Printf ( printf )
import Brick.Widgets.Border (border)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Metronome
import Control.Lens (view)
import MyLib (startMetronome)
import Graphics.Vty.Attributes
import Brick.Util
import Data.Maybe (fromJust)
import Data.Char (digitToInt)
import Brick.Widgets.Center (hCenter)
import Control.Monad (when)

newtype AppEvent = 
  Beep Int

clickTrackFile :: IO FilePath
clickTrackFile = getDataFileName "157-click1.wav"

loadDigits :: IO [(Int, String)]
loadDigits =
  let loadDigit = \i -> fmap (i, ) . readFile =<< getDataFileName ("digits/" <> show i)
   in traverse loadDigit [0..9]

data Name = 
      MinusBox Int | PlusBox Int | Beat Int | U deriving (Eq, Show, Ord)

uiMain :: IO ()
uiMain = do
  let initialState = Metronome 114 (list U [(Always Accent, False), (beat, False), (beat, False), (beat, False)] 10)
  rr <- newIORef initialState 

  digits <- loadDigits

  let app = App {
              appDraw = drawUI digits,
              appChooseCursor = \_s _locs -> Nothing,
              appHandleEvent = \s' e -> do
                  s'' <-
                    case e of 
                      (VtyEvent e') -> handleEventLensed s' metronomeBeats handleListEvent e'
                      _ -> pure s'
                  let (f, s) = handleEvent s'' e
                  liftIO (writeIORef rr s)
                  f s,
              appStartEvent = pure,
              appAttrMap = \_s -> attrMap defAttr styles
             }
  True <- initAudio 100 48000 512
  True <- loaderAvailable "wav"

  clickTrack <- flip sampleFromFile 1.0 =<< clickTrackFile 
  eventChan <- Brick.BChan.newBChan 10
  _stop <- startMetronome clickTrack rr (writeBChan eventChan . Beep)
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  let output = outputIface initialVty
  when (supportsMode output Mouse) $ do
    setMode output Mouse True

  finalState <- customMain initialVty buildVty
                    (Just eventChan) app initialState
  finishAudio
  print finalState

(~>) :: a -> b -> (a, b)
a ~> b = (a, b)

handleEvent :: Metronome n1 -> BrickEvent Name AppEvent -> (s -> EventM n2 (Next s), Metronome n1)
handleEvent s e = case e of
   MouseUp (MinusBox n) (Just BLeft) _ -> continue ~> modifyBpm (\x -> x - n) s
   MouseUp (PlusBox n) (Just BLeft) _ -> continue ~> modifyBpm (+ n) s
   MouseUp (UI.Beat n) (Just BLeft) _ -> continue ~> setAccent n s
   VtyEvent e2 -> case e2 of

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

actions :: [(Key, Metronome n -> Metronome n)]
actions = 
  [
    -- Change bpm
    KChar '.' ~> modifyBpm succ
  , KChar ',' ~> modifyBpm pred
  , KChar '>' ~> modifyBpm (\x -> x + 5)
  , KChar '<' ~> modifyBpm (\x -> x - 5)
  , KChar '=' ~> addBeat
  , KChar '-' ~> removeBeat
  , KChar 'x' ~> toggleAccentOnSelected
  , KChar '[' ~> changeProbSelected (-0.1)
  , KChar ']' ~> changeProbSelected 0.1
  ]

drawUI :: [(Int, String)] -> Metronome Name -> [Widget Name]
drawUI digits s = 
  [ 
        border (hCenter $ displayBpm digits (view metronomeBpm s))
    <=> (
       border (clickable (MinusBox 5) $ hCenter $ str " -5 ") 
       <+> 
       border (clickable (MinusBox 1) $ hCenter $ str " - ")
       <+> 
       border (clickable (PlusBox 1) $ hCenter $ str " + ")
       <+> 
       border (clickable (PlusBox 5) $ hCenter $ str " +5 ")
        )
    <=> border (renderListWithIndex renderItem True (view metronomeBeats s))
  ]
    where renderItem idx _s (b, thisClick) = clickable (UI.Beat idx) $ hCenter $
            str " " <=>
            -- ((if thisClick then txt "> " else txt "  ") <+> padRight Max (str (showBeat b))) <=>
            ((if thisClick then txt "> " else txt "") <+> str (showBeat b) <+> (if thisClick then txt " <" else txt "")) <=>
            str " " 
          showBeat (Always b) = show b
          showBeat (Sometimes f b) = show b <> " ?? " <> printf "%2.2f" f

displayBpm :: [(Int, String)] -> Int -> Widget n
displayBpm digits num =
  let thisDigits = fromJust . flip lookup digits <$> digs num
   in foldl1 (<+>) $ fmap str thisDigits

digs :: Int -> [Int]
digs = map digitToInt . show

-- Styles

styles :: [(AttrName, Attr)]
styles = [ 
    (listSelectedAttr, bg blue)
  ]
