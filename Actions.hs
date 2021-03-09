module Actions where

import Metronome
    ( addBeat,
      changeProbSelected,
      modifyBpm,
      removeBeat,
      setAccent,
      setPlayed,
      toggleAccentOnSelected,
      Metronome, setShouldQuit, metronomeBeats )
import Graphics.Vty
  ( Button (BLeft),
    Event (EvKey),
    Key (KChar),
    Modifier (MCtrl)
  )
import Brick (EventM, BrickEvent(AppEvent, MouseUp, VtyEvent))
import Brick.Types (handleEventLensed)
import Brick.Widgets.List (handleListEvent)

newtype AppEvent
  = Beep Int

data Name
  = MinusBox Int
  | PlusBox Int
  | Beat Int
  | U
  deriving (Eq, Show, Ord)

handleEvent :: Ord n => Metronome n -> BrickEvent Name AppEvent -> EventM n (Metronome n)
handleEvent s e = case e of
  MouseUp (MinusBox n) (Just BLeft) _ -> pure $ modifyBpm (\x -> x - n) s
  MouseUp (PlusBox n) (Just BLeft) _ -> pure $ modifyBpm (+ n) s
  MouseUp (Actions.Beat n) (Just BLeft) _ -> pure $ setAccent n s
  VtyEvent e' ->
    do s'' <- handleEventLensed s metronomeBeats handleListEvent e'
       case e' of
        -- Quit
        EvKey k [m] | k == KChar 'c' && m == MCtrl -> pure $ setShouldQuit s''
        EvKey k _ | k == KChar 'q' -> pure $ setShouldQuit s''
        -- Metronome modifications
        EvKey k _ -> pure $ 
          case lookup k actions of
              Nothing -> s''
              Just f -> f s''
        _ -> pure s''
  AppEvent (Beep n) -> pure $
    setPlayed n s
  _ -> pure s

actions :: [(Key, Metronome n -> Metronome n)]
actions =
  [ -- Change bpm
    KChar '.' ~> modifyBpm succ,
    KChar ',' ~> modifyBpm pred,
    KChar '>' ~> modifyBpm (\x -> x + 5),
    KChar '<' ~> modifyBpm (\x -> x - 5),
    KChar '=' ~> addBeat,
    KChar '-' ~> removeBeat,
    KChar 'x' ~> toggleAccentOnSelected,
    KChar '[' ~> changeProbSelected (-0.1),
    KChar ']' ~> changeProbSelected 0.1
  ]

(~>) :: a -> b -> (a, b)
a ~> b = (a, b)
