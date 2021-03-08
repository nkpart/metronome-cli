module Actions where

import Metronome
    ( addBeat,
      changeProbSelected,
      modifyBpm,
      removeBeat,
      setAccent,
      setPlayed,
      toggleAccentOnSelected,
      Metronome )
import Brick
    ( EventM,
      continue,
      halt,
      BrickEvent(AppEvent, MouseUp, VtyEvent),
      Next )
import Graphics.Vty
  ( Button (BLeft),
    Event (EvKey),
    Key (KChar),
    Modifier (MCtrl)
  )

newtype AppEvent
  = Beep Int

data Name
  = MinusBox Int
  | PlusBox Int
  | Beat Int
  | U
  deriving (Eq, Show, Ord)


handleEvent :: Metronome n1 -> BrickEvent Name AppEvent -> (s -> EventM n2 (Next s), Metronome n1)
handleEvent s e = case e of
  MouseUp (MinusBox n) (Just BLeft) _ -> continue ~> modifyBpm (\x -> x - n) s
  MouseUp (PlusBox n) (Just BLeft) _ -> continue ~> modifyBpm (+ n) s
  MouseUp (Actions.Beat n) (Just BLeft) _ -> continue ~> setAccent n s
  VtyEvent e2 -> case e2 of
    -- Quit
    EvKey k [m] | k == KChar 'c' && m == MCtrl -> halt ~> s
    EvKey k _ | k == KChar 'q' -> halt ~> s
    -- Metronome modifications
    EvKey k _ ->
      continue
        ~> case lookup k actions of
          Nothing -> s
          Just f -> f s
    _ -> continue ~> s
  AppEvent (Beep n) ->
    continue ~> setPlayed n s
  _ -> continue ~> s

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
