module Actions (Name(..), handleEvent, AppEvent(..)) where

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
  | ClickBeat Int
  | U
  deriving (Eq, Show, Ord)

handleEvent :: Ord n => Metronome n -> BrickEvent Name AppEvent -> EventM n (Metronome n)
handleEvent s e = case e of
  MouseUp (MinusBox n) (Just BLeft) _ -> pure $ modifyBpm (\x -> x - n) s
  MouseUp (PlusBox n) (Just BLeft) _ -> pure $ modifyBpm (+ n) s
  MouseUp (ClickBeat n) (Just BLeft) _ -> pure $ setAccent n s
  VtyEvent e' ->
    do -- Process list keys
       s'' <- handleEventLensed s metronomeBeats handleListEvent e'
       -- Process other keys 
       pure $ case e' of
        -- Quit
        EvKey (KChar 'c') [MCtrl] -> setShouldQuit s''
        EvKey (KChar 'q') _ -> setShouldQuit s''
        -- Metronome modifications
        EvKey (KChar '.') _ -> modifyBpm succ s''
        EvKey (KChar ',') _ -> modifyBpm pred s''
        EvKey (KChar '>') _ -> modifyBpm (\x -> x + 5) s''
        EvKey (KChar '<') _ -> modifyBpm (\x -> x - 5) s''
        EvKey (KChar '=') _ -> addBeat s''
        EvKey (KChar '-') _ -> removeBeat s''
        EvKey (KChar 'x') _ -> toggleAccentOnSelected s''
        EvKey (KChar '[') _ -> changeProbSelected (-0.1) s''
        EvKey (KChar ']') _ -> changeProbSelected 0.1 s''
        _ -> s''
  AppEvent (Beep n) -> pure $
    setPlayed n s
  _ -> pure s
