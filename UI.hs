{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module UI where

import Brick
  ( App
      ( App,
        appAttrMap,
        appChooseCursor,
        appDraw,
        appHandleEvent,
        appStartEvent
      ),
    AttrName,
    BrickEvent (VtyEvent),
    Widget,
    attrMap,
    clickable,
    customMain,
    handleEventLensed,
    str,
    txt,
    (<+>),
    (<=>),
  )

import Brick.BChan (newBChan, writeBChan)
import Brick.Util (bg)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.List (handleListEvent, listSelectedAttr, renderListWithIndex)
import Control.Lens (view)
import Control.Monad (when)
import Actions

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (newIORef, writeIORef)
import Graphics.Vty
  ( Mode (Mouse),
    Output (setMode, supportsMode),
    defaultConfig,
    mkVty,
    outputIface,
  )
import Graphics.Vty.Attributes (Attr, blue, defAttr)
import Metronome
  ( Metronome (..),
    metronomeBeats,
    metronomeBpm,
  )
import MyLib (startMetronome)
import Paths_metronome_cli (getDataFileName)
import Q (Q (Always, Sometimes))
import Text.Printf (printf)
import qualified SDL
import qualified SDL.Mixer as Mixer
import qualified Digits
import Config

clickTrackFile :: IO FilePath
clickTrackFile = getDataFileName "157-click1.wav"

uiMain :: IO ()
uiMain = do
  initialState <- readConfig
  rr <- newIORef initialState
  digits <- Digits.loadDigits
  let app =
        App
          { appDraw = drawUI digits,
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

  SDL.initialize ([SDL.InitAudio] :: [SDL.InitFlag])
  Mixer.initialize ([Mixer.InitMP3]  :: [ Mixer.InitFlag ])
  Mixer.openAudio Mixer.defaultAudio 256

  clickTrack <- Mixer.load =<< clickTrackFile
  eventChan <- Brick.BChan.newBChan 10
  _stop <- startMetronome clickTrack rr (writeBChan eventChan . Beep)
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  let output = outputIface initialVty
  when (supportsMode output Mouse) $ do
    setMode output Mouse True

  finalState <-
    customMain
      initialVty
      buildVty
      (Just eventChan)
      app
      initialState

  Mixer.free clickTrack
  Mixer.closeAudio
  Mixer.quit
  SDL.quit

  writeConfig finalState

drawUI :: [(Int, String)] -> Metronome Name -> [Widget Name]
drawUI digits s =
  [       bpmDisplay
      <=> (minus5 <+> minus1 <+> plus1 <+> plus5)
      <=> border (renderListWithIndex renderBeat True (view metronomeBeats s))
  ]
  where
    bpmDisplay = border (hCenter $ displayBpm digits (view metronomeBpm s))
    minus5 = border (clickable (MinusBox 5) $ hCenter $ str " -5 ")
    minus1 = border (clickable (MinusBox 1) $ hCenter $ str " - ")
    plus1 = border (clickable (PlusBox 1) $ hCenter $ str " + ")
    plus5 = border (clickable (PlusBox 5) $ hCenter $ str " +5 ")
    renderBeat idx _s (b, thisClick) =
      clickable (Actions.Beat idx) $
        hCenter $
          str " "
            <=> ((if thisClick then txt "> " else txt "") <+> str (showBeat b) <+> (if thisClick then txt " <" else txt ""))
            <=> str " "
    showBeat (Always b) = show b
    showBeat (Sometimes f b) = show b <> " ?? " <> printf "%2.2f" f

displayBpm :: [(Int, String)] -> Int -> Widget n
displayBpm digits =
   foldl1 (<+>) . fmap str . Digits.render digits

-- Styles

styles :: [(AttrName, Attr)]
styles =
  [ (listSelectedAttr, bg blue)
  ]
