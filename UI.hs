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
    Widget,
    attrMap,
    clickable,
    customMain,
    str,
    txt,
    (<+>),
    (<=>),
  )

import Brick.BChan (newBChan, writeBChan)
import Brick.Util (bg)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.List (listSelectedAttr, renderListWithIndex, GenericList)
import Lens.Micro.Platform (view)
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
import MyLib (startMetronome, initPlayback, quitPlayback)
import Q (Q (Always, Sometimes))
import Text.Printf (printf)
import qualified Digits
import Config
import Brick.Main (continue, halt)
import qualified Data.Vector as V

uiMain :: IO ()
uiMain = do

  playback <- initPlayback

  initialState <- readConfig
  rr <- newIORef initialState
  digits <- Digits.loadDigits
  let app =
        App
          { appDraw = drawUI digits,
            appChooseCursor = \_s _locs -> Nothing,
            appHandleEvent = \s' e -> do
              s <- handleEvent s' e
              liftIO (writeIORef rr s)
              if _metronomeShouldQuit s
                 then halt s
                 else continue s
          ,
            appStartEvent = pure,
            appAttrMap = \_s -> attrMap defAttr styles
          }

  eventChan <- Brick.BChan.newBChan 10

  _stop <- startMetronome playback rr (writeBChan eventChan . Beep . snd)

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

  writeConfig finalState
  quitPlayback playback


drawUI :: [(Int, String)] -> Metronome (GenericList Name V.Vector) -> [Widget Name]
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
      clickable (ClickBeat idx) $
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
