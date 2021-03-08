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
    BrickEvent (AppEvent, MouseUp, VtyEvent),
    EventM,
    Next,
    Widget,
    attrMap,
    clickable,
    continue,
    customMain,
    halt,
    handleEventLensed,
    str,
    txt,
    (<+>),
    (<=>),
  )

import System.FilePath
import Brick.BChan (newBChan, writeBChan)
import Brick.Util (bg)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.List (handleListEvent, list, listSelectedAttr, renderListWithIndex, listElements)
import Control.Lens (view)
import Control.Monad (when)
import Actions

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Char (digitToInt)
import Data.IORef (newIORef, writeIORef)
import Data.Maybe (fromJust)
import Graphics.Vty
  ( Mode (Mouse),
    Output (setMode, supportsMode),
    defaultConfig,
    mkVty,
    outputIface,
  )
import Graphics.Vty.Attributes (Attr, blue, defAttr)
import Metronome
  ( BeatSound (Accent),
    Metronome (Metronome),
    beat,
    metronomeBeats,
    metronomeBpm,
  )
import MyLib (startMetronome)
import Paths_metronome_cli (getDataFileName)
import Q (Q (Always, Sometimes))
import Text.Printf (printf)
import System.Directory (createDirectoryIfMissing, doesFileExist, getUserDocumentsDirectory)
import GHC.Exts (fromList)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import qualified SDL
import qualified SDL.Mixer as Mixer
import qualified Digits

clickTrackFile :: IO FilePath
clickTrackFile = getDataFileName "157-click1.wav"

configDirectory :: IO FilePath
configDirectory = getUserDocumentsDirectory <&> (</> ".config/metronome-cli")

configFile :: FilePath
configFile = "settings"

data Conf = Conf {
   _confBpm :: Int,
   _confBeats :: [(Q BeatSound, Bool)]
 } deriving (Eq, Show, Read)

uiMain :: IO ()
uiMain = do
  resolvedConfigDir <- configDirectory
  createDirectoryIfMissing True resolvedConfigDir

  conf <- do
    hasConf <- doesFileExist (resolvedConfigDir </> configFile)
    if hasConf
       then read <$> readFile (resolvedConfigDir </> configFile)
       else pure (Conf 114 [(Always Accent, False), (beat, False), (beat, False), (beat, False)])

  let initialState = Metronome (_confBpm conf) (list U (fromList $ _confBeats conf) 10)
  
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

  writeFile (resolvedConfigDir </> configFile) (show $ Conf (view metronomeBpm finalState) (toList $ listElements $ view metronomeBeats finalState))
  print finalState

drawUI :: [(Int, String)] -> Metronome Name -> [Widget Name]
drawUI digits s =
  [ border (hCenter $ displayBpm digits (view metronomeBpm s))
      <=> ( border (clickable (MinusBox 5) $ hCenter $ str " -5 ")
              <+> border (clickable (MinusBox 1) $ hCenter $ str " - ")
              <+> border (clickable (PlusBox 1) $ hCenter $ str " + ")
              <+> border (clickable (PlusBox 5) $ hCenter $ str " +5 ")
          )
      <=> border (renderListWithIndex renderItem True (view metronomeBeats s))
  ]
  where
    renderItem idx _s (b, thisClick) =
      clickable (Actions.Beat idx) $
        hCenter $
          str " "
            <=> ((if thisClick then txt "> " else txt "") <+> str (showBeat b) <+> (if thisClick then txt " <" else txt ""))
            <=> str " "
    showBeat (Always b) = show b
    showBeat (Sometimes f b) = show b <> " ?? " <> printf "%2.2f" f

displayBpm :: [(Int, String)] -> Int -> Widget n
displayBpm digits num =
  let thisDigits = Digits.render digits num
   in foldl1 (<+>) $ fmap str thisDigits

-- Styles

styles :: [(AttrName, Attr)]
styles =
  [ (listSelectedAttr, bg blue)
  ]
