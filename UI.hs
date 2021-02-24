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

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Char (digitToInt)
import Data.IORef (newIORef, writeIORef)
import Data.Maybe (fromJust)
import Graphics.Vty
  ( Button (BLeft),
    Event (EvKey),
    Key (KChar),
    Mode (Mouse),
    Modifier (MCtrl),
    Output (setMode, supportsMode),
    defaultConfig,
    mkVty,
    outputIface,
  )
import Graphics.Vty.Attributes (Attr, blue, defAttr)
import Metronome
  ( BeatSound (Accent),
    Metronome (Metronome),
    addBeat,
    beat,
    changeProbSelected,
    metronomeBeats,
    metronomeBpm,
    modifyBpm,
    removeBeat,
    setAccent,
    setPlayed,
    toggleAccentOnSelected,
  )
import MyLib (startMetronome)
import Paths_metronome_cli (getDataFileName)
import Q (Q (Always, Sometimes))
import Sound.ProteaAudio
  ( finishAudio,
    initAudio,
    loaderAvailable,
    sampleFromFile,
  )
import Text.Printf (printf)
import System.Directory (createDirectoryIfMissing, doesFileExist, getUserDocumentsDirectory)
import GHC.Exts (fromList)
import Data.Foldable (toList)
import Data.Functor ((<&>))

newtype AppEvent
  = Beep Int

clickTrackFile :: IO FilePath
clickTrackFile = getDataFileName "157-click1.wav"

loadDigits :: IO [(Int, String)]
loadDigits =
  let loadDigit = \i -> fmap (i,) . readFile =<< getDataFileName ("digits/" <> show i)
   in traverse loadDigit [0 .. 9]

configDirectory :: IO FilePath
configDirectory = getUserDocumentsDirectory <&> (</> ".config/metronome-cli")

configFile :: FilePath
configFile = "settings"

data Conf = Conf {
   _confBpm :: Int,
   _confBeats :: [(Q BeatSound, Bool)]
 } deriving (Eq, Show, Read)

data Name
  = MinusBox Int
  | PlusBox Int
  | Beat Int
  | U
  deriving (Eq, Show, Ord)

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

  digits <- loadDigits

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

  finalState <-
    customMain
      initialVty
      buildVty
      (Just eventChan)
      app
      initialState

  finishAudio
  writeFile (resolvedConfigDir </> configFile) (show $ Conf (view metronomeBpm finalState) (toList $ listElements $ view metronomeBeats finalState))
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
      clickable (UI.Beat idx) $
        hCenter $
          str " "
            <=> ((if thisClick then txt "> " else txt "") <+> str (showBeat b) <+> (if thisClick then txt " <" else txt ""))
            <=> str " "
    showBeat (Always b) = show b
    showBeat (Sometimes f b) = show b <> " ?? " <> printf "%2.2f" f

displayBpm :: [(Int, String)] -> Int -> Widget n
displayBpm digits num =
  let thisDigits = fromJust . flip lookup digits <$> digitsFor num
   in foldl1 (<+>) $ fmap str thisDigits

digitsFor :: Int -> [Int]
digitsFor = map digitToInt . show

-- Styles

styles :: [(AttrName, Attr)]
styles =
  [ (listSelectedAttr, bg blue)
  ]
