module Config where

import Q
import Metronome
import System.Directory
import System.FilePath ((</>))
import Data.Functor ((<&>))
import Lens.Micro.Platform (view)
import GHC.Exts (fromList)
import Actions
import Brick.Widgets.List (list, GenericList)
import qualified Data.Vector as V
import Lens.Micro ((^..))

configDirectory :: IO FilePath
configDirectory = getUserDocumentsDirectory <&> (</> ".config/metronome-cli")

configFile :: FilePath
configFile = "settings"

data Conf = Conf {
   _confBpm :: Int,
   _confBeats :: [(Q BeatSound, Bool)]
 } deriving (Eq, Show, Read)

readConfig :: IO (Metronome (GenericList Name V.Vector))
readConfig = do
    resolvedConfigDir <- configDirectory
    hasConf <- doesFileExist (resolvedConfigDir </> configFile)
    conf <- if hasConf
       then read <$> readFile (resolvedConfigDir </> configFile)
       else pure (Conf 114 [(Always Accent, False), (beat, False), (beat, False), (beat, False)])
    let initialState = Metronome (_confBpm conf) (list U (fromList $ _confBeats conf) 10) False
    pure initialState

writeConfig :: Traversable f => Metronome f -> IO ()
writeConfig finalState = do
  resolvedConfigDir <- configDirectory
  createDirectoryIfMissing True resolvedConfigDir
  writeFile (resolvedConfigDir </> configFile) (show $ Conf (view metronomeBpm finalState) (finalState ^.. metronomeBeats . traverse))
