module Config where

import Q
import Metronome
import System.Directory
import System.FilePath ((</>))
import Data.Functor ((<&>))
import Lens.Micro.Platform (view)
import GHC.Exts (fromList)
import Actions
import Brick.Widgets.List (list)
import Lens.Micro ((^..))
import Data.Functor.Compose (Compose(Compose))

configDirectory :: IO FilePath
configDirectory = getUserDocumentsDirectory <&> (</> ".config/metronome-cli")

configFile :: FilePath
configFile = "settings"

data Conf = Conf {
   _confBpm :: Int,
   _confBeats :: [(Bool, Q BeatSound)]
 } deriving (Eq, Show, Read)

readConfig :: IO (UIMetronome Name)
readConfig = do
    resolvedConfigDir <- configDirectory
    hasConf <- doesFileExist (resolvedConfigDir </> configFile)
    conf <- if hasConf
       then read <$> readFile (resolvedConfigDir </> configFile)
       else pure (Conf 114 [(False, Always Accent), (False, beat), (False, beat), (False, beat)])
    let initialState = Metronome (BPM $ _confBpm conf) (Compose $ list U (fromList $ _confBeats conf) 10) False
    pure initialState

writeConfig :: UIMetronome Name -> IO ()
writeConfig finalState = do
  resolvedConfigDir <- configDirectory
  createDirectoryIfMissing True resolvedConfigDir
  writeFile (resolvedConfigDir </> configFile) (show $ Conf (unBPM $ view metronomeBpm finalState) (finalState ^.. metronomeBeats . compose . traverse))
