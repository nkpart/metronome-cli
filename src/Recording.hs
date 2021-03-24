{-# LANGUAGE GADTs #-}
module Recording where

import qualified SDL
import qualified SDL.Audio as SDLA
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as MV
import Data.Foldable (toList, for_)
import Data.IORef
import Sound.File.Sndfile as SF
import Sound.File.Sndfile.Buffer.Vector 
import Control.Monad (unless)
import System.IO
import qualified SDL.Mixer as Mixer

configSampleRate :: Num a => a
configSampleRate = 48000

configCaptureSeconds :: Int
configCaptureSeconds = 5

configRingMaxSize :: Int
configRingMaxSize = 15

recording :: IO ()
recording =
  do
    SDL.initialize [SDL.InitAudio]
    Mixer.initialize ([Mixer.InitMP3]  :: [ Mixer.InitFlag ])
    Mixer.openAudio Mixer.defaultAudio 256
    Just (recDevice:_) <- fmap toList <$> SDLA.getAudioDeviceNames SDLA.ForCapture

    buffersRef <- newIORef []

    let spec = SDLA.OpenDeviceSpec {
                    SDLA.openDeviceFreq = SDLA.Mandate configSampleRate,
                    SDLA.openDeviceFormat = SDLA.Mandate SDLA.FloatingLEAudio,
                    SDLA.openDeviceChannels = SDLA.Mandate SDLA.Mono,
                    SDLA.openDeviceSamples = 4096,
                    SDLA.openDeviceUsage = SDLA.ForCapture,
                    SDLA.openDeviceName = Just recDevice,
                    SDLA.openDeviceCallback = collectBuffers buffersRef
                                   }

    (recDeviceId, _) <- SDLA.openAudioDevice spec
    SDLA.setAudioDevicePlaybackState recDeviceId SDLA.Play

    putStrLn "Waiting to capture some stuff"

    hSetBuffering stdin NoBuffering

    let go n = 
          do 
            ch <- getChar
            case ch of
              ' ' -> do
                 bufs <- readIORef buffersRef
                 let forUse = lastN (configSampleRate * configCaptureSeconds) bufs
                 putStrLn "\nCaptured"
                 let fp = "out-" <> show n <> ".wav"
                 writeBuffersToWav fp forUse
                 justDid <- Mixer.load fp
                 Mixer.play justDid 
                 -- This seems a bad way to do this.
                 Mixer.whenChannelFinished $ \_ -> Mixer.free justDid
                 go (n + 1)
              'q' -> pure ()
              _ -> go n

    go (0 :: Int) 

    SDLA.closeAudioDevice recDeviceId
    Mixer.closeAudio
    Mixer.quit
    SDL.quit
    pure ()

lastN :: (Foldable t, MV.Storable a) => Int -> t (VS.Vector a) -> [VS.Vector a]
lastN amount bufs = do
     let (xs, _) = foldl (\(acc, sz) b ->
               if sz > amount
                  then 
                    (acc, sz)
                  else
                    (b:acc, sz + VS.length b)
                ) ([], 0) bufs
      in reverse xs

writeBuffersToWav :: FilePath -> [VS.Vector Float] -> IO ()
writeBuffersToWav fp bufsMR = do
    let info = SF.Info { frames  = -1, samplerate = configSampleRate, channels = 1, format = Format HeaderFormatWav SampleFormatFloat EndianLittle, sections = 1, seekable = True }
    unless (SF.checkFormat info) $ fail $ "Bad info for hsndfile, check: " <> show info
    h <- SF.openFile fp SF.WriteMode info
    for_ (reverse bufsMR) $ hPutBuffer h . toBuffer
    SF.hClose h

ringBufferSamples :: Int
ringBufferSamples =
  ourSampleRate * numberOfSecondsInRing
    where ourSampleRate = configSampleRate
          numberOfSecondsInRing = configRingMaxSize

collectBuffers :: IORef [VS.Vector Float] -> SDLA.AudioFormat actualSampleType -> MV.IOVector actualSampleType -> IO ()
collectBuffers ref SDLA.FloatingLEAudio vec = 
  do this <- VS.freeze $ MV.unsafeCast vec
     bufs <- readIORef ref
     let xs = lastN ringBufferSamples (this: bufs)
     writeIORef ref xs
collectBuffers _ af _ = fail $ "Why are we here" <> show af
