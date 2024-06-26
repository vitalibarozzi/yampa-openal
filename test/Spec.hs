{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

import Control.Concurrent
import Control.Monad
import qualified FRP.Yampa as Yampa
import FRP.Yampa.OpenAL
import qualified Sound.ALUT as ALUT
import qualified Sound.OpenAL as AL
import System.Timeout
import System.CPUTime
import Data.IORef
import FRP.Yampa
import FRP.Yampa.OpenAL.Types
import FRP.Yampa.OpenAL.Effects
import FRP.Yampa.OpenAL.Source

-- TODO add more specific, unit tests, maybe? even though to know its working 
-- you still gonna need a human there. but still

main :: IO ()
main = do
    withSoundstage (False, Yampa.NoEvent) sf \handle -> do
        helloBuffer <- ALUT.createBuffer ALUT.HelloWorld
        whiteBuffer <- ALUT.createBuffer (ALUT.WhiteNoise 1)
        audioBuffer <- ALUT.createBuffer (ALUT.File "./test/audio.wav")
        let queue1 = audioBuffer
        let queue2 = whiteBuffer
        let tdelay = 16_660
        dtRef <- newIORef 0
        let sources = 
              [ --source "some-source"  queue1
              --, setPitch (constant 1.3) (source "other-source" queue1)
                --setGain (average [tremolo 1.0 3]) (source "other-source" queue1)
                withPitch (vibrato 0.3 10) (source "other-source" queue1)

              --, setPitch (constant 0.6) (source "idk-source" queue1)
              --, setPitch (constant 0.3) (source "zidk-source" queue1)
              --, setPitch (constant 0.2) (source "widk-source" queue1)
              --, setPitch (constant 0.1) (source "ridk-source" queue1)
              ]
        -- TODO i want to check that the offset of sources with different pitchs but the same buffer is different
        forM_ sources \src -> do
            Yampa.react handle (0, Just (False, Yampa.Event (src :)))
        _ <- timeout 9_000_000 $ forever do
            dt <- tack dtRef
            _ <- tick dtRef
            _ <- Yampa.react handle (dt / 1_000_000, Nothing)
            when (dt < tdelay) (threadDelay $ round (tdelay / 1.5))
        pure ()
  where 
    sf = soundstage_ 1 AL.InverseDistance 0 0 1

tick :: IORef Integer -> IO ()
tick ref = getCPUTime >>= writeIORef ref

tack :: IORef Integer -> IO Double
tack ref = do
    t1 <- getCPUTime
    t0 <- readIORef ref
    return (realToFrac (t1 - t0) / 10_000)
