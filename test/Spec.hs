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
import Data.Function

main :: IO ()
main = do
    let tdelay = 16_660
    dtRef <- newIORef 0
    let mySoundstage = soundstage_ 1 AL.InverseDistance 0 0 1
    withSoundstage (False, Yampa.NoEvent) mySoundstage \handle -> do
        hello <- ALUT.createBuffer ALUT.HelloWorld
        audio <- ALUT.createBuffer (ALUT.File "./test/audio.wav")
        ogsrc <- AL.genObjectName
        --hlsrc <- AL.genObjectName
        loadSources handle
              -- TODO missing test for multiple buffers AND buffer streaming api
              [ source ogsrc audio
                    & setOffset 80
                    -- & withGain (tremolo (-1.1) 10)
                    -- & setState AL.Paused
                    -- & withPitch (constant $ -0.2)
                    -- & withPitch (vibrato 0.1 0.5)
                    -- & withGain (tremolo 0.2 20)
                    --, source hlsrc audio
                    --      & withPitch (vibrato 0.3 10) 
              ]
        void $ timeout 10_000_000 $ forever do
            dt <- tack dtRef
            _ <- tick dtRef
            _ <- Yampa.react handle (dt / 1_000_000, Nothing)
            when (dt < tdelay) (threadDelay $ round (tdelay / 1.5))
  where
    loadSources handle sources = forM_ sources \src -> Yampa.react handle (0, Just (False, Yampa.Event (src :))) -- insert the sources in the soundstage without passing time


tick :: IORef Integer -> IO ()
tick ref = do
    getCPUTime >>= writeIORef ref


tack :: IORef Integer -> IO Double
tack ref = do
    t1 <- getCPUTime
    t0 <- readIORef ref
    return (realToFrac (t1 - t0) / 10_000)
