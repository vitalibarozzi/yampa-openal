{-# LANGUAGE Arrows #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import Control.Concurrent
import Control.Monad
import qualified FRP.Yampa as Yampa
import FRP.Yampa.OpenAL
import qualified Sound.ALUT as ALUT
import qualified Sound.OpenAL as AL
import System.Timeout
import System.CPUTime
import Data.IORef


-- TODO add more specific, unit tests, maybe? even though to know its working 
-- you still gonna need a human there. but still

main :: IO ()
main = do
    withSoundstage ((), Yampa.NoEvent) sf \handle -> do
        helloBuffer <- ALUT.createBuffer ALUT.HelloWorld
        let queue = [helloBuffer]
        let tdelay = 16_660
        --let dt = realToFrac tdelay / 1_000_000
        dtRef <- newIORef 0
        _ <- Yampa.react handle (0, Just ((), Yampa.Event ([source "some-source" queue] <>)))
        _ <- timeout 3_000_000 $ forever do
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
    return (realToFrac (t1 - t0) / 10000)
