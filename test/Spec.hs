{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import FRP.Yampa
import qualified FRP.Yampa as Yampa
import FRP.Yampa.OpenAL
import qualified Sound.ALUT as ALUT
import qualified Sound.OpenAL as AL
import System.CPUTime
import System.Timeout
import Data.Function

-----------------------------------------------------------
main :: IO ()
main = do
    let tdelay = 1
    dtRef <- newIORef 0 
    let mySoundstage = soundstage_ mempty 1 AL.InverseDistance 0 0 1
    withSoundstage (0, Yampa.NoEvent) mySoundstage \handle -> do
        hello <- ALUT.createBuffer (ALUT.Sine 440 1 2.2)
        other <- ALUT.createBuffer (ALUT.Sine 340 1 2.3)
        fulll <- ALUT.createBuffer (ALUT.Sine 240 1 2.4)
        sm <- ALUT.createBuffer (ALUT.Sine 240 1 0.3)
        sm1 <- ALUT.createBuffer (ALUT.Sine 340 1 0.3)
        sm2 <- ALUT.createBuffer (ALUT.Sine 440 1 0.3)
        sm3 <- ALUT.createBuffer (ALUT.Sine 540 1 0.3)
        sm4 <- ALUT.createBuffer (ALUT.Sine 640 1 0.3)
        silence <- ALUT.createBuffer (ALUT.Sine 10 1 0.15)
        audio <- ALUT.createBuffer (ALUT.File "./test/audio.wav")
        ogsrc <- AL.genObjectName
        let getBuffs = \case 0 -> []
                             1 -> [sm,silence,sm1,silence,sm2,silence]
                             _ -> [sm,sm1,sm2,sm3,sm4]
        loadSources
            handle
            [ mkSrc ogsrc [audio] 
                \s ->
            --[ mkSrc ogsrc [sm,sm1,sm2,sm3,sm4] \s -> 
                --s --  \s -> s 
                -- & setOffset 0.8
                  s & streaming getBuffs
                  -- & setQueue [hello,fulll]
                  -- & withPitch (constant (-0.15))
            ] 
        t0 <- getCPUTime
        void $ timeout 3_000_000 $ forever do 
            dt <- tack dtRef
            t1 <- getCPUTime
            let n = if abs (t1 - t0) <= 30_000_000_000 
                        then 0
                        else if abs (t1 - t0) >= 41_000_100_000 && abs (t1 - t0) <= 80_000_000_000 then 1 else 2
            let neg = if abs (t1 - t0) >= 45_020_000_000  
                         then 1 -- (-1)
                         else 1
            _ <- tick dtRef
            _ <- Yampa.react handle (neg * (dt / 89_480_000), Just (n, NoEvent))
            pure () -- when (dt < tdelay) (threadDelay $ round (tdelay / 1.5))
  where
    loadSources :: ReactHandle (Int, Event (Map AL.Source (SF a Source) -> Map AL.Source (SF a Source))) b -> [(AL.Source, SF a Source)] -> IO ()
    loadSources handle sources =
        forM_ sources \(sid, src) -> 
            Yampa.react handle (0, Just (0, Yampa.Event (Map.insert sid src))) -- insert the sources in the soundstage without passing time


mkSrc src queue k = 
    (src, source src queue & k)

-----------------------------------------------------------
tick :: IORef Integer -> IO ()
tick ref = do
    getCPUTime >>= writeIORef ref

-----------------------------------------------------------
tack :: IORef Integer -> IO Double
tack ref = do
    t1 <- getCPUTime
    t0 <- readIORef ref
    return (realToFrac (t1 - t0) / 10_000)
