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
    let tdelay = 16_660 
    dtRef <- newIORef 0 
    let mySoundstage = soundstage_ mempty 1 AL.InverseDistance 0 0 1
    withSoundstage (0, Yampa.NoEvent) mySoundstage \handle -> do
        hello <- ALUT.createBuffer (ALUT.Sine 440 1 4)
        other <- ALUT.createBuffer (ALUT.Sine 340 1 4)
        fulll <- ALUT.createBuffer (ALUT.Sine 240 1 4)
        sm <- ALUT.createBuffer (ALUT.Sine 240 1 0.5)
        silence <- ALUT.createBuffer (ALUT.Sine 10 1 0.25)
        audio <- ALUT.createBuffer (ALUT.File "./test/audio.wav")
        ogsrc <- AL.genObjectName
        --hlsrc <- AL.genObjectName
        let getBuffs = \case 0 -> [sm,silence,sm,silence]
                             1 -> [other]
        loadSources
            handle
            -- TODO missing test for buffers streaming api
            [ mkSrc ogsrc [hello] \s -> s 
                  -- & streaming getBuffs
                  -- & setQueue [hello,fulll]
                  -- & withPitch (constant 1.45)
                  -- & foo (setQueue [other])
            -- , mkSrc hlsrc [audio] id
            ] 
        t0 <- getCPUTime
        void $ timeout 10_000_000 $ forever do 
            dt <- tack dtRef
            t1 <- getCPUTime
            let n = if abs (t1 - t0) >= 40_000_000_000 then 1 else 0
            _ <- tick dtRef
            _ <- Yampa.react handle (dt / 1_000_000, Just (n, NoEvent))
            when (dt < tdelay) (threadDelay $ round (tdelay / 1.5))
  where
    loadSources :: ReactHandle (Int, Event (Map AL.Source (SF a Source) -> Map AL.Source (SF a Source))) b -> [(AL.Source, SF a Source)] -> IO ()
    loadSources handle sources =
        forM_ sources \(sid, src) -> 
            Yampa.react handle (0, Just (0, Yampa.Event (Map.insert sid src))) -- insert the sources in the soundstage without passing time


mkSrc src queue k = 
    (src, source src queue & k)

    --returnA -< s1 { 
--afterSec x sfk sf1 = proc foo -> do
--    x <- Yampa.switch (identity &&& (Yampa.delayEvent 3 <<< now ())) (\c -> sf1) -< (foo, NoEvent)
--    returnA -< undefined x
    -- TODO this is not working because we need a switch?
    --t <- Yampa.time -< foo
    --if t >= x
    --    then sfk sf1 -< foo
    --    else sf1 -< foo

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

            -- & withGain (tremolo (-1.1) 10)
            -- & setState AL.Paused
            -- & withPitch (constant $ -0.2)
            -- & withPitch (vibrato 0.1 0.5)
            -- & withGain (tremolo 0.2 20)
            -- , source hlsrc audio
            --      & withPitch (vibrato 0.3 10)
