{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent
import Control.Monad
import Data.Function
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import FRP.Yampa
import qualified FRP.Yampa as Yampa
import FRP.Yampa.OpenAL
import FRP.Yampa.OpenAL.Source
import FRP.Yampa.OpenAL.Types
import qualified Sound.ALUT as ALUT
import qualified Sound.OpenAL as AL
import System.CPUTime
import System.Timeout

main :: IO ()
main = do
    let tdelay = 16_660
    dtRef <- newIORef 0
    let mySoundstage = soundstage_ mempty 1 AL.InverseDistance 0 0 1
    withSoundstage (False, Yampa.NoEvent) mySoundstage \handle -> do
        hello <- ALUT.createBuffer ALUT.HelloWorld
        audio <- ALUT.createBuffer (ALUT.File "./test/audio.wav")
        ogsrc <- AL.genObjectName
        -- hlsrc <- AL.genObjectName
        loadSources
            handle
            -- TODO missing test for multiple buffers AND buffer streaming api
            [ (ogsrc, (source ogsrc (take 100 $ repeat hello))) -- [hello,hello]))--  & setOffset 80)
            -- & withGain (tremolo (-1.1) 10)
            -- & setState AL.Paused
            -- & withPitch (constant $ -0.2)
            -- & withPitch (vibrato 0.1 0.5)
            -- & withGain (tremolo 0.2 20)
            -- , source hlsrc audio
            --      & withPitch (vibrato 0.3 10)
            ]
        void $ timeout 10_000_000 $ forever do
            dt <- tack dtRef
            _ <- tick dtRef
            _ <- Yampa.react handle (dt / 1_000_000, Nothing)
            when (dt < tdelay) (threadDelay $ round (tdelay / 1.5))
  where
    loadSources :: ReactHandle (Bool, Event (Map AL.Source (SF a Source) -> Map AL.Source (SF a Source))) b -> [(AL.Source, SF a Source)] -> IO ()
    loadSources handle sources =
        forM_ sources \(sid, src) -> Yampa.react handle (0, Just (False, Yampa.Event (Map.insert sid src))) -- insert the sources in the soundstage without passing time

tick :: IORef Integer -> IO ()
tick ref = do
    getCPUTime >>= writeIORef ref

tack :: IORef Integer -> IO Double
tack ref = do
    t1 <- getCPUTime
    t0 <- readIORef ref
    return (realToFrac (t1 - t0) / 10_000)
