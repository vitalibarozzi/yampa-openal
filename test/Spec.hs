{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
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
import qualified Sound.ALUT as ALUT
import qualified Sound.OpenAL as AL
import System.CPUTime
import System.Timeout
import Linear.V3

data State = State
    { foo :: Int
    , bar :: Event (Map AL.Source (SF State Source) -> Map AL.Source (SF State Source))
    }

-----------------------------------------------------------
initialState :: State
initialState = State 0 NoEvent

isTriggered = (== 1) . foo

-----------------------------------------------------------
main :: IO ()
main = do
    dtRef <- newIORef 0
    let
    let mySoundstage = soundstage_ mempty 1 AL.InverseDistance 0 0 1 (\State{..} -> bar)
    withSoundstage initialState mySoundstage \handle -> do
        hello <- ALUT.createBuffer (ALUT.Sine 440 1 10.2)
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
        srcXX <- AL.genObjectName
        let getBuffs = \case
                State 0 _ -> []
                State 1 _ -> [sm, silence, sm1, silence, sm2, silence]
                _________ -> [sm, sm1, sm2, sm3, sm4]
        loadSources
            handle
            -- TODO test adding and removing sources dynamically in the middle of the program
            [ mkSrc
                ogsrc
                []
                --[sm,sm,sm1,sm1,sm1,sm2,sm2,sm2,sm3,sm3,sm3,sm2,sm2,sm2,sm1]
                \s ->s
                     & setPosition (V3 (-10) 0 0)
                     & setVelocity (V3 (-5.5) (-5) 0)
                     -- & edgeStop isTriggered 
            , mkSrc
                srcXX
                [hello]
                --[sm,sm,sm1,sm1,sm1,sm2,sm2,sm2,sm3,sm3,sm3,sm2,sm2,sm2,sm1]
                     -- TODO and then we want to play it only when we send it an event using the State
                \s ->s
                     -- & setState AL.Stopped 
                     & setPosition (V3 (-20) 0 5)
                     & setVelocity (V3 30 0 0)
                     -- & edgePlay isTriggered 
                     & withPitch (constant 1.8)
            ]
        t0 <- getCPUTime
        void $ timeout 10_000_000 $ forever do
            dt <- tack dtRef
            t1 <- getCPUTime
            let crazyNumber = 89_480_000
            let n = if abs (t1 - t0) >= 1562628940000 then 1 else 0
            _ <- tick dtRef
            _ <- Yampa.react handle (dt / crazyNumber, Just (State n NoEvent))
            pure () -- when (dt < tdelay) (threadDelay $ round (tdelay / 1.5))
  where
    loadSources :: ReactHandle State b -> [(AL.Source, SF State Source)] -> IO ()
    loadSources handle sources =
        forM_ sources \(sid, src) ->
            Yampa.react handle (0, Just (State 2 (Yampa.Event (Map.insert sid src)))) -- insert the sources in the soundstage without passing time

-----------------------------------------------------------
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
