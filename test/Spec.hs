{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import qualified Data.Map as Map
import FRP.Yampa
import qualified FRP.Yampa as Yampa
import FRP.Yampa.OpenAL
import Linear.V3
import Sound.OpenAL.AL.Extensions
import Sound.OpenAL.ALC.Device
import Sound.OpenAL.ALC.Extensions
import System.Timeout

main :: IO ()
main = do
    withALUT \alApp -> do
        -- mapM_ print =<< alExtensions
        -- mdevice <- openDevice Nothing
        -- case mdevice of
        --    Just device -> do
        --        mapM_ print =<< alcExtensions device
        --        undefined

        let tdelay = 16660
        let dt = realToFrac tdelay / 1000000
        handle <- Yampa.reactInit (pure 0) (\_ updated ss -> when updated (runSoundscape alApp ss) >> pure updated) sf
        timeout 7000000 $ forever do
            concurrently
                (threadDelay tdelay)
                (Yampa.react handle (dt, Nothing))
        timeout 5000000 $ forever do
            concurrently
                (threadDelay tdelay)
                (Yampa.react handle (negate dt, Nothing))
        timeout 7000000 $ forever do
            concurrently
                (threadDelay tdelay)
                (Yampa.react handle (dt, Nothing))
        pure ()
  where
    rS hz = source (Sine hz 1)
    sf = proc _ -> do
        -- x <- second (phaser 0.5 30) <<< second (tremolo 0.5 60) <<< arr head <<< mkSrc <<< Yampa.time -< ()
        -- x <- second (phaser 0.2 1) <<< arr head <<< mkSrc <<< Yampa.time -< ()
        -- z <- source (HelloWorld) -< ()
        z <- source (File "test/audio.wav") -< ()
        x <- tremolo 0.2 60 <<< source Silence -< () -- (File "test/audio.wav") -< ()
        t <- Yampa.time -< ()
        case t of
            t
                | t <= (0 * d) -> returnA -< soundscape{soundscapeSources = Map.fromList [(1, z)]}
                | t <= (1 * d) -> returnA -< soundscape{soundscapeSources = Map.fromList [(1, z),(2,x)]}
                | t <= (2 * d) -> returnA -< soundscape{soundscapeSources = Map.fromList [(1, z)]}
                | t <= (3 * d) -> returnA -< soundscape{soundscapeSources = Map.fromList [(1, z),(2,x)]}
                | t <= (4 * d) -> returnA -< soundscape{soundscapeSources = Map.fromList [(1, z)]}
                | t <= (5 * d) -> returnA -< soundscape{soundscapeSources = Map.fromList [(1, z),(2,x)]}
                | t <= (6 * d) -> returnA -< soundscape{soundscapeSources = Map.fromList [(1, z)]}
                | t <= (7 * d) -> returnA -< soundscape{soundscapeSources = Map.fromList [(1, z),(2,x)]}
                | t <= (8 * d) -> returnA -< soundscape{soundscapeSources = Map.fromList [(1, z)]}
                | otherwise -> returnA -< soundscape{soundscapeSources = Map.fromList [(1, z),(2,x)]}
      where
        d = 0.3
