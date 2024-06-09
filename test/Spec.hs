{-# LANGUAGE Arrows #-}
{-# LANGUAGE NumericUnderscores #-}
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
import Data.IORef

main :: IO ()
main = do
    undefined
{-
    withAL 6400000 \alApp -> do
        -- mapM_ print =<< alExtensions
        -- mdevice <- openDevice Nothing
        -- case mdevice of
        --    Just device -> do
        --        mapM_ print =<< alcExtensions device
        --        undefined

        let tdelay = 16_660
        let dt = realToFrac tdelay / 1_000_000

        handle <- reactInitSoundscape alApp 0 sf

        --timeout 1000000 $ forever do
        --        (Yampa.react handle (negate dt, Nothing))
        --        (threadDelay tdelay)
        dtRef <- newIORef 0
        timeout 3_000_000 $ forever do
            dt <- tack dtRef
            tick dtRef
            Yampa.react handle (dt / 1_000_000, Nothing)
            when (dt < tdelay) (threadDelay $ round (tdelay / 1.5))
        pure ()
  where
    sf = ss [-- (fmap pure (slideIn 0.5 4 <<< source (File "test/audio.wav" (0, 137))))
              reverb () <<< (tremolo 0.8 10 <<< source (Sine 230 1))
            ]
            -}
