{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}

import FRP.Yampa.OpenAL
import qualified Sound.ALUT.Loaders as AL
import qualified FRP.Yampa as Yampa
import FRP.Yampa (SF,Event(..),returnA,(<<<),arr)
import Control.Concurrent
import Control.Monad
import qualified Data.Map as Map
import System.Random
import Debug.Trace
import Control.Concurrent.Async


main :: IO ()
main = do
    withALUT \alApp -> do
      stdGen <- getStdGen
      handle <- Yampa.reactInit (pure 0) (\_ updated ss -> when updated (runSoundscape alApp ss) >> pure updated) $
        proc _someState -> do
            isTrue  <- arr ((> 0) . round @Float) <<< Yampa.noise stdGen -< ()
            xs <-     Yampa.delay del [(100, rS 840)]
                  <<< Yampa.delay del [] 
                  <<< Yampa.delay del [(100, rS 940)]
                  <<< Yampa.delay del [] 
                  <<< Yampa.delay del [(100, rS 1040)]
                  <<< Yampa.delay del []
                  <<< Yampa.delay del [(100, rS 1120)]
                  -< [(100, rS 840)]
            --returnA -< soundscape { soundscapeSources = if isTrue then mempty else Map.fromList xs }
            returnA -< soundscape { soundscapeSources = Map.fromList xs }
      forever do
          concurrently
              (threadDelay tdelay)
              (Yampa.react handle (dt, Nothing))
  where
    tdelay = 16660
    dt = realToFrac tdelay / 1000000
    del = 0.1
    rS     hz = (source (Sine hz 1)) { sourceGain = 0.5 }
    fooss     = source (File "test/audio.wav" Nothing)
