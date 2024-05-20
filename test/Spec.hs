{-# LANGUAGE Arrows #-}
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


main :: IO ()
main = do
    withALUT \alApp -> do
      stdGen <- getStdGen
      handle <- Yampa.reactInit (pure 0) (\_ updated ss -> when updated (runSoundscape alApp ss) >> pure updated) $
        proc _someState -> do
            t <- Yampa.time         -< ()
            m <- Yampa.noise stdGen -< ()
            zs <- Yampa.delay 0.5 [(0, rS 140)] -< [(0, hello t m)]
            ys <- Yampa.delay 1.0 [             ] -< [(222, sS t)]
            xs <- Yampa.delay 2.0 [(111, rS 440)] -< []
            returnA -< soundscape { soundscapeSources = Map.fromList (zs<>ys<>xs) }
      forever do
          threadDelay 50000
          Yampa.react handle (0.05, Nothing)
  where
    hello d m = (source AL.HelloWorld     ) { sourcePitch = abs $ sin (realToFrac d+m)}
    sS    d   = (source (AL.Sine 200 1 10))  { sourceOffset = Just (10, abs $ realToFrac d)}
    rS     hz = source (AL.Sine hz  1 10)
