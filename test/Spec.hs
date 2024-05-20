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
            xs <- Yampa.delay 2.0 [(111, rS 440)] -< [(111, zz (abs $ realToFrac t))]
            returnA -< soundscape { soundscapeSources = Map.fromList (zs<>ys<>xs) }
      forever do
          --10hz
          threadDelay 100000
          Yampa.react handle (0.1, Nothing)
  where
    hello d m = (source HelloWorld     ) { sourcePitch = abs $ sin (realToFrac d+m)}
    sS    d   = (source (Impulse 240 2)) { sourcePitch = abs $ sin (realToFrac d), sourceOffset = Just (1, abs $ realToFrac d)}
    rS     hz = source (Sine hz 10)
    zz     t  = (source (Square 100 t)) { sourcePitch = abs $ sin $ t + (realToFrac $ round $ sin t) }
