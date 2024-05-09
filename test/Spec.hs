{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import FRP.Yampa.ALUT
import qualified Sound.OpenAL.AL.Listener as AL
import qualified Sound.OpenAL.AL.Buffer as AL
import qualified Sound.OpenAL.AL.Source as AL
import qualified Sound.ALUT.Loaders as AL
import qualified Sound.ALUT.Initialization as AL
import qualified Sound.OpenAL as AL
import qualified Data.ObjectName as ObjectName
import Linear.V2
import Linear.V3
import qualified FRP.Yampa as Yampa
import FRP.Yampa (SF,Event(..),returnA)
import Data.IORef
import Control.Concurrent
import Control.Monad
import Foreign.Storable
import Data.StateVar
import qualified Data.Map as Map
import System.Random


main :: IO ()
main = do

    AL.runALUT "heyyy" ["heellp"] \x y -> do

      helloSource <- ObjectName.genObjectName
      sineSource  <- ObjectName.genObjectName

      case AL.sourceRelative helloSource of StateVar get set -> set AL.World -- TODO move to reactInitALUT?
      case AL.sourceRelative sineSource  of StateVar get set -> set AL.World -- TODO move to reactInitALUT??

      let ds = AL.HelloWorld
      let es = AL.Sine 400 0.93 5
      bufferDS <- AL.createBuffer ds
      bufferES <- AL.createBuffer es
      case AL.buffer helloSource of StateVar get set -> set (Just bufferDS)
      case AL.buffer sineSource  of StateVar get set -> set (Just bufferES)

      stdGen <- getStdGen

      handle <- reactInitALUT () do
        proc s -> do
            x <- fmap (*1.2) (Yampa.noise stdGen) -< s
            y <- fmap (*1.2) (Yampa.noise stdGen) -< s
            z <- fmap (*1.2) (Yampa.noise stdGen) -< s

            n <- Yampa.noise stdGen -< s

            m <- Yampa.arr sin Yampa.<<< Yampa.integral -< 2
            let sources                = [(helloSource, (source helloSource) { sourcePitch = sin m })
                                         ,(sineSource , (source  sineSource) { sourcePitch = sin n })
                                         ]
            let soundscapeSources      = Map.fromList sources
            let soundscapeListener     = listener { listenerPosition = V3 x y z }
            let soundscapeShouldClose  = False
            returnA -< Soundscape{..}

      forever do
          threadDelay 1000000
          Yampa.react handle (0.01, Just ())
