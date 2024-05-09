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

    AL.runALUT "hello" ["idk"] \x y -> do

      let ds = AL.HelloWorld
      helloSource <- ObjectName.genObjectName
      case AL.sourceRelative    helloSource of StateVar get set -> set AL.World
      --case AL.referenceDistance helloSource of StateVar get set -> set 10
      --case AL.maxDistance       helloSource of StateVar get set -> set 10

      buffer <- AL.createBuffer (AL.Sine 500 1 10)--ds
      case AL.buffer helloSource of StateVar get set -> set (Just buffer)

      stdGen <- getStdGen

      handle <- reactInitALUT () do
        proc s -> do
            (x,y,z) <- Yampa.integral -< (-1,1,1)
            n <- Yampa.noise stdGen -< s
            let sources            = [(helloSource, (source helloSource) { sourcePitch = n })]
            let soundscapeSources      = Map.fromList sources
            let soundscapeListener     = listener { listenerPosition = V3 x y z }
            let soundscapeShouldClose  = False
            returnA -< Soundscape{..}

      forever do
          threadDelay 100000
          Yampa.react handle (0.01,Just ())
