{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import FRP.Yampa.OpenAL
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
import FRP.Yampa (SF,Event(..),returnA,(<<<),arr)
import Data.IORef
import Control.Concurrent
import Control.Monad
import Foreign.Storable
import Data.StateVar
import qualified Data.Map as Map
import System.Random


main :: IO ()
main = do

    AL.runALUT "testing-yampa-alut" [] \x y -> do
     
     ----------------------------------------------
      -- resource handling done outside of yampa
      helloSource <- ObjectName.genObjectName
      sineSource  <- ObjectName.genObjectName
      AL.sourceRelative helloSource $= AL.World -- TODO move to reactInitALUT?
      AL.sourceRelative sineSource  $= AL.World -- TODO move to reactInitALUT??
      bufferDS <- AL.createBuffer AL.HelloWorld
      bufferES <- AL.createBuffer (AL.Sine 200 1 10)
      AL.buffer helloSource $= Just bufferDS
      AL.buffer sineSource  $= Just bufferES
      stdGen <- getStdGen
      
      ----------------------------------------------
      -- processing done inside yampa
      handle <- reactInitOpenAL 0 do
        proc s -> do
            x <- Yampa.noise stdGen -< s
            n <- Yampa.noise stdGen -< s
            m <- Yampa.noise stdGen -< s
            sigX <- Yampa.occasionally stdGen 0.5 () -< s
            sigY <- Yampa.occasionally stdGen 0.5 () -< s
            sigZ <- Yampa.occasionally stdGen 0.5 () -< s
            let sigX_ = if sigX == Event () then x*s*(-1) else x*s
            let sigY_ = if sigX == Event () then (-1) else 1
            let sigZ_ = if sigX == Event () then (-1) else 1
            let soundscapeSources = Map.fromList
                                        [ (helloSource, (source helloSource) { sourcePitch = sin m, sourceRelative = AL.World })
                                        , (sineSource , (source  sineSource) { sourcePitch = sin n, sourceRelative = AL.World })
                                        ]
            let soundscapeListener = listener { listenerPosition = V3 sigX_ sigY_ sigZ_ }
            let soundscapeShouldClose = False
            returnA -< Soundscape{..}
      forever do
          threadDelay 1000000
          Yampa.react handle (0.01, Just 1.3)
