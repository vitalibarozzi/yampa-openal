{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FRP.Yampa.OpenAL.Types
{-
    Soundstage (..),
    Listener (..),
    Source (..),
    --Status(..),
    SourceSignal,
    DeltaSource,
    Pitch,
    Meters,
    Angle,
    Factor,
    Depth,
    Rate,
    SV,
    MetersPerSecond,
    AL.SoundDataSource (..),
    AL.SourceRelative (..),
    AL.DistanceModel (..),
    AL.LoopingMode (..),
    AL.SourceState (..),
    AL.Buffer,
    -}
where

import Data.Map (Map)
import FRP.Yampa (SF,Time)
import Linear as L
import qualified Sound.ALUT.Loaders as AL
import qualified Sound.OpenAL as AL
import qualified Sound.OpenAL.AL.Buffer as AL
import Data.IORef
import Control.Concurrent.MVar
import Data.Set (Set)



-- | Internal.
data ALApp = ALApp
    { sourceMap :: !(MVar (Map String AL.Source))
    , createdMap :: !(IORef (Set String))
    }

-----------------------------------------------------------
type MetersPerSecond = Float

-----------------------------------------------------------
type Meters = Float

-----------------------------------------------------------
type Factor = Float

-----------------------------------------------------------
type Gain = Double

-----------------------------------------------------------
type Angle = Double

-----------------------------------------------------------
type Pitch = Double

-----------------------------------------------------------
type Depth = Double {- 0 to 1 -}

-----------------------------------------------------------
type Rate = Double


{-
-----------------------------------------------------------

{- | A model of the how the sound elements are defined at
one specific point in time.
-}
data Soundstage = Soundstage
    { soundstageSources {--------} :: !(Map String Source)
    , soundstageDopplerFactor {--} :: !Factor
    , soundstageSpeedOfSound {---} :: !MetersPerSecond
    , soundstageDistanceModel {--} :: !AL.DistanceModel
    , soundstageListener {-------} :: !Listener
    , soundstageTime {-----------} :: !(Maybe Time)
    }
    deriving
        (Eq, Show)

-----------------------------------------------------------
data Listener = Listener_
    { listenerPosition {-----} :: !(V3 Float) -- meters
    , listenerVelocity {-----} :: !(V3 Float) -- meters per second
    , listenerOrientation {--} :: !(V3 Float, V3 Float) -- meters
    , listenerGain {---------} :: !Gain
    }
    deriving
        (Eq, Show)

-}
