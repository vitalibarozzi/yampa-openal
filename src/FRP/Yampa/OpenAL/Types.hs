{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FRP.Yampa.OpenAL.Types (
    Soundstage (..),
    Listener (..),
    Source (..),
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
)
where

import Data.Map (Map)
import FRP.Yampa (SF,Time)
import Linear as L
import qualified Sound.ALUT.Loaders as AL
import qualified Sound.OpenAL as AL

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
    , soundstageTime {-----------} :: !Time
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

-----------------------------------------------------------

-- | A source of audio in space.
data Source = Source
    { sourceID {-----------------} :: !String -- Static.
    , sourceBufferQueue {--------} :: !AL.Buffer -- Static. TODO not a queue anymore
    , sourceLoopingMode {--------} :: !AL.LoopingMode -- Static.
    , sourceRolloffFactor {------} :: !Factor -- Static.
    , sourceReferenceDistance {--} :: !Meters -- Static.
    , sourceMaxDistance {--------} :: !Meters -- Static.
    , sourceRelative {-----------} :: !AL.SourceRelative -- Static.
    , sourceGainBounds {---------} :: !(Gain, Gain) -- Static.
    , sourceStartOffset {--------} :: !Time -- Static.
    , sourcePosition {-----------} :: !(V3 Meters)
    , sourceVelocity {-----------} :: !(V3 MetersPerSecond)
    , sourceDirection {----------} :: !(V3 Meters)
    , sourceGain {---------------} :: !Gain
    , sourceConeAngles {---------} :: !(Angle, Angle) -- Outer cone, inner cone, in degrees.
    , sourceConeOuterGain {------} :: !Gain -- Gain outside the cone.
    , sourceState {--------------} :: !AL.SourceState -- Almost always AL.Playing. Should not be changed directly.
    , sourcePitch {--------------} :: !Pitch -- Almost always one. Should not be changed directly as it causes time dialation.
    , sourceOffset {-------------} :: !Time -- Avoid changing it directly.
    }
    deriving
        (Eq, Show)

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

-----------------------------------------------------------
type SourceSignal a = SF a Source

-----------------------------------------------------------
type DeltaSource = SF Source Source

-----------------------------------------------------------
-- idea
type SV a b = (SF Source b, SF a b -> SF a Source -> SF a Source)
