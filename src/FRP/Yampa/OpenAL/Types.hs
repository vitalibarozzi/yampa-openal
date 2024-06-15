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
    AL.SoundDataSource (..),
    AL.SourceRelative (..),
    AL.DistanceModel (..),
    AL.LoopingMode (..),
    AL.SourceState (..),
    AL.Buffer,
)
where

import Data.Map (Map)
import FRP.Yampa (SF)
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
    }
    deriving
        (Eq, Show)

-----------------------------------------------------------
data Listener = Listener_
    { listenerPosition {-----} :: !(V3 Float) -- meters
    , listenerVelocity {-----} :: !(V3 Float) -- meters per second
    , listenerOrientation {--} :: !(V3 Float, V3 Float) -- meters
    , listenerGain {---------} :: !Float
    }
    deriving
        (Eq, Show)

-----------------------------------------------------------

-- | A source of audio in space.
data Source = Source
    { sourceID {-----------------} :: !String -- Static.
    , sourceBufferQueue {--------} :: ![AL.Buffer] -- Static.
    , sourceLoopingMode {--------} :: !AL.LoopingMode -- Static.
    , sourcePosition {-----------} :: !(V3 Meters)
    , sourceVelocity {-----------} :: !(V3 MetersPerSecond)
    , sourceDirection {----------} :: !(V3 Meters)
    , sourceGain {---------------} :: !Gain
    , sourceRelative {-----------} :: !AL.SourceRelative
    , sourceConeAngles {---------} :: !(Angle, Angle) -- Outer cone, inner cone, in degrees.
    , sourceConeOuterGain {------} :: !Gain -- Gain outside the cone.
    , sourceRolloffFactor {------} :: !Factor
    , sourceReferenceDistance {--} :: !Meters
    , sourceMaxDistance {--------} :: !Meters
    , sourceGainBounds {---------} :: !(Gain, Gain)
    , sourceState {--------------} :: !AL.SourceState -- Almost always AL.Playing. Should not be changed directly.
    , sourcePitch {--------------} :: !Pitch -- Almost always one. Should not be changed directly as it causes time dialation.
    , sourceStartOffset {--------} :: !Float
    , sourceOffset {-------------} :: !Float
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
type SourceSignal a = SF a Source

-----------------------------------------------------------
type DeltaSource = SF Source Source
