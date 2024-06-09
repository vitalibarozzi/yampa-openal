{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FRP.Yampa.OpenAL.Types (
    Soundstage (..),
    Source (..),
    SourceSignal,
    AL.SoundDataSource(..),
    AL.SourceRelative(..),
    AL.DistanceModel(..),
    AL.LoopingMode(..),
    AL.SourceState(..),
    AL.Buffer,
)
where


import Data.Map (Map)
import Linear.V3 (V3)
import qualified Sound.OpenAL as AL
import qualified Sound.ALUT.Loaders as AL

-----------------------------------------------------------

{- | A model of the how the sound elements are defined at
one specific point in time.
-}
data Soundstage = Soundstage
    { soundstageSources {--------------} :: !(Map String Source)
    , soundstageDopplerFactor {--------} :: !Factor
    , soundstageSpeedOfSound {---------} :: !MetersPerSecond
    , soundstageDistanceModel {--------} :: !AL.DistanceModel
    , soundstageListenerPosition {-----} :: !(V3 Meters)
    , soundstageListenerVelocity {-----} :: !(V3 MetersPerSecond)
    , soundstageListenerOrientation {--} :: !(V3 Meters, V3 Meters)
    , soundstageListenerGain {---------} :: !Gain
    }
    deriving
        (Eq, Show)

-----------------------------------------------------------

-- | A source of audio in space.
data Source = Source
    { sourceID {-----------------} :: !String
    , sourcePosition {-----------} :: !(V3 Meters)
    , sourceVelocity {-----------} :: !(V3 MetersPerSecond)
    , sourceDirection {----------} :: !(V3 Meters)
    , sourceGain {---------------} :: !Gain
    , sourcePitch {--------------} :: !Gain
    , sourceRelative {-----------} :: !AL.SourceRelative
    , sourceConeAngles {---------} :: !(Angle, Angle) -- Outer cone, inner cone, in degrees.
    , sourceConeOuterGain {------} :: !Gain -- Gain outside the cone.
    , sourceRolloffFactor {------} :: !Factor
    , sourceReferenceDistance {--} :: !Meters
    , sourceMaxDistance {--------} :: !Meters
    , sourceGainBounds {---------} :: !(Gain, Gain)
    , sourceLoopingMode {--------} :: !AL.LoopingMode
    , sourceState {--------------} :: !AL.SourceState
    , sourceBufferQueue {--------} :: ![AL.Buffer]
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
type SourceSignal a = SF a Source
