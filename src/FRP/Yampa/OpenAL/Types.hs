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
import Data.VectorSpace
import FRP.Yampa (SF)
import Linear as L
import qualified Sound.ALUT.Loaders as AL
import qualified Sound.OpenAL as AL

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
data Listener = Listener_
    { listenerPosition :: V3 Float
    , listenerVelocity :: V3 Float
    , listenerOrientation :: (V3 Float, V3 Float)
    , listenerGain :: Float
    }
    deriving
        (Eq, Show)

-----------------------------------------------------------

-- | A source of audio in space.
data Source = Source
    -- data
    { sourceID {-----------------} :: !String
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
    , -- executive
      sourceState {--------------} :: !AL.SourceState -- almost always playing.
    , sourceLoopingMode {--------} :: !AL.LoopingMode -- almost always one-shot.
    , sourceBufferQueue {--------} :: ![AL.Buffer]
    , sourcePitch {--------------} :: !Pitch -- almost always one.
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

-----------------------------------------------------------
instance (Eq a, Floating a) => VectorSpace (V2 a) a where
    zeroVector = L.zero
    (*^) = (L.*^)
    negateVector = L.negated
    (^+^) = (L.^+^)
    (^-^) = (L.^-^)
    dot = L.dot

-----------------------------------------------------------
instance (Eq a, Floating a) => VectorSpace (V3 a) a where
    zeroVector = L.zero
    (*^) = (L.*^)
    negateVector = L.negated
    (^+^) = (L.^+^)
    (^-^) = (L.^-^)
    dot = L.dot
