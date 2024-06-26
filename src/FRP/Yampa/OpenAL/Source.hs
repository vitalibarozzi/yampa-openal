{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module FRP.Yampa.OpenAL.Source
    (Source
        ( sourceRolloffFactor
        , sourceReferenceDistance
        , sourceMaxDistance
        , sourceRelative
        , sourceGainBounds
        , sourceLoopingMode
        , sourceBufferQueue
        , sourceGain
        , sourcePosition
        , sourceVelocity
        , sourceDirection
        , sourceConeAngles
        , sourceConeOuterGain
        )
    , source
    , source_
    , setOffset
    , setState
    , withPitch
    )
where

import FRP.Yampa.OpenAL.Util ()
import qualified Sound.OpenAL.AL.Buffer as AL
import Data.Maybe
import FRP.Yampa
import qualified FRP.Yampa as Yampa
import FRP.Yampa.OpenAL.Types
import Linear.V3
import qualified Sound.OpenAL as AL

-----------------------------------------------------------

-- | A source of audio in space.
data Source = Source
    { _sourceID {-----------------} :: !String -- Private, do not export. Static.
    , _sourceState {--------------} :: !AL.SourceState -- Private, do not export.
    , _sourcePitch {--------------} :: !Pitch -- Private, do not export.
    , _sourceOffset {-------------} :: !Time -- Private, do not export. Avoid changing it directly.
    , sourceRolloffFactor {-------} :: !Factor -- Static.
    , sourceReferenceDistance {---} :: !Meters -- Static.
    , sourceMaxDistance {---------} :: !Meters -- Static.
    , sourceRelative {------------} :: !AL.SourceRelative -- Static.
    , sourceGainBounds {----------} :: !(AL.Gain, AL.Gain) -- Static.
    , sourceLoopingMode {---------} :: !AL.LoopingMode -- Static.
    , sourceBufferQueue {---------} :: ![AL.Buffer]
    , sourceGain {----------------} :: !Gain
    , sourcePosition {------------} :: !(V3 Meters)
    , sourceVelocity {------------} :: !(V3 MetersPerSecond)
    , sourceDirection {-----------} :: !(V3 Meters)
    , sourceConeAngles {----------} :: !(Angle, Angle) -- Outer cone, inner cone, in degrees.
    , sourceConeOuterGain {-------} :: !AL.Gain -- Gain outside the cone.
    }
    deriving
        (Eq, Show)

-----------------------------------------------------------

-- | Constructor with default values.
source ::
    String ->
    AL.Buffer ->
    SF a Source
{-# INLINE source #-}
source name queue =
    source_
        name
        [queue]
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing


{- | Smart constructor for a sound source. It handles stuff
like keeping the offset correctly when we speed up or slow
down time by changing pitch, for example. Also handles the
movement of the source based on its velocity.
-}
source_ ::
    String -> -- name
    [AL.Buffer] -> -- queue
    Maybe DTime -> -- start at
    Maybe (AL.Gain, AL.Gain) -> -- Gain bounds
    Maybe AL.Gain -> -- Outer gain.
    Maybe AL.SourceRelative ->
    Maybe Float -> -- rolloff
    Maybe Pitch -> -- pitch
    Maybe (V3 Float) -> -- position
    Maybe (V3 Float) -> -- velocity
    Maybe (V3 Float) -> -- direction
    Maybe (Double, Double) -> -- cone angles -- TODO sf
    Maybe AL.SourceState -> -- source state
    Maybe Double -> -- source gain
    Maybe AL.LoopingMode -> -- source gain
    SF a Source
{-# INLINE source_ #-}
source_
    _sourceID
    sourceBufferQueue
    (fromMaybe 0 -> sourceStartOffset)
    (fromMaybe (0.2, 2) -> sourceGainBounds)
    (fromMaybe 0.01 -> sourceConeOuterGain)
    (fromMaybe AL.World -> sourceRelative)
    (fromMaybe 1 -> sourceRolloffFactor)
    (fromMaybe 1 -> _sourcePitch)
    (fromMaybe 0 -> position0)
    (fromMaybe 0 -> sourceVelocity)
    (fromMaybe 0 -> sourceDirection)
    (fromMaybe (360,360) -> sourceConeAngles)
    (fromMaybe AL.Stopped -> state0)
    (fromMaybe 1 -> sourceGain)
    (fromMaybe AL.OneShot -> sourceLoopingMode)=
        proc a -> do
            t <- Yampa.time -< a
            sourcePosition <- arr (+position0) <<< Yampa.integral -< sourceVelocity
            let correction s = if s == AL.Playing then 1 else 0
            returnA
                -<
                    Source
                        { sourceReferenceDistance = 1
                        , sourceMaxDistance = 1_000 -- 1KM
                        , _sourceState = state0
                        , _sourceOffset = sourceStartOffset + (correction state0 * t * _sourcePitch)
                        , ..
                        }

-----------------------------------------------------------
setState :: AL.SourceState -> SF a Source -> SF a Source
{-# INLINE setState #-}
setState state src =
    src >>> proc src0 -> do
        let newSource =
                source_
                    (_sourceID src0)
                    (sourceBufferQueue src0)
                    (Just $ _sourceOffset src0)
                    (Just (sourceGainBounds src0))
                    (Just (sourceConeOuterGain src0))
                    (Just (sourceRelative src0))
                    (Just $ sourceRolloffFactor src0)
                    (Just $ _sourcePitch src0)
                    (Just $ sourcePosition src0)
                    (Just $ sourceVelocity src0)
                    (Just $ sourceDirection src0)
                    (Just $ sourceConeAngles src0)
                    (Just state)
                    (Just $ sourceGain src0)
                    (Just $ sourceLoopingMode src0)
        rSwitch identity -< (src0, tag (Event ()) newSource)

-----------------------------------------------------------
-- | Will jump to this exact offset.
setOffset :: Time -> SF a Source -> SF a Source
{-# INLINE setOffset #-}
setOffset offset src =
    src >>> proc src0 -> do
        -- Using AL.Initial to communicate to the backend that
        -- we should jump to that offset.
        state1 <- initially AL.Initial -< _sourceState src0
        let newSource =
                source_
                    (_sourceID src0)
                    (sourceBufferQueue src0)
                    (Just offset)
                    (Just (sourceGainBounds src0))
                    (Just (sourceConeOuterGain src0))
                    (Just (sourceRelative src0))
                    (Just $ sourceRolloffFactor src0)
                    (Just $ _sourcePitch src0)
                    (Just $ sourcePosition src0)
                    (Just $ sourceVelocity src0)
                    (Just $ sourceDirection src0)
                    (Just $ sourceConeAngles src0)
                    (Just state1)
                    (Just $ sourceGain src0)
                    (Just $ sourceLoopingMode src0)
        rSwitch identity -< (src0, tag (Event ()) newSource)

-----------------------------------------------------------
-- \| Modify the pitch and adjusts the offset of the source
--    accordingly. If pitch is modified separatly from the offset
--    they will get desynchronized as the the actual sound in IO
--    will be running faster or slower in relation to the source.
withPitch :: SF a Pitch -> SF a Source -> SF a Source
{-# INLINE withPitch #-}
withPitch pitchSF src = do
    (src &&& pitchSF) >>> proc (src0, pitch) -> do
        pitchChanged <- iEdge False -< pitch /= _sourcePitch src0
        let newSource =
                source_
                    (_sourceID src0)
                    (sourceBufferQueue src0)
                    (Just $ _sourceOffset src0)
                    (Just (sourceGainBounds src0))
                    (Just (sourceConeOuterGain src0))
                    (Just (sourceRelative src0))
                    (Just $ sourceRolloffFactor src0)
                    (Just pitch)
                    (Just $ sourcePosition src0)
                    (Just $ sourceVelocity src0)
                    (Just $ sourceDirection src0)
                    (Just $ sourceConeAngles src0)
                    (Just $ _sourceState src0)
                    (Just $ sourceGain src0)
                    (Just $ sourceLoopingMode src0)
        rSwitch identity -< (src0, tag pitchChanged newSource)

