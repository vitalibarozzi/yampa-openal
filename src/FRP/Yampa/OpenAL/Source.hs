{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module FRP.Yampa.OpenAL.Source (
    Source (
        sourceRolloffFactor,
        sourceReferenceDistance,
        sourceMaxDistance,
        sourceRelative,
        sourceGainBounds,
        sourceLoopingMode,
        sourceGain,
        sourcePosition,
        sourceVelocity,
        sourceDirection,
        sourceConeAngles,
        sourceConeOuterGain
    ),
    emptySource,
    source,
    source_,
    setOffset,
    setState,
    withPitch,
    withGain,
    updateSource,
    ----
    readSourceID, -- temp?
)
where

import Control.Monad
import Data.Maybe
import FRP.Yampa
import qualified FRP.Yampa as Yampa
import FRP.Yampa.OpenAL.Types
import FRP.Yampa.OpenAL.Util
import Linear.V3
import qualified Sound.OpenAL as AL
import Sound.OpenAL (get)
import Control.Monad.IO.Class

-----------------------------------------------------------

-- | A source of audio in space with multiple buffers in
-- queue to be played.
data Source = Source
    { _sourceID {-----------------} :: !AL.Source -- Internal.
    , _sourceState {--------------} :: !AL.SourceState -- Internal.
    , _sourcePitch {--------------} :: !Pitch -- Internal.
    , _sourceOffset {-------------} :: !Time -- Internal.
    , _sourceBufferQueue {--------} :: ![AL.Buffer] -- Empty buffer queues are valid.
    , sourceLoopingMode {---------} :: !AL.LoopingMode -- Static.
    , sourceRelative {------------} :: !AL.SourceRelative -- Static.
    , sourceRolloffFactor {-------} :: !Factor -- Static.
    , sourceReferenceDistance {---} :: !Meters -- Static.
    , sourceMaxDistance {---------} :: !Meters -- Static.
    , sourceGain {----------------} :: !AL.Gain
    , sourceGainBounds {----------} :: !(AL.Gain, AL.Gain) -- Static.
    , sourceConeOuterGain {-------} :: !AL.Gain -- Gain outside the cone.
    , sourcePosition {------------} :: !(V3 Meters)
    , sourceVelocity {------------} :: !(V3 MetersPerSecond)
    , sourceDirection {-----------} :: !(V3 Meters)
    , sourceConeAngles {----------} :: !(Angle, Angle) -- Outer cone, inner cone, in degrees.

    }
    deriving
        (Eq, Show)


-----------------------------------------------------------
emptySource :: AL.Source -> Source
{-# INLINE emptySource #-}
emptySource sid =
    Source
        sid
        AL.Initial
        1
        0
        []
        AL.OneShot
        AL.World
        1
        1
        1_000
        0.7
        (0.2, 2.0)
        0.5
        0
        0
        (V3 0 (-1) 0)
        (360,360)

-----------------------------------------------------------

{- | Simple constructor with default values and only one
buffer. Starts playing immediatly and will play only once,
been heard everywhere in the world.
-}
source ::
    AL.Source ->
    AL.Buffer ->
    SF a Source
{-# INLINE source #-}
source name queue =
    source_
        name
        (pure queue)
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
        (Just AL.Playing)
        Nothing
        Nothing

-----------------------------------------------------------
setState :: AL.SourceState -> SF a Source -> SF a Source
{-# INLINE setState #-}
setState state src =
    src >>> proc src0 -> do
        let newSource =
                source_
                    (_sourceID src0)
                    (_sourceBufferQueue src0)
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
                    (_sourceBufferQueue src0)
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
withGain :: SF a AL.Gain -> SF a Source -> SF a Source
{-# INLINE withGain #-}
withGain gainSF srcSF = proc a -> do
    sourceGain1 <- gainSF -< a
    source1     <- srcSF  -< a
    returnA -< source1 { sourceGain = sourceGain1 }

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
                    (_sourceBufferQueue src0)
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

{- | Smart constructor for a sound source. It handles stuff
like keeping the offset correctly when we speed up or slow
down time by changing pitch, for example. Also handles the
movement of the source based on its velocity.
-}
source_ ::
    AL.Source -> -- name
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
    Maybe (Double, Double) -> -- cone angles
    Maybe AL.SourceState -> -- source state
    Maybe AL.Gain -> -- source gain
    Maybe AL.LoopingMode -> -- source looping mode
    SF a Source
{-# INLINE source_ #-}
source_
    _sourceID
    _sourceBufferQueue
    (fromMaybe {------------} 0 -> sourceStartOffset)
    (fromMaybe {-----} (0.2, 2) -> sourceGainBounds)
    (fromMaybe {---------} 0.01 -> sourceConeOuterGain)
    (fromMaybe {-----} AL.World -> sourceRelative)
    (fromMaybe {------------} 1 -> sourceRolloffFactor)
    (fromMaybe {------------} 1 -> _sourcePitch)
    (fromMaybe {------------} 0 -> position0)
    (fromMaybe {------------} 0 -> sourceVelocity)
    (fromMaybe {------------} 0 -> sourceDirection)
    (fromMaybe {---} (360, 360) -> sourceConeAngles)
    (fromMaybe {----} AL.Paused -> state0)
    (fromMaybe {------------} 1 -> sourceGain)
    (fromMaybe {---} AL.OneShot -> sourceLoopingMode) =
        proc a -> do
            t <- Yampa.time -< a
            sourcePosition <- arr (+ position0) <<< Yampa.integral -< sourceVelocity
            let correction s = if s == AL.Playing then 1 else 0
            returnA
                -<
                    Source
                        { sourceReferenceDistance = 1 -- 1M
                        , sourceMaxDistance = 1_000 -- 1KM
                        , _sourceState = state0
                        , _sourceOffset = sourceStartOffset + (correction state0 * t * _sourcePitch)
                        , ..
                        }

-----------------------------------------------------------

{- | Compare the two sources and update them in IO, with
OpenAL, to match the changes, keeping it updated with
the last changes in the source, but only updating what
have changed.
-}
updateSource :: (MonadIO m) => Source -> Source -> m ()
{-# INLINEABLE updateSource #-}
updateSource s0 s1 = do
    handleState   (_sourceID s1) -- we decide if the source should keep playing or stop or pause
    handleBuffers (_sourceID s1) -- we make changes to the buffers
    handleFields  (_sourceID s1) -- we change the source data (pausing if needed, then resuming)
  where
    handleBuffers sid = do
        -- TODO for now we assume the buffer queue will not change
        let buffChanged = _sourceBufferQueue s0 /= _sourceBufferQueue s1
        when (buffChanged || _sourceState s0 == AL.Initial) do
            pn <- get (AL.buffersProcessed sid)
            qn <- get (AL.buffersQueued sid)
            case (pn,qn) of
                (0,0) -> do
                    AL.queueBuffers sid (_sourceBufferQueue s1)
                    AL.play [sid]
                _____ -> error . show $ (pn,qn)
            

    handleFields sid = do
        ($=?) (AL.secOffset sid) (_sourceState s1 == AL.Initial) (realToFrac (_sourceOffset s1)) -- TODO check if we need to pause before changing the offset to avoid pops
        ($=?) (AL.coneAngles sid) (_sourcePitch s1 /= _sourcePitch s0) (realToFrac $ fst $ sourceConeAngles s1, realToFrac $ snd (sourceConeAngles s1))
        ($=?) (AL.coneOuterGain sid) (_sourcePitch s1 /= _sourcePitch s0) (sourceConeOuterGain s1)
        ($=?) (AL.direction sid) (_sourcePitch s1 /= _sourcePitch s0) (_v3ToVector (sourceDirection s1))
        ($=?) (AL.gainBounds sid) (_sourcePitch s1 /= _sourcePitch s0) (let foox = fst (sourceGainBounds s1) in (foox, snd (sourceGainBounds s1)))
        ($=?) (AL.loopingMode sid) (_sourcePitch s1 /= _sourcePitch s0) (sourceLoopingMode s1)
        ($=?) (AL.maxDistance sid) (_sourcePitch s1 /= _sourcePitch s0) (realToFrac (sourceMaxDistance s1))
        ($=?) (AL.pitch sid) (_sourcePitch s1 /= _sourcePitch s0) (realToFrac (abs (_sourcePitch s1)))
        ($=?) (AL.referenceDistance sid) (_sourcePitch s1 /= _sourcePitch s0) (realToFrac (sourceReferenceDistance s1))
        ($=?) (AL.rolloffFactor sid) (_sourcePitch s1 /= _sourcePitch s0) (realToFrac (sourceRolloffFactor s1))
        ($=?) (AL.sourceGain sid) (_sourcePitch s1 /= _sourcePitch s0) (realToFrac (abs (sourceGain s1)))
        ($=?) (AL.sourcePosition sid) (_sourcePitch s1 /= _sourcePitch s0) (_v3ToVertex (sourcePosition s1))
        ($=?) (AL.sourceRelative sid) (_sourcePitch s1 /= _sourcePitch s0) (sourceRelative s1)
        ($=?) (AL.sourceVelocity sid) (_sourcePitch s1 /= _sourcePitch s0) (_v3ToVector (sourceVelocity s1))

    handleState sid = do
        when (notPlaying && _sourceState s1 == AL.Playing) (AL.play [sid])
        when (notPaused && _sourceState s1 == AL.Paused) (AL.pause [sid])
        when (notStopped && _sourceState s1 == AL.Stopped) (AL.stop [sid])
      where
        notPlaying = _sourceState s0 == AL.Initial || _sourceState s0 == AL.Stopped || _sourceState s0 == AL.Paused
        notPaused = _sourceState s0 == AL.Stopped || _sourceState s0 == AL.Initial
        notStopped = _sourceState s0 == AL.Initial || _sourceState s0 == AL.Paused


-- temp
readSourceID = _sourceID
