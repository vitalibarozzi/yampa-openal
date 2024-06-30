{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | Describes OpenAL source.
module FRP.Yampa.OpenAL.Source (

    -- * A sound source.
    Source (
        sourceRolloffFactor,
        sourceReferenceDistance,
        sourceMaxDistance,
        sourceRelative,
        sourceGainBounds,
        sourceGain,
        sourcePosition,
        sourceVelocity,
        sourceDirection,
        sourceConeAngles,
        sourceConeOuterGain
    ),

    -- * Constructors.
    emptySource,
    source,
    source_,
    streaming,

    -- * Getters for read-only fields
    sourceID,
    sourceOffset,
    sourceState,
    sourcePitch,
    sourceBufferQueue,
    sourceLoopingMode,

    -- * Constant setters.
    setRelative,
    setPosition,
    setVelocity,
    setLoopingMode, -- TODO probably should be public and done only with the smart constructor, never after
    setSourceType, -- TODO probably should be done public and only with the smart constructor, never after
    setOffset,
    setState, -- TODO replace with withState
    setQueue,
    setMaxDistance,
    setReferenceDistance,
    setRolloffFactor,

    -- * Variable setters.
    withState,
    withDirection,
    withConeAngles,
    withConeOuterGain,
    withPitch,
    withGain,

    -- * Playback controls.
    edgePlay,
    edgePause,
    edgeStop,

    -- * Execution.
    updateSource,
)
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import FRP.Yampa
import qualified FRP.Yampa as Yampa
import FRP.Yampa.OpenAL.Types
import FRP.Yampa.OpenAL.Util
import Linear.V3
import Sound.OpenAL (get, ($=))
import qualified Sound.OpenAL as AL

-----------------------------------------------------------

{- | A source of audio in space with multiple buffers in
queue to be played.
-}
data Source = Source
    -- PRIVATE------------------------------
    { _sourceID {-----------------} :: !AL.Source -- immutable.
    , _sourceChannelType {--------} :: !ChannelType -- immutable. If stereo cannot use positional sound.
    , _sourceBufferMagnitude {----} :: !Magnitude -- immutable.
    , _sourceBufferSampleRate {---} :: !Float -- immutable.
    , _sourceLoopingMode {--------} :: !AL.LoopingMode
    , _sourceBufferQueue {--------} :: ![AL.Buffer] -- Empty buffer queues are valid.
    , _sourcePitch {--------------} :: !Pitch
    , _sourceOffset {-------------} :: !Time
    , _sourceState {--------------} :: !AL.SourceState
    , _sourceType {---------------} :: !AL.SourceType
    , -- PUBLIC ----------------------------------
      sourceRolloffFactor {-------} :: !Factor
    , sourceReferenceDistance {---} :: !Meters
    , sourceMaxDistance {---------} :: !Meters
    , sourceGain {----------------} :: !AL.Gain
    , sourceGainBounds {----------} :: !(AL.Gain, AL.Gain)
    , sourceConeOuterGain {-------} :: !AL.Gain -- Gain outside the cone.
    , sourceRelative {------------} :: !AL.SourceRelative -- Decides in relation to what the source moves.
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
        { _sourceID {-----------------} = sid
        , _sourceChannelType {--------} = Mono
        , _sourceBufferMagnitude {----} = Bit16
        , _sourceBufferSampleRate {---} = 0
        , _sourceLoopingMode {--------} = AL.OneShot
        , _sourceBufferQueue {--------} = []
        , _sourcePitch {--------------} = 0
        , _sourceOffset {-------------} = 0
        , _sourceState {--------------} = AL.Initial
        , _sourceType {---------------} = AL.Undetermined
        , sourceRolloffFactor {-------} = 0
        , sourceReferenceDistance {---} = 0
        , sourceMaxDistance {---------} = 0
        , sourceGain {----------------} = 0
        , sourceGainBounds {----------} = (0, 0)
        , sourceConeOuterGain {-------} = 0
        , sourceRelative {------------} = AL.World
        , sourcePosition {------------} = 0
        , sourceVelocity {------------} = 0
        , sourceDirection {-----------} = 0
        , sourceConeAngles {----------} = (0, 0)
        }

-----------------------------------------------------------
sourceID :: Source -> AL.Source
{-# INLINE sourceID #-}
sourceID = _sourceID

-----------------------------------------------------------
sourceType :: Source -> AL.SourceType
{-# INLINE sourceType #-}
sourceType = _sourceType

-----------------------------------------------------------
sourceOffset :: Source -> Time
{-# INLINE sourceOffset #-}
sourceOffset = _sourceOffset

-----------------------------------------------------------
sourceState :: Source -> AL.SourceState
{-# INLINE sourceState #-}
sourceState = _sourceState

-----------------------------------------------------------
sourcePitch :: Source -> Pitch
{-# INLINE sourcePitch #-}
sourcePitch = _sourcePitch

-----------------------------------------------------------
sourceBufferQueue :: Source -> [AL.Buffer]
{-# INLINE sourceBufferQueue #-}
sourceBufferQueue = _sourceBufferQueue

-----------------------------------------------------------
sourceLoopingMode :: Source -> AL.LoopingMode
{-# INLINE sourceLoopingMode #-}
sourceLoopingMode = _sourceLoopingMode

-----------------------------------------------------------

{- | Simple constructor with default values and only one
buffer. Starts playing immediatly and will play only once,
been heard everywhere in the world.
-}
source ::
    AL.Source ->
    [AL.Buffer] ->
    SF a Source
{-# INLINE source #-}
source name queue =
    source_
        name
        queue
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
        Nothing

-----------------------------------------------------------
setState :: AL.SourceState -> SF a Source -> SF a Source
{-# INLINE setState #-}
setState state src =
    switchNow src \src0 ->
        source_
            (_sourceID src0)
            (_sourceBufferQueue src0)
            (if state == AL.Stopped then Nothing else Just $ sourceOffset src0)
            (Just $ sourceGainBounds src0)
            (Just $ sourceConeOuterGain src0)
            (Just $ sourceRelative src0)
            (Just $ sourceRolloffFactor src0)
            (Just $ sourcePitch src0)
            (Just $ sourcePosition src0)
            (Just $ sourceVelocity src0)
            (Just $ sourceDirection src0)
            (Just $ sourceConeAngles src0)
            (Just state)
            (Just $ sourceGain src0)
            (Just $ sourceLoopingMode src0)
            (Just $ sourceType src0)

-----------------------------------------------------------

-- | Will jump to this exact offset.
setOffset :: Time -> SF a Source -> SF a Source
{-# INLINE setOffset #-}
setOffset offset src =
    switchNow src \src0 ->
        source_
            (_sourceID src0)
            (_sourceBufferQueue src0)
            (Just offset)
            (Just $ sourceGainBounds src0)
            (Just $ sourceConeOuterGain src0)
            (Just $ sourceRelative src0)
            (Just $ sourceRolloffFactor src0)
            (Just $ sourcePitch src0)
            (Just $ sourcePosition src0)
            (Just $ sourceVelocity src0)
            (Just $ sourceDirection src0)
            (Just $ sourceConeAngles src0)
            (Just if sourceState src0 == AL.Stopped then AL.Paused else sourceState src0)
            (Just $ sourceGain src0)
            (Just $ sourceLoopingMode src0)
            (Just $ sourceType src0)

-----------------------------------------------------------
setQueue :: [AL.Buffer] -> SF a Source -> SF a Source
{-# INLINE setQueue #-}
setQueue queue src =
    switchNow src \src0 ->
        source_
            (_sourceID src0)
            queue
            (Just $ _sourceOffset src0)
            (Just $ sourceGainBounds src0)
            (Just $ sourceConeOuterGain src0)
            (Just $ sourceRelative src0)
            (Just $ sourceRolloffFactor src0)
            (Just $ _sourcePitch src0)
            (Just $ sourcePosition src0)
            (Just $ sourceVelocity src0)
            (Just $ sourceDirection src0)
            (Just $ sourceConeAngles src0)
            (Just $ _sourceState src0)
            (Just $ sourceGain src0)
            (Just $ sourceLoopingMode src0)
            (Just $ sourceType src0)

-----------------------------------------------------------
setRelative :: AL.SourceRelative -> SF a Source -> SF a Source
{-# INLINE setRelative #-}
setRelative relative src =
    switchNow src \src0 ->
        source_
            (_sourceID src0)
            (sourceBufferQueue src0)
            (Just $ _sourceOffset src0)
            (Just $ sourceGainBounds src0)
            (Just $ sourceConeOuterGain src0)
            (Just relative)
            (Just $ sourceRolloffFactor src0)
            (Just $ _sourcePitch src0)
            (Just $ sourcePosition src0)
            (Just $ sourceVelocity src0)
            (Just $ sourceDirection src0)
            (Just $ sourceConeAngles src0)
            (Just $ _sourceState src0)
            (Just $ sourceGain src0)
            (Just $ sourceLoopingMode src0)
            (Just $ sourceType src0)

-----------------------------------------------------------
setPosition :: V3 Float -> SF a Source -> SF a Source
{-# INLINE setPosition #-}
setPosition pos src =
    switchNow src \src0 ->
        source_
            (sourceID src0)
            (sourceBufferQueue src0)
            (Just $ sourceOffset src0)
            (Just $ sourceGainBounds src0)
            (Just $ sourceConeOuterGain src0)
            (Just $ sourceRelative src0)
            (Just $ sourceRolloffFactor src0)
            (Just $ sourcePitch src0)
            (Just pos)
            (Just $ sourceVelocity src0)
            (Just $ sourceDirection src0)
            (Just $ sourceConeAngles src0)
            (Just $ sourceState src0)
            (Just $ sourceGain src0)
            (Just $ sourceLoopingMode src0)
            (Just $ sourceType src0)

-----------------------------------------------------------
setVelocity :: V3 Float -> SF a Source -> SF a Source
{-# INLINE setVelocity #-}
setVelocity vel src =
    switchNow src \src0 ->
        source_
            (sourceID src0)
            (sourceBufferQueue src0)
            (Just $ sourceOffset src0)
            (Just $ sourceGainBounds src0)
            (Just $ sourceConeOuterGain src0)
            (Just $ sourceRelative src0)
            (Just $ sourceRolloffFactor src0)
            (Just $ sourcePitch src0)
            (Just $ sourcePosition src0)
            (Just vel)
            (Just $ sourceDirection src0)
            (Just $ sourceConeAngles src0)
            (Just $ sourceState src0)
            (Just $ sourceGain src0)
            (Just $ sourceLoopingMode src0)
            (Just $ sourceType src0)

-----------------------------------------------------------
setLoopingMode :: AL.LoopingMode -> SF a Source -> SF a Source
{-# INLINE setLoopingMode #-}
setLoopingMode loopingMode src =
    switchNow src \src0 ->
        source_
            (sourceID src0)
            (sourceBufferQueue src0)
            (Just $ sourceOffset src0)
            (Just $ sourceGainBounds src0)
            (Just $ sourceConeOuterGain src0)
            (Just $ sourceRelative src0)
            (Just $ sourceRolloffFactor src0)
            (Just $ sourcePitch src0)
            (Just $ sourcePosition src0)
            (Just $ sourceVelocity src0)
            (Just $ sourceDirection src0)
            (Just $ sourceConeAngles src0)
            (Just $ sourceState src0)
            (Just $ sourceGain src0)
            (Just loopingMode)
            (Just $ sourceType src0)

-----------------------------------------------------------
setSourceType :: AL.SourceType -> SF a Source -> SF a Source
{-# INLINE setSourceType #-}
setSourceType srcType src =
    switchNow src \src0 ->
        source_
            (sourceID src0)
            (sourceBufferQueue src0)
            (Just $ sourceOffset src0)
            (Just $ sourceGainBounds src0)
            (Just $ sourceConeOuterGain src0)
            (Just $ sourceRelative src0)
            (Just $ sourceRolloffFactor src0)
            (Just $ sourcePitch src0)
            (Just $ sourcePosition src0)
            (Just $ sourceVelocity src0)
            (Just $ sourceDirection src0)
            (Just $ sourceConeAngles src0)
            (Just $ sourceState src0)
            (Just $ sourceGain src0)
            (Just $ sourceLoopingMode src0)
            (Just srcType)

-----------------------------------------------------------

-- | Variable gain.
withGain :: SF a AL.Gain -> SF a Source -> SF a Source
{-# INLINE withGain #-}
withGain gainSF srcSF = proc a -> do
    sourceGain1 <- gainSF -< a
    source1 <- srcSF -< a
    returnA -< source1{sourceGain = sourceGain1}

-----------------------------------------------------------
-- \| Modify the pitch and adjusts the offset of the source
--    accordingly. If pitch is modified separatly from the offset
--    they will get desynchronized as the the actual sound in IO
--    will be running faster or slower in relation to the source.
withPitch :: SF a Pitch -> SF a Source -> SF a Source
{-# INLINE withPitch #-}
withPitch pitchSF src =
    (src &&& pitchSF) >>> proc (src0, pitch) -> do
        let newSource =
                source_
                    (sourceID src0)
                    (sourceBufferQueue src0)
                    (Just $ sourceOffset src0)
                    (Just (sourceGainBounds src0))
                    (Just (sourceConeOuterGain src0))
                    (Just (sourceRelative src0))
                    (Just $ sourceRolloffFactor src0)
                    (Just pitch)
                    (Just $ sourcePosition src0)
                    (Just $ sourceVelocity src0)
                    (Just $ sourceDirection src0)
                    (Just $ sourceConeAngles src0)
                    (Just $ sourceState src0)
                    (Just $ sourceGain src0)
                    (Just $ sourceLoopingMode src0)
                    (Just $ sourceType src0)
        ev <- edge -< pitch /= _sourcePitch src0
        rSwitch identity -< (src0, tag ev newSource)

-----------------------------------------------------------

{- | Extracts the buffer stream from the `a` value. Remember
that the buffers in queue need to all have the same format
and sample-rate.
-}
streaming ::
    (a -> [AL.Buffer]) ->
    SF a Source ->
    SF a Source
{-# INLINE streaming #-}
streaming stream src =
    src &&& arr stream >>> proc (src0, queue) -> do
        let newSource =
                source_
                    (_sourceID src0)
                    queue
                    (Just (_sourceOffset src0))
                    (Just (sourceGainBounds src0))
                    (Just (sourceConeOuterGain src0))
                    (Just (sourceRelative src0))
                    (Just $ sourceRolloffFactor src0)
                    (Just $ _sourcePitch src0)
                    (Just $ sourcePosition src0)
                    (Just $ sourceVelocity src0)
                    (Just $ sourceDirection src0)
                    (Just $ sourceConeAngles src0)
                    (Just $ _sourceState src0)
                    (Just $ sourceGain src0)
                    (Just AL.OneShot)
                    (Just AL.Streaming)
        let ev = if queue /= [] then Event () else NoEvent -- TODO--ev <- edge -< queue /= []
        rSwitch identity -< (src0, tag ev newSource)

-----------------------------------------------------------

-- | Plays the source when the condition is true.
edgePlay :: (a -> Bool) -> SF a Source -> SF a Source
{-# INLINE edgePlay #-}
edgePlay k src =
    switch go \src0 ->
        source_
            (_sourceID src0)
            (_sourceBufferQueue src0)
            (Just $ sourceOffset src0)
            (Just $ sourceGainBounds src0)
            (Just $ sourceConeOuterGain src0)
            (Just $ sourceRelative src0)
            (Just $ sourceRolloffFactor src0)
            (Just $ sourcePitch src0)
            (Just $ sourcePosition src0)
            (Just $ sourceVelocity src0)
            (Just $ sourceDirection src0)
            (Just $ sourceConeAngles src0)
            (Just AL.Playing)
            (Just $ sourceGain src0)
            (Just $ sourceLoopingMode src0)
            (Just $ sourceType src0)
  where
    go = proc a -> do
        s <- src -< a
        e <- edge -< k a
        returnA -< (s, tag e s)

-----------------------------------------------------------

-- | Stops the source when the condition is true.
edgeStop :: (a -> Bool) -> SF a Source -> SF a Source
{-# INLINE edgeStop #-}
edgeStop k src =
    switch go \src0 ->
        source_
            (_sourceID src0)
            (_sourceBufferQueue src0)
            Nothing
            (Just $ sourceGainBounds src0)
            (Just $ sourceConeOuterGain src0)
            (Just $ sourceRelative src0)
            (Just $ sourceRolloffFactor src0)
            (Just $ sourcePitch src0)
            (Just $ sourcePosition src0)
            (Just $ sourceVelocity src0)
            (Just $ sourceDirection src0)
            (Just $ sourceConeAngles src0)
            (Just AL.Stopped)
            (Just $ sourceGain src0)
            (Just $ sourceLoopingMode src0)
            (Just $ sourceType src0)
  where
    go = proc a -> do
        s <- src -< a
        e <- edge -< k a
        returnA -< (s, tag e s)

-----------------------------------------------------------

-- | Pauses the source when the condition is true.
edgePause :: (a -> Bool) -> SF a Source -> SF a Source
{-# INLINE edgePause #-}
edgePause k src =
    switch go \src0 ->
        source_
            (_sourceID src0)
            (_sourceBufferQueue src0)
            (Just $ sourceOffset src0)
            (Just $ sourceGainBounds src0)
            (Just $ sourceConeOuterGain src0)
            (Just $ sourceRelative src0)
            (Just $ sourceRolloffFactor src0)
            (Just $ sourcePitch src0)
            (Just $ sourcePosition src0)
            (Just $ sourceVelocity src0)
            (Just $ sourceDirection src0)
            (Just $ sourceConeAngles src0)
            (Just AL.Paused)
            (Just $ sourceGain src0)
            (Just $ sourceLoopingMode src0)
            (Just $ sourceType src0)
  where
    go = proc a -> do
        s <- src -< a
        e <- edge -< k a
        returnA -< (s, tag e s)

-----------------------------------------------------------

{- | Smart constructor for a sound source. It handles stuff
like keeping the offset correctly when we speed up or slow
down time by changing pitch, for example. Also handles the
movement of the source based on its velocity and has a
bunch of default values as well for the arguments.
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
    Maybe AL.SourceType -> -- source looping mode
    SF a Source
{-# INLINE source_ #-}
source_
    _sourceID
    _sourceBufferQueue
    (fromMaybe {------------} 0 -> sourceStartOffset)
    (fromMaybe {-----} (0.0, 2) -> sourceGainBounds)
    (fromMaybe {----------} 0.5 -> sourceConeOuterGain)
    (fromMaybe {-----} AL.World -> sourceRelative)
    (fromMaybe {------------} 1 -> sourceRolloffFactor)
    (fromMaybe {------------} 1 -> _sourcePitch)
    (fromMaybe {---} (V3 0 0 0) -> position0)
    (fromMaybe {---} (V3 0 0 0) -> sourceVelocity)
    (fromMaybe {---} (V3 0 0 0) -> sourceDirection)
    (fromMaybe {---} (360, 360) -> sourceConeAngles)
    (fromMaybe {---} AL.Initial -> state0)
    (fromMaybe {------------} 1 -> sourceGain)
    (fromMaybe {---} AL.OneShot -> _sourceLoopingMode)
    (fromMaybe {----} AL.Static -> _sourceType) =
        -- TODO add to the arguments
        -- , _sourceChannelType {--------} :: !ChannelType -- immutable. If stereo cannot use positional sound.
        -- , _sourceBufferMagnitude {----} :: !Magnitude -- immutable.
        -- , _sourceBufferSampleRate {---} :: !Float  -- immutable.

        proc a -> do
            t <- Yampa.time -< a
            sourcePosition <- arr (+ position0) <<< Yampa.integral -< sourceVelocity
            let correction s = if s == AL.Playing then 1 else 0
            let state1 = if not (null _sourceBufferQueue) then state0 else AL.Stopped
            returnA
                -<
                    Source
                        { sourceReferenceDistance = 1 -- 1M
                        , sourceMaxDistance = 1_000 -- 1KM
                        , _sourceState = state1
                        , _sourceOffset = if state1 == AL.Stopped then 0 else sourceStartOffset + correction state1 * t * _sourcePitch
                        , _sourceChannelType = Mono -- TODO
                        , _sourceBufferMagnitude = Bit16 -- TODO
                        , _sourceBufferSampleRate = 48_000 -- TODO
                        , _sourceType = AL.Static -- TODO
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
    -- TODO
    -- Undetermined
    -- Static
    -- Streaming
    handleBuffers (_sourceID s1) -- we make changes to the buffers
    handleFields (_sourceID s1) -- we change the source data (pausing if needed, then resuming)
    handleState (_sourceID s1) -- we decide if the source should keep playing or stop or pause
  where
    handleBuffers sid = do
        -- TODO we need a whole set of functionality here to support playing buffers when time is passing backwards
        when (_sourceBufferQueue s0 /= _sourceBufferQueue s1) do
            unless (null (_sourceBufferQueue s1)) do
                -- TODO we also need to consider the state inside the sources
                get (AL.sourceState sid) >>= \case
                    AL.Playing -> do
                        AL.stop [sid]
                        unqueuProcessed
                        AL.queueBuffers sid (_sourceBufferQueue s1)
                    AL.Initial -> do
                        unqueuProcessed
                        unqueueUnprocessed
                        AL.queueBuffers sid (_sourceBufferQueue s1)
                    AL.Paused -> do
                        AL.stop [sid]
                        unqueuProcessed
                        unqueueUnprocessed
                        AL.queueBuffers sid (_sourceBufferQueue s1)
                        AL.pause [sid]
                    AL.Stopped -> do
                        unqueuProcessed
                        unqueueUnprocessed
                        AL.queueBuffers sid (_sourceBufferQueue s1)
                        when (_sourceState s1 == AL.Playing) (AL.play [sid])
      where
        unqueuProcessed = do
            -- TODO only unqueu if sourceType == AL.Streaming
            pn <- get (AL.buffersProcessed sid)
            when (pn > 0) (void (AL.unqueueBuffers sid pn))

        unqueueUnprocessed = do
            -- TODO only unqueu if sourceType == AL.Streaming
            qn <- get (AL.buffersQueued sid)
            when (qn > 0) (AL.buffer sid $= Nothing)

    -- TODO we need a general function that uppon receiving a buffer ,returns its inverse (in a new buffer) copyReversed :: ...etc
    handleFields sid = do
        offset <- get (AL.secOffset sid)
        let diff = realToFrac (_sourceOffset s1) - offset
        if _sourceState s0 == AL.Initial -- TODO || sourceType == AL.Undetermined
            then do
                AL.secOffset sid $= abs (realToFrac (_sourceOffset s1)) -- TODO when playing in reverse we need to adjust the offset so we play in the right place of the reversed buffer
                AL.pitch sid $= realToFrac (abs (_sourcePitch s1))
                AL.coneAngles sid $= (realToFrac $ fst $ sourceConeAngles s1, realToFrac $ snd (sourceConeAngles s1))
                AL.coneOuterGain sid $= sourceConeOuterGain s1
                AL.direction sid $= _v3ToVector (sourceDirection s1)
                AL.gainBounds sid $= (let foox = fst (sourceGainBounds s1) in (foox, snd (sourceGainBounds s1)))
                AL.loopingMode sid $= sourceLoopingMode s1
                AL.maxDistance sid $= realToFrac (sourceMaxDistance s1)
                AL.referenceDistance sid $= realToFrac (sourceReferenceDistance s1)
                AL.rolloffFactor sid $= realToFrac (sourceRolloffFactor s1)
                AL.sourceGain sid $= abs (sourceGain s1)
                AL.sourcePosition sid $= _v3ToVertex (sourcePosition s1)
                AL.sourceRelative sid $= sourceRelative s1
                AL.sourceVelocity sid $= _v3ToVector (sourceVelocity s1)
            else do
                setWhen (AL.secOffset sid) (_sourceState s1 == AL.Initial || diff > 0.01) (abs (realToFrac (_sourceOffset s1))) -- TODO when playing in reverse we need to adjust the offset so we play in the right place of the reversed buffer
                setWhen (AL.pitch sid) (_sourcePitch s1 /= _sourcePitch s0) (realToFrac (abs (_sourcePitch s1)))
                setWhen (AL.coneAngles sid) (sourceConeAngles s1 /= sourceConeAngles s0) (realToFrac $ fst $ sourceConeAngles s1, realToFrac $ snd (sourceConeAngles s1))
                setWhen (AL.coneOuterGain sid) (sourceConeOuterGain s1 /= sourceConeOuterGain s0) (sourceConeOuterGain s1)
                setWhen (AL.direction sid) (sourceDirection s1 /= sourceDirection s0) (_v3ToVector (sourceDirection s1))
                setWhen (AL.gainBounds sid) (sourceGainBounds s1 /= sourceGainBounds s0) (let foox = fst (sourceGainBounds s1) in (foox, snd (sourceGainBounds s1)))
                setWhen (AL.loopingMode sid) (sourceLoopingMode s1 /= sourceLoopingMode s0) (sourceLoopingMode s1)
                setWhen (AL.maxDistance sid) (sourceMaxDistance s1 /= sourceMaxDistance s0) (realToFrac (sourceMaxDistance s1))
                setWhen (AL.referenceDistance sid) (sourceReferenceDistance s1 /= sourceReferenceDistance s0) (realToFrac (sourceReferenceDistance s1))
                setWhen (AL.rolloffFactor sid) (sourceRolloffFactor s1 /= sourceRolloffFactor s0) (realToFrac (sourceRolloffFactor s1))
                setWhen (AL.sourceGain sid) (sourceGain s0 /= sourceGain s1) (abs (sourceGain s1))
                setWhen (AL.sourcePosition sid) (sourcePosition s1 /= sourcePosition s0) (_v3ToVertex (sourcePosition s1))
                setWhen (AL.sourceRelative sid) (sourceRelative s1 /= sourceRelative s0) (sourceRelative s1)
                setWhen (AL.sourceVelocity sid) (sourceVelocity s1 /= sourceVelocity s0) (_v3ToVector (sourceVelocity s1))

    handleState sid = do
        when (notPlaying && _sourceState s1 == AL.Playing) (AL.play [sid])
        when (notPaused && _sourceState s1 == AL.Paused) (AL.pause [sid])
        when (notStopped && _sourceState s1 == AL.Stopped) (AL.stop [sid])
      where
        notPlaying = _sourceState s0 == AL.Initial || _sourceState s0 == AL.Stopped || _sourceState s0 == AL.Paused
        notPaused = _sourceState s0 == AL.Stopped || _sourceState s0 == AL.Initial
        notStopped = _sourceState s0 == AL.Initial || _sourceState s0 == AL.Paused || _sourceState s0 == AL.Playing

setMaxDistance = undefined
setReferenceDistance = undefined
setRolloffFactor = undefined
withState = undefined
withDirection = undefined
withConeAngles = undefined
withConeOuterGain = undefined
