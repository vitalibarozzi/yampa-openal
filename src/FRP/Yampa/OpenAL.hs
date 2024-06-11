{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FRP.Yampa.OpenAL where

import qualified Data.Map as Map
import FRP.Yampa
import qualified FRP.Yampa as Yampa
import FRP.Yampa.OpenAL.Types
import Linear.V3
import qualified Sound.OpenAL as AL

-------------------------------------------------------------------------------
-- CONSTRUCTORS ---------------------------------------------------------------
-------------------------------------------------------------------------------

-----------------------------------------------------------

-- | Constructor with default values.
source ::
    String ->
    [Buffer] ->
    SF a Source
{-# INLINE source #-}
source name queue =
    source_
        name
        queue
        0 -- startAt
        0 -- pitch
        0 -- vel
        0 -- pos
        0 -- ori
        AL.Playing

-----------------------------------------------------------

-- | Constructor with default values from a position.
listener :: V3 Float -> SF a Listener
listener pos =
    listener_ pos 0 (V3 0 (-1) 0, V3 0 0 1) 1

-----------------------------------------------------------

{- | Constructor with default values. To be used when you
 - don't need to change the sources signals of the soundstage
 - during its execution.
-}
soundstage :: [SF a Source] -> SF a Soundstage
{-# INLINE soundstage #-}
soundstage sources = proc a -> do
    ev <- initially (Event (const sources)) -< NoEvent
    soundstage_ 1 AL.InverseDistance 0 0 1 -< (a, ev)

-------------------------------------------------------------------------------
-- LISTENER MODIFIERS ---------------------------------------------------------
-------------------------------------------------------------------------------

-----------------------------------------------------------
withListenerPosition :: SF (Listener, Event (V3 Float)) Listener
{-# INLINE withListenerPosition #-}
withListenerPosition = proc (l, evpos) -> do
    let newListener = listener_ (fromEvent evpos) (listenerVelocity l) (listenerOrientation l) (listenerGain l)
    let payload = (,) l (tag evpos newListener)
    rSwitch identity -< payload

-----------------------------------------------------------
withListenerVelocity :: SF (Listener, V3 Float) Listener
{-# INLINE withListenerVelocity #-}
withListenerVelocity = proc (l, v1) -> do
    let newListener = listener_ (listenerPosition l) v1 (listenerOrientation l) (listenerGain l)
    let velocityChanged = if v1 /= listenerVelocity l then Event () else NoEvent
    let payload = (,) l (tag velocityChanged newListener)
    rSwitch identity -< payload

-----------------------------------------------------------
withListenerOrientation :: SF (Listener, (V3 Float, V3 Float)) Listener
{-# INLINE withListenerOrientation #-}
withListenerOrientation = proc (l, ori) -> do
    let newListener = listener_ (listenerPosition l) (listenerVelocity l) ori (listenerGain l)
    let velocityChanged = if ori /= listenerOrientation l then Event () else NoEvent
    let payload = (,) l (tag velocityChanged newListener)
    rSwitch identity -< payload

-----------------------------------------------------------
withListenerGain :: SF (Listener, Float) Listener
{-# INLINE withListenerGain #-}
withListenerGain = proc (l, gain) -> do
    let newListener = listener_ (listenerPosition l) (listenerVelocity l) (listenerOrientation l) gain
    let velocityChanged = if gain /= listenerGain l then Event () else NoEvent
    let payload = (,) l (tag velocityChanged newListener)
    rSwitch identity -< payload

-------------------------------------------------------------------------------
-- SOURCE MODIFIERS -----------------------------------------------------------
-------------------------------------------------------------------------------

-- TODO
--sourcePosition {-----------} :: !(V3 Meters)
--sourceVelocity {-----------} :: !(V3 MetersPerSecond)
--sourceDirection {----------} :: !(V3 Meters)
--sourceGain {---------------} :: !Gain
--sourceConeAngles {---------} :: !(Angle, Angle) -- Outer cone, inner cone, in degrees.
--sourceConeOuterGain {------} :: !Gain -- Gain outside the cone.
--sourceRolloffFactor {------} :: !Factor
--sourceStartOffset {--------} :: !Float
--sourceOffset {-------------} :: !Float

-----------------------------------------------------------

{- | Modify the pitch and adjusts the offset of the source
accordingly. If pitch is modified separatly from the offset
they will get desynchronized as the the actual sound in IO
will be running faster or slower in relation to the source.
-}
withPitch :: SF (Source, Pitch) Source
{-# INLINE withPitch #-}
withPitch = proc (src0, pitch) -> do
    let newSource =
            source_
                (sourceID src0)
                (sourceBufferQueue src0)
                (realToFrac $ sourceOffset src0)
                pitch
                (sourcePosition src0)
                (sourceVelocity src0)
                (sourceDirection src0)
                (sourceState src0)
    let pitchChanged = if pitch /= sourcePitch src0 then Event () else NoEvent
    let payload = (,) src0 (tag pitchChanged newSource)
    rSwitch identity -< payload

-----------------------------------------------------------
withState :: SF (Source, Event AL.SourceState) Source
{-# INLINE withState #-}
withState = proc (src0, evstate) -> do
    let newSource =
            source_
                (sourceID src0)
                (sourceBufferQueue src0)
                (realToFrac (sourceOffset src0))
                (sourcePitch src0)
                (sourcePosition src0)
                (sourceVelocity src0)
                (sourceDirection src0)
                (fromEvent evstate)
    let payload = (,) src0 (tag evstate newSource)
    rSwitch identity -< payload

-------------------------------------------------------------------------------
-- ADVANCED API ---------------------------------------------------------------
-------------------------------------------------------------------------------

-----------------------------------------------------------
source_ ::
    String ->
    [Buffer] ->
    DTime ->
    Double ->
    V3 Float ->
    V3 Float ->
    V3 Float ->
    AL.SourceState ->
    SF a Source
{-# INLINE source_ #-}
source_ name queue startAt pitch position0 velocity0 direction state = proc a -> do
    t <- Yampa.time -< a
    dx <- Yampa.integral -< velocity0
    returnA
        -<
            Source
                { sourceID = name
                , sourcePosition = position0 + dx
                , sourceVelocity = velocity0
                , sourceDirection = direction
                , sourceGain = 1 -- TODO
                , sourcePitch = pitch
                , sourceGainBounds = (0, 1)
                , sourceConeAngles = (360, 360) -- TODO -- sounds go in all directions
                , sourceConeOuterGain = 0 -- TODO -- we hear nothing outside the cone
                , sourceRelative = AL.World
                , sourceRolloffFactor = 1
                , sourceReferenceDistance = 1
                , sourceMaxDistance = 10_000 -- try some others
                , sourceLoopingMode = OneShot -- TODO
                , sourceState = state
                , sourceBufferQueue = queue
                , sourceStartOffset = realToFrac startAt
                , sourceOffset = realToFrac (startAt + (correction * t * pitch))
                }
  where
    correction = if state == AL.Playing then 1 else 0

-----------------------------------------------------------
listener_ :: V3 Float -> V3 Float -> (V3 Float, V3 Float) -> Float -> SF a Listener
{-# INLINE listener_ #-}
listener_ x0 v0 ori0 gain0 = proc a -> do
    dx <- Yampa.integral -< v0
    returnA -< Listener_ (x0 + dx) v0 ori0 gain0

-----------------------------------------------------------
-- For when you want to change the collection of source
-- signals of the soundstage at some point.
soundstage_ ::
    Float ->
    AL.DistanceModel ->
    V3 Float ->
    V3 Float ->
    Double ->
    SF (a, Event ([SourceSignal a] -> [SourceSignal a])) Soundstage
{-# INLINE soundstage_ #-}
soundstage_ factor model pos vel gain =
    Soundstage
        <$> fmap (Map.fromList . fmap (\x -> (sourceID x, x))) (drpSwitchB [])
        <*> pure factor
        <*> pure 343.3
        <*> pure model
        -- TODO listener stuff
        <*> constant pos -- TODO should change with the velocity
        <*> constant vel
        <*> constant (V3 0 0 (-1), V3 0 1 0)
        <*> constant gain
