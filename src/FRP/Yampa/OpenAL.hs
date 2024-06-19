{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module FRP.Yampa.OpenAL (
    module FRP.Yampa.OpenAL,
    module FRP.Yampa.OpenAL.IO,
)
where

import qualified Data.Map as Map
import FRP.Yampa
import qualified FRP.Yampa as Yampa
import FRP.Yampa.OpenAL.IO
import FRP.Yampa.OpenAL.Types
import FRP.Yampa.OpenAL.Util
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
        (0.2, 1.2) -- Gain bounds
        AL.World -- SourceRelative
        1 -- RolloffFactor
        0 -- vel
        1 -- pitch
        0 -- pos
        0 -- ori
        (360, 360)
        AL.Playing -- State
        1 -- Gain

-----------------------------------------------------------

-- | Constructor with default values from a position.
listener :: V3 Float -> SF a Listener
listener pos =
    listener_
        pos
        0
        (V3 0 (-1) 0, V3 0 0 1)
        1

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
-- LISTENER GETTERS -----------------------------------------------------------
-------------------------------------------------------------------------------

-----------------------------------------------------------
getListenerPosition :: SF Listener (V3 Double)
getListenerPosition = arr undefined

-----------------------------------------------------------
getListenerVelocity :: SF Listener (V3 Double)
getListenerVelocity = undefined

-----------------------------------------------------------
getListenerOrientation :: SF Listener (V3 Double)
getListenerOrientation = undefined

-----------------------------------------------------------
getListenerGain :: SF Listener AL.Gain
getListenerGain = undefined

-------------------------------------------------------------------------------
-- LISTENER SETTERS -----------------------------------------------------------
-------------------------------------------------------------------------------

{-
-----------------------------------------------------------
setListenerPosition =
    undefined
  where
    withListenerPosition :: SF (Listener, Event (V3 Float)) Listener
    {-# INLINE withListenerPosition #-}
    withListenerPosition = proc (l, evpos) -> do
        let newListener = listener_ (fromEvent evpos) (listenerVelocity l) (listenerOrientation l) (listenerGain l)
        let payload = (,) l (tag evpos newListener)
        rSwitch identity -< payload

-----------------------------------------------------------
setListenerVelocity =
    undefined
  where
    withListenerVelocity :: SF (Listener, V3 Float) Listener
    {-# INLINE withListenerVelocity #-}
    withListenerVelocity = proc (l, v1) -> do
        -- TODO maybe dont needs switch
        let newListener = listener_ (listenerPosition l) v1 (listenerOrientation l) (listenerGain l)
        let velocityChanged = if v1 /= listenerVelocity l then Event () else NoEvent
        let payload = (,) l (tag velocityChanged newListener)
        rSwitch identity -< payload

-----------------------------------------------------------
withListenerOrientation :: SF (Listener, (V3 Float, V3 Float)) Listener
{-# INLINE withListenerOrientation #-}
withListenerOrientation = proc (l, ori) -> do
    -- TODO maybe dont needs switch
    let newListener = listener_ (listenerPosition l) (listenerVelocity l) ori (listenerGain l)
    let velocityChanged = if ori /= listenerOrientation l then Event () else NoEvent
    let payload = (,) l (tag velocityChanged newListener)
    rSwitch identity -< payload

-----------------------------------------------------------
withListenerGain :: SF (Listener, Float) Listener
{-# INLINE withListenerGain #-}
withListenerGain = proc (l, gain) -> do
    -- TODO maybe dont needs switch
    let newListener = listener_ (listenerPosition l) (listenerVelocity l) (listenerOrientation l) gain
    let velocityChanged = if gain /= listenerGain l then Event () else NoEvent
    let payload = (,) l (tag velocityChanged newListener)
    rSwitch identity -< payload
-}
-------------------------------------------------------------------------------
-- SOURCE GETTERS -------------------------------------------------------------
-------------------------------------------------------------------------------

-----------------------------------------------------------
getGain :: SF Source AL.Gain
getGain = arr (realToFrac . sourceGain)

-----------------------------------------------------------
getPitch :: SF Source Pitch
getPitch = arr sourcePitch

-----------------------------------------------------------
getState :: SF Source AL.SourceState
getState = arr sourceState

-----------------------------------------------------------
getPosition :: SF Source (V3 Float)
getPosition = arr sourcePosition

-----------------------------------------------------------
getVelocity :: SF Source (V3 Float)
getVelocity = arr sourceVelocity

-----------------------------------------------------------
getDirection :: SF Source (V3 Float)
getDirection = arr sourceDirection

-----------------------------------------------------------
getConeAngles :: SF Source (Double, Double)
getConeAngles = arr sourceConeAngles

-----------------------------------------------------------
getConeOuterGain :: SF Source AL.Gain
getConeOuterGain = arr (realToFrac . sourceConeOuterGain)

-----------------------------------------------------------
getStartOffset :: SF Source Double
getStartOffset = arr (realToFrac . sourceStartOffset)

-----------------------------------------------------------
getOffset :: SF Source Double
getOffset = arr (realToFrac . sourceOffset)

-------------------------------------------------------------------------------
-- SOURCE SETTERS -----------------------------------------------------------
-------------------------------------------------------------------------------

-----------------------------------------------------------
setGain :: SF a AL.Gain -> SF a Source -> SF a Source
setGain gainSF src =
    -- TODO better returnA -< src0 { sourceGain = realToFrac gain }
    (src &&& gainSF) >>> proc (src0, gain) -> do
        let gainChanged = gain /= realToFrac (sourceGain src0)
        let newSource =
                source_
                    (sourceID src0)
                    (sourceBufferQueue src0)
                    (realToFrac $ sourceOffset src0)
                    (sourceGainBounds src0)
                    (sourceRelative src0)
                    (realToFrac (sourceRolloffFactor src0))
                    (sourcePosition src0)
                    (sourcePitch src0)
                    (sourceVelocity src0)
                    (sourceDirection src0)
                    (sourceConeAngles src0)
                    (sourceState src0)
                    (realToFrac gain)
        rSwitch identity -< (src0, tag (if gainChanged then Event () else NoEvent) newSource)

-----------------------------------------------------------
-- \| Modify the pitch and adjusts the offset of the source
--    accordingly. If pitch is modified separatly from the offset
--    they will get desynchronized as the the actual sound in IO
--    will be running faster or slower in relation to the source.
--    Thats why a switch is needed.
setPitch :: SF a Pitch -> SF a Source -> SF a Source
setPitch pitchSF src = do
    (src &&& pitchSF) >>> proc (src0, pitch) -> do
        pitchChanged <- iEdge False -< pitch /= sourcePitch src0
        let newSource =
                source_
                    (sourceID src0)
                    (sourceBufferQueue src0)
                    (realToFrac $ sourceOffset src0)
                    (sourceGainBounds src0)
                    (sourceRelative src0)
                    (realToFrac (sourceRolloffFactor src0))
                    (sourcePosition src0)
                    pitch
                    (sourceVelocity src0)
                    (sourceDirection src0)
                    (sourceConeAngles src0)
                    (sourceState src0)
                    (sourceGain src0)
        rSwitch identity -< (src0, tag pitchChanged newSource)

-----------------------------------------------------------
setState :: SF a AL.SourceState -> SF a Source -> SF a Source
setState stateSF src =
    (src &&& stateSF) >>> proc (src1, state1) -> do
        stateChanged <- edge -< state1 /= sourceState src1
        let newSource =
                source_
                    (sourceID src1)
                    (sourceBufferQueue src1)
                    (realToFrac (sourceOffset src1))
                    (sourceGainBounds src1)
                    (sourceRelative src1)
                    (realToFrac (sourceRolloffFactor src1))
                    (sourcePosition src1)
                    (sourcePitch src1)
                    (sourceVelocity src1)
                    (sourceDirection src1)
                    (sourceConeAngles src1)
                    state1
                    (sourceGain src1)
        rSwitch identity -< (src1, tag stateChanged newSource)

-----------------------------------------------------------
setPosition :: (V3 Double)
setPosition = undefined

-----------------------------------------------------------
setVelocity :: (V3 Double)
setVelocity = undefined

-----------------------------------------------------------
setDirection :: (V3 Double)
setDirection = undefined

-----------------------------------------------------------
setConeAngles :: (Double, Double) -- Outer cone, inner cone, in degrees.
setConeAngles = undefined

-----------------------------------------------------------
setConeOuterGain :: AL.Gain -- Gain outside the cone.
setConeOuterGain = undefined

-----------------------------------------------------------
setStartOffset :: Double
setStartOffset = undefined

-------------------------------------------------------------------------------
-- ADVANCED API ---------------------------------------------------------------
-------------------------------------------------------------------------------

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
    -- initialSources =
    Soundstage
        <$> fmap (Map.fromList . fmap (\x -> (sourceID x, x))) (drpSwitchB [])
        <*> pure factor
        <*> pure 343.3
        <*> pure model
        <*> listener_ pos vel (V3 0 0 (-1), V3 0 1 0) (realToFrac gain)

-----------------------------------------------------------

-- | Smart constructor for the listener. It handles the movement.
listener_ ::
    V3 Float ->
    V3 Float ->
    (V3 Float, V3 Float) ->
    Float ->
    SF a Listener
{-# INLINE listener_ #-}
listener_ x0 v0 ori0 gain0 = proc _ -> do
    dx <- Yampa.integral -< v0
    returnA -< Listener_ (x0 + dx) v0 ori0 (realToFrac gain0)

-----------------------------------------------------------

{- | Smart constructor for a sound source. It handles stuff
like keeping the offset correctly when we speed up or slow
down time by changing pitch, for example. Also handles the
movement of the source based on its velocity.
-}
source_ ::
    -- statics
    String -> -- name
    Buffer -> -- queue
    DTime -> -- start at
    (Double, Double) ->
    AL.SourceRelative ->
    Double -> -- rolloff
    V3 Float -> -- position
    -- dynamics
    Pitch -> -- pitch
    V3 Float -> -- velocity -- TODO should be an sf
    V3 Float -> -- direction -- TODO should be an sf
    (Double, Double) -> -- cone angles -- TODO sf
    AL.SourceState -> -- source state -- TODO sf
    Double -> -- source gain -- TODO sf
    SF a Source
{-# INLINE source_ #-}
source_ name queue startAt gainBounds relative rolloff position0 pitch0 velocity0 direction angles state0 gain = proc a -> do
    t <- Yampa.time -< a
    -- calculate the change in position
    dx <- Yampa.integral -< velocity0
    -- Stop at end of offset.
    state1 <- Yampa.delay 0 state0 -< state0
    returnA
        -<
            Source
                { sourceID = name
                , sourceBufferQueue = queue
                , sourceLoopingMode = OneShot -- TODO  can this be the cause of not playing all the buffers? (Maybe)
                , sourceGainBounds = gainBounds -- (0.2, 2)
                , sourceRelative = relative -- AL.World
                , sourceReferenceDistance = 1
                , sourceMaxDistance = 1_000 -- 1KM
                , sourceRolloffFactor = realToFrac rolloff
                , sourceConeOuterGain = 0.01
                , sourceConeAngles = angles -- (360, 360)
                , sourceState = state1
                , sourcePitch = pitch0
                , sourceStartOffset = realToFrac startAt
                , sourceOffset = realToFrac (startAt + (correction state1 * t * pitch0))
                , sourcePosition = position0 + dx
                , sourceVelocity = velocity0
                , sourceDirection = direction
                , sourceGain = gain
                }
  where
    correction s = if s == AL.Playing then 1 else 0
