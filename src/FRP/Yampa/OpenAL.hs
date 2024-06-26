{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module FRP.Yampa.OpenAL (
    module FRP.Yampa.OpenAL,
    module FRP.Yampa.OpenAL.IO,
)
where

import qualified Data.Map as Map
import Data.Maybe
import FRP.Yampa
import qualified FRP.Yampa as Yampa
import FRP.Yampa.OpenAL.IO
import FRP.Yampa.OpenAL.Types
import FRP.Yampa.OpenAL.Util
import Linear.V3
import qualified Sound.OpenAL as AL

{-
-------------------------------------------------------------------------------
-- CONSTRUCTORS ---------------------------------------------------------------
-------------------------------------------------------------------------------

-----------------------------------------------------------

-- | Constructor with default values.
source ::
    String ->
    Buffer ->
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
        Nothing
        Nothing

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
getState = arr (fromMaybe Stopped . sourceState)

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
getConeOuterGain = arr sourceConeOuterGain

-----------------------------------------------------------
getStartOffset :: SF Source Double
getStartOffset = arr sourceStartOffset

-----------------------------------------------------------
getOffset :: SF Source Double
getOffset = arr sourceOffset

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
                    (sourceBuffer src0)
                    (Just $ sourceOffset src0)
                    (Just (sourceGainBounds src0))
                    (Just (sourceConeOuterGain src0))
                    (Just (sourceRelative src0))
                    (Just $ sourceRolloffFactor src0)
                    (Just (sourcePitch src0))
                    (Just $ sourcePosition src0)
                    (Just $ sourceVelocity src0)
                    (Just $ sourceDirection src0)
                    (Just $ sourceConeAngles src0)
                    (sourceState src0)
                    (Just $ realToFrac gain)
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
                    (sourceBuffer src0)
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
                    (sourceState src0)
                    (Just $ sourceGain src0)
        rSwitch identity -< (src0, tag pitchChanged newSource)

-----------------------------------------------------------
setState :: SF a AL.SourceState -> SF a Source -> SF a Source
setState stateSF src =
    (src &&& stateSF) >>> proc (src0, state1) -> do
        stateChanged <- edge -< Just state1 /= sourceState src0
        let newSource =
                source_
                    (sourceID src0)
                    (sourceBuffer src0)
                    (Just $ sourceOffset src0)
                    (Just (sourceGainBounds src0))
                    (Just (sourceConeOuterGain src0))
                    (Just (sourceRelative src0))
                    (Just $ sourceRolloffFactor src0)
                    (Just $ sourcePitch src0)
                    (Just $ sourcePosition src0)
                    (Just $ sourceVelocity src0)
                    (Just $ sourceDirection src0)
                    (Just $ sourceConeAngles src0)
                    (Just state1)
                    (Just $ sourceGain src0)
        rSwitch identity -< (src0, tag stateChanged newSource)

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

{- | Smart constructor for a sound source. It handles stuff
like keeping the offset correctly when we speed up or slow
down time by changing pitch, for example. Also handles the
movement of the source based on its velocity.
-}
source_ ::
    String -> -- name
    Buffer -> -- queue
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
    SF a Source
{-# INLINE source_ #-}
source_
    sourceID
    sourceBuffer
    (fromMaybe 0 -> sourceStartOffset)
    (fromMaybe (0.2, 2) -> sourceGainBounds)
    (fromMaybe 0.01 -> sourceConeOuterGain)
    (fromMaybe AL.World -> sourceRelative)
    (fromMaybe 1 -> sourceRolloffFactor)
    (fromMaybe 1 -> sourcePitch)
    (fromMaybe 0 -> position0)
    (fromMaybe 0 -> sourceVelocity)
    (fromMaybe 0 -> sourceDirection)
    (fromMaybe (360,360) -> sourceConeAngles)
    (fromMaybe AL.Initial -> state0)
    (fromMaybe 1 -> sourceGain) =
        proc a -> do
            t <- Yampa.time -< a
            dx <- Yampa.integral -< sourceVelocity
            state1 <- initially Nothing -< Just state0
            let correction s = if s == Just AL.Playing then 1 else 0
            returnA
                -<
                    Source
                        { sourceLoopingMode = OneShot
                        , sourceReferenceDistance = 1
                        , sourceMaxDistance = 1_000 -- 1KM
                        , sourceState = state1
                        , sourceOffset = sourceStartOffset + (correction state1 * t * sourcePitch)
                        , sourcePosition = position0 + dx
                        , ..
                        }
                        -}
