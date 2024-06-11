{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | Effects implemented without processing the buffers directly.
module FRP.Yampa.OpenAL.Effects where

import FRP.Yampa.OpenAL
import FRP.Yampa.OpenAL.Types
import qualified Data.Map as Map
import FRP.Yampa
import qualified FRP.Yampa as Yampa


{-
-----------------------------------------------------------

-- | Sine variations of gain along time.
tremolo :: Depth -> Rate -> SF Source Source
{-# INLINE tremolo #-}
tremolo (abs -> depth) (abs -> hz) = proc s -> do
    deltaGain <- arr (sin . (* hz)) <<< Yampa.time -< s
    returnA -< s{sourceGain = abs (sourceGain s + (deltaGain * depth))}

-----------------------------------------------------------

-- | Sine variations of pitch along time.
vibrato :: Depth -> Rate -> SF Source Source
{-# INLINE vibrato #-}
vibrato (abs -> depth) (abs -> hz) = proc src -> do
    t <- Yampa.time -< src
    deltaPitch <- arr (sin . (* hz)) -< t
    returnA -< src{sourceProperTime = t + (undefined deltaPitch * depth)}

-----------------------------------------------------------
-- |
reverb :: () -> () -> SF Source SourceGroup
reverb feedback decay = proc src -> do
    -- TODO
    returnA -< Map.fromList [(1, src)]


-------------------
-------------------
-------------------
-------------------
-------------------
-------------------
-------------------


-----------------------------------------------------------

{- | Changes the pitch by changing how fast the time passes
for this source in relation to the soundscape.
-}
pitchShift :: Double -> SF Source Source
pitchShift amount = proc src -> do
    returnA -< src{sourceProperTime = sourceProperTime src * abs amount}

-----------------------------------------------------------
-- |
fadeIn :: Depth -> Seconds -> SF Source Source
{-# INLINE fadeIn #-}
fadeIn (abs -> depth) ss = proc s -> do
    t <- Yampa.time -< ()
    let xt = t - ss
    if xt < 0
        then returnA -< s
        else returnA -< s{sourceGain = abs (sourceGain s / (depth / xt))}

-----------------------------------------------------------
slideIn :: Depth -> Seconds -> SF Source Source
{-# INLINE slideIn #-}
slideIn (abs -> depth) ss = proc s -> do
    t <- Yampa.time -< ()
    let xt = t / ss
    if xt >= 1
        then returnA -< s
        else returnA -< undefined -- s{sourcePitch = abs (clamp01 (sourcePitch s * ((xt ^ 2) / depth)))}
  where
    clamp01 n = if n <= 0.2 then 0.2 else min n 1
    -}
