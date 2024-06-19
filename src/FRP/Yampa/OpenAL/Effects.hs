{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module FRP.Yampa.OpenAL.Effects where

import FRP.Yampa (Time, ReactHandle, SF, (<<<), arr, returnA)
import qualified FRP.Yampa as Yampa
import FRP.Yampa.OpenAL.Types
import qualified Sound.OpenAL as AL


-----------------------------------------------------------
tremolo :: Depth -> Rate -> SF a AL.Gain
tremolo depth hz = proc a -> do
    deltaGain <- arr (sin . (* hz)) <<< Yampa.time -< a
    returnA -< abs (realToFrac $ deltaGain * depth)

-----------------------------------------------------------
vibrato :: Depth -> Rate -> SF a Pitch
vibrato depth hz = proc a -> do
    deltaPitch <- arr (sin . (* hz)) <<< Yampa.time -< a
    returnA -< abs (realToFrac $ deltaPitch * depth)


-----------------------------------------------------------
fadeIn :: Time -> AL.Gain -> SF a AL.Gain
fadeIn _ _ = pure 0


-----------------------------------------------------------
fadeOut :: Time -> AL.Gain -> SF a AL.Gain
fadeOut _ _ = undefined

-----------------------------------------------------------
average :: (Fractional n) => [SF a n] -> SF a n
average sfs = proc a -> do
    ns <- Yampa.parB sfs -< a
    returnA -< if null ns then 0 else sum ns / (realToFrac (length ns))
