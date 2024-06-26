{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module FRP.Yampa.OpenAL.Listener
where

import FRP.Yampa (SF, returnA)
import qualified FRP.Yampa as Yampa
import FRP.Yampa.OpenAL.Source ()
import FRP.Yampa.OpenAL.Types (Gain)
import Linear as L (V3 (..))

-----------------------------------------------------------
data Listener = Listener_
    { listenerPosition {-----} :: !(V3 Float) -- meters
    , listenerVelocity {-----} :: !(V3 Float) -- meters per second
    , listenerOrientation {--} :: !(V3 Float, V3 Float) -- meters
    , listenerGain {---------} :: !Gain
    }
    deriving
        (Eq, Show)

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
