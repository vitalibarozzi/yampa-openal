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
-- TODO add export listt
where

import Control.Monad.IO.Class
import Data.Bifunctor
import Data.StateVar (($=))
import FRP.Yampa (SF, returnA)
import qualified FRP.Yampa as Yampa
import FRP.Yampa.OpenAL.Source ()
import FRP.Yampa.OpenAL.Util
import Linear as L (V3 (..))
import qualified Sound.OpenAL.AL as AL

-----------------------------------------------------------
data Listener = Listener
    { listenerPosition {-----} :: !(V3 Float) -- meters
    , listenerVelocity {-----} :: !(V3 Float) -- meters per second
    , listenerOrientation {--} :: !(V3 Float, V3 Float) -- meters
    , listenerGain {---------} :: !AL.Gain
    }
    deriving
        (Eq, Show)

-----------------------------------------------------------

-- | Constructor with default values from a position.
listener :: V3 Float -> SF a Listener
{-# INLINE listener #-}
listener pos =
    listener_
        pos
        0 -- TODO
        (V3 0 (-1) 0, V3 0 0 1) -- TODO
        1 -- TODO

-----------------------------------------------------------

-- | Smart constructor for the listener. It handles the movement.
-- TODO add maybes and default values
listener_ ::
    V3 Float ->
    V3 Float ->
    (V3 Float, V3 Float) ->
    Float ->
    SF a Listener
{-# INLINE listener_ #-}
listener_ x0 v0 ori0 gain0 = proc _ -> do
    dx <- Yampa.integral -< v0
    returnA -< Listener (x0 + dx) v0 ori0 (realToFrac gain0)

-----------------------------------------------------------
setListenerPosition ::
    V3 Float ->
    SF a Listener ->
    SF a Listener
setListenerPosition =
    undefined

-----------------------------------------------------------
withListenerVelocity ::
    SF a (V3 Float) ->
    SF a Listener ->
    SF a Listener
withListenerVelocity =
    undefined

-----------------------------------------------------------
updateListener :: (MonadIO m) => Maybe Listener -> Listener -> m ()
{-# INLINE updateListener #-}
updateListener mlistener0 listener1 = do
    liftIO do
        case mlistener0 of
            Nothing -> do
                ($=) AL.listenerPosition (_v3ToVertex (listenerPosition listener1))
                ($=) AL.listenerVelocity (_v3ToVector (listenerVelocity listener1))
                ($=) AL.orientation (bimap _v3ToVector _v3ToVector (listenerOrientation listener1))
                ($=) AL.listenerGain (abs (listenerGain listener1))
            Just _listener0 -> do
                ($=?) AL.listenerPosition True (_v3ToVertex (listenerPosition listener1)) -- TODO check difference
                ($=?) AL.listenerVelocity True (_v3ToVector (listenerVelocity listener1)) -- TODO check difference
                ($=?) AL.orientation True (bimap _v3ToVector _v3ToVector (listenerOrientation listener1)) -- TODO check difference
                ($=?) AL.listenerGain True (abs (listenerGain listener1)) -- TODO check difference
