{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module FRP.Yampa.OpenAL.Listener (
    Listener (..),
    listener,
    listener_,
    setListenerPosition,
    withListenerVelocity,
    updateListener,
)
where

import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Maybe
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

-- | Constructor with default values.
listener :: SF a Listener
{-# INLINE listener #-}
listener =
    listener_
        Nothing
        Nothing
        Nothing
        Nothing

-----------------------------------------------------------

{- | Smart constructor for the listener. It handles the movement.
-}
listener_ ::
    Maybe (V3 Float) ->
    Maybe (V3 Float) ->
    Maybe (V3 Float, V3 Float) ->
    Maybe Float ->
    SF a Listener
{-# INLINE listener_ #-}
listener_ mx0 mv0 mori0 mgain0 = proc _ -> do
    let x0 = fromMaybe 0 mx0
    let v0 = fromMaybe 0 mv0
    let ori0 = fromMaybe (V3 0 (-1) 0, V3 0 0 1) mori0
    let gain0 = fromMaybe 1 mgain0
    dx <- Yampa.integral -< v0
    returnA -< Listener (x0 + dx) v0 ori0 (realToFrac gain0)

-----------------------------------------------------------
setListenerPosition ::
    V3 Float ->
    SF a Listener ->
    SF a Listener
{-# INLINE setListenerPosition #-}
setListenerPosition =
    undefined -- TODO

-----------------------------------------------------------
withListenerVelocity ::
    SF a (V3 Float) ->
    SF a Listener ->
    SF a Listener
{-# INLINE withListenerVelocity #-}
withListenerVelocity =
    undefined -- TODO

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
