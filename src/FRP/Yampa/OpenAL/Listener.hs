{-# LANGUAGE Arrows #-}

module FRP.Yampa.OpenAL.Listener (
    Listener (..),
    listener,
    listener_,
    setListenerPosition,
    withListenerVelocity,
    withListenerOrientation,
    withListenerGain,
    updateListener,
)
where

import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Maybe
import Data.StateVar (($=))
import FRP.Yampa (SF, arr, constant, edge, identity, rSwitch, tag, (&&&), (<<<), (>>>))
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

-- | Smart constructor for the listener. It handles the movement.
listener_ ::
    Maybe (V3 Float) ->
    Maybe (V3 Float) ->
    Maybe (V3 Float, V3 Float) ->
    Maybe AL.Gain ->
    SF a Listener
{-# INLINE listener_ #-}
listener_ mx0 mv0 mori0 mgain0 = do
    let v0 = fromMaybe 0 mv0
    let x0 = fromMaybe 0 mx0
    let o0 = fromMaybe (V3 0 (-1) 0, V3 0 0 1) mori0
    let g0 = fromMaybe 1 mgain0
    Listener
        <$> (arr (+ x0) <<< Yampa.integral <<< constant v0)
        <*> constant v0
        <*> constant o0
        <*> constant g0

-----------------------------------------------------------
setListenerPosition ::
    V3 Float ->
    SF a Listener ->
    SF a Listener
{-# INLINE setListenerPosition #-}
setListenerPosition pos lis =
    (constant pos &&& lis) >>> arr (\(x, l) -> l{listenerPosition = x})

-----------------------------------------------------------
withListenerVelocity ::
    SF a (V3 Float) ->
    SF a Listener ->
    SF a Listener
{-# INLINE withListenerVelocity #-}
withListenerVelocity vel lis =
    (lis &&& vel) >>> proc (lis0, vel0) -> do
        let newSource =
                listener_
                    (Just (listenerPosition lis0))
                    (Just vel0)
                    (Just (listenerOrientation lis0))
                    (Just (listenerGain lis0))
        ev <- edge -< vel0 /= listenerVelocity lis0
        rSwitch identity -< (lis0, tag ev newSource)

-----------------------------------------------------------
withListenerOrientation ::
    SF a (V3 Float, V3 Float) ->
    SF a Listener ->
    SF a Listener
{-# INLINE withListenerOrientation #-}
withListenerOrientation ori lis =
    (ori &&& lis) >>> arr (\(o, l) -> l{listenerOrientation = o})

-----------------------------------------------------------
withListenerGain ::
    SF a AL.Gain ->
    SF a Listener ->
    SF a Listener
{-# INLINE withListenerGain #-}
withListenerGain gain lis =
    (gain &&& lis) >>> arr (\(g, l) -> l{listenerGain = g})

-----------------------------------------------------------

-- | Updates only what have changed for the Listener.
updateListener :: (MonadIO m) => Maybe Listener -> Listener -> m ()
{-# INLINEABLE updateListener #-}
updateListener mlistener0 listener1 =
    liftIO $ do
        case mlistener0 of
            -- if the first execution we updated everything
            Nothing -> do
                AL.listenerPosition $= _v3ToVertex (listenerPosition listener1)
                AL.listenerVelocity $= _v3ToVector (listenerVelocity listener1)
                AL.orientation $= bimap _v3ToVector _v3ToVector (listenerOrientation listener1)
                AL.listenerGain $= abs (listenerGain listener1)
            -- otherwise we update only what changed
            Just listener0 -> do
                setWhen AL.listenerPosition (listenerPosition listener1 /= listenerPosition listener0) (_v3ToVertex (listenerPosition listener1))
                setWhen AL.listenerVelocity (listenerVelocity listener1 /= listenerVelocity listener0) (_v3ToVector (listenerVelocity listener1))
                setWhen AL.orientation (listenerOrientation listener1 /= listenerOrientation listener0) (bimap _v3ToVector _v3ToVector (listenerOrientation listener1))
                setWhen AL.listenerGain (listenerGain listener1 /= listenerGain listener0) (abs (listenerGain listener1))
