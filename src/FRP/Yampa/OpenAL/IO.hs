{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
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

module FRP.Yampa.OpenAL.IO where

import FRP.Yampa.OpenAL.IO.Update
import FRP.Yampa.OpenAL.IO.Update
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import FRP.Yampa (ReactHandle, SF)
import qualified FRP.Yampa as Yampa
import FRP.Yampa.OpenAL.Types
import FRP.Yampa.OpenAL.Util
import qualified Sound.ALUT.Initialization as AL
import qualified Sound.OpenAL as AL
import FRP.Yampa.OpenAL.Source
import Data.Map (Map)
import FRP.Yampa
import Linear as L
import qualified Sound.ALUT.Loaders as AL
import qualified Sound.OpenAL as AL
import qualified Sound.OpenAL.AL.Buffer as AL

-----------------------------------------------------------

{- | A model of the how the sound elements are defined at
one specific point in time.
-}
data Soundstage = Soundstage
    { soundstageSources {--------} :: !(Map String Source)
    , soundstageDopplerFactor {--} :: !Factor
    , soundstageSpeedOfSound {---} :: !MetersPerSecond
    , soundstageDistanceModel {--} :: !AL.DistanceModel
    , soundstageListener {-------} :: !Listener
    , soundstageTime {-----------} :: !(Maybe Time)
    }
    deriving
        (Eq, Show)

-----------------------------------------------------------
data Listener = Listener_
    { listenerPosition {-----} :: !(V3 Float) -- meters
    , listenerVelocity {-----} :: !(V3 Float) -- meters per second
    , listenerOrientation {--} :: !(V3 Float, V3 Float) -- meters
    , listenerGain {---------} :: !Gain
    }
    deriving
        (Eq, Show)

-------------------------------------------------------------------------------
-- SIMPLE API -----------------------------------------------------------------
-------------------------------------------------------------------------------

-- | Main function of the library.
withSoundstage ::
    (MonadIO m) =>
    a ->
    SF a Soundstage ->
    (ReactHandle a Soundstage -> m b) ->
    m b
{-# INLINE withSoundstage #-}
withSoundstage a sf reactimate =
    withAL Nothing do
        reactimate <=< reactInitSoundstage a sf

-------------------------------------------------------------------------------
-- ADVANCED API ---------------------------------------------------------------
-------------------------------------------------------------------------------

-----------------------------------------------------------
withAL :: 
   (MonadIO m) => 
   Maybe ([AL.ALError] -> IO ()) ->  -- ^ Error handler.
   (ALApp -> m a) -> -- ^ App cont.
   m a
{-# INLINE withAL #-}
withAL clientErrorHandler k = do
    AL.runALUT appName [] \_ _ -> do
        ___ <- liftIO (forkIO (forever handleError))
        --ref <- liftIO (newMVar mempty)
        k undefined -- (ALApp ref)
  where
    handleError = do
        threadDelay 1 
        errors <- AL.alErrors 
        fromMaybe internalErrorHandler clientErrorHandler errors
    internalErrorHandler errors = do
        if not (null errors)
            then putStrLn (appName <> show errors)
            else threadDelay 5_000_000

-----------------------------------------------------------
reactInitSoundstage ::
    (MonadIO m) =>
    a ->
    SF a Soundstage ->
    ALApp ->
    m (ReactHandle a Soundstage)
{-# INLINE reactInitSoundstage #-}
reactInitSoundstage a sf alApp = liftIO do
    ref <- newMVar Nothing
    Yampa.reactInit (pure a) (actuate ref) sf
  where
    actuate ssRef _ updated s1 = do
        when updated do
            takeMVar ssRef >>= \case
                Nothing -> undefined -- soundstage alApp Nothing s1
                Just s0 -> undefined -- soundstage alApp (Just s0) s1
            putMVar ssRef (Just s1)
        pure updated


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


-----------------------------------------------------------
-- For when you want to change the collection of source
-- signals of the soundstage at some point.
soundstage_ ::
    Float ->
    AL.DistanceModel ->
    V3 Float ->
    V3 Float ->
    Double ->
    SF (a, Event ([SF a Source] -> [SF a Source])) Soundstage
{-# INLINE soundstage_ #-}
soundstage_ factor model pos vel gain =
    -- initialSources =
    Soundstage
        <$> undefined -- fmap (Map.fromList . fmap (\x -> (sourceID x, x))) (drpSwitchB [])
        <*> pure factor
        <*> pure 343.3
        <*> pure model
        <*> listener_ pos vel (V3 0 0 (-1), V3 0 1 0) (realToFrac gain)
        <*> (initially Nothing <<< arr Just <<< Yampa.time)

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

