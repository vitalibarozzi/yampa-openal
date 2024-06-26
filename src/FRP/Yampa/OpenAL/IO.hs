{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module FRP.Yampa.OpenAL.IO where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Maybe
import FRP.Yampa
import qualified FRP.Yampa as Yampa
import FRP.Yampa.OpenAL.Soundstage
import FRP.Yampa.OpenAL.Types
import FRP.Yampa.OpenAL.Util
import qualified Sound.ALUT.Initialization as AL
import qualified Sound.OpenAL as AL

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
withSoundstage a sf reactimate_ =
    withAL Nothing do
        reactimate_ <=< reactInitSoundstage a sf

-------------------------------------------------------------------------------
-- ADVANCED API ---------------------------------------------------------------
-------------------------------------------------------------------------------

-----------------------------------------------------------
withAL ::
    (MonadIO m) =>
    -- | Error handler.
    Maybe ([AL.ALError] -> IO ()) ->
    -- | App cont.
    (ALApp -> m a) ->
    m a
{-# INLINE withAL #-}
withAL clientErrorHandler k = do
    AL.runALUT appName [] \_ _ -> do
        ___ <- liftIO (forkIO (forever handleError))
        ref <- liftIO (newMVar mempty)
        ioref <- liftIO (newIORef mempty)
        k (ALApp ref ioref)
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
                Nothing -> updateSoundstage Nothing s1
                Just s0 -> updateSoundstage (Just s0) s1
            putMVar ssRef (Just s1)
        pure updated
