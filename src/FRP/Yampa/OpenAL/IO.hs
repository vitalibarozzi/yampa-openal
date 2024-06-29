{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module FRP.Yampa.OpenAL.IO (withSoundstage, withAL, reactInitSoundstage)
where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import FRP.Yampa
import qualified FRP.Yampa as Yampa
import FRP.Yampa.OpenAL.Soundstage
import FRP.Yampa.OpenAL.Types
import FRP.Yampa.OpenAL.Util
import qualified Sound.ALUT.Initialization as AL
import Sound.OpenAL.AL.StringQueries
import Sound.OpenAL.AL.Extensions
import Data.StateVar

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
    withAL do
        reactimate_ <=< reactInitSoundstage a sf

-------------------------------------------------------------------------------
-- ADVANCED API ---------------------------------------------------------------
-------------------------------------------------------------------------------

-----------------------------------------------------------
withAL ::
    (MonadIO m) =>
    -- | App cont.
    (ALApp -> m a) ->
    m a
{-# INLINE withAL #-}
withAL k = AL.runALUT appName [] \_ _ -> do
    ref <- liftIO (newMVar mempty) -- TODO not used anymore
    ioref <- liftIO (newIORef mempty) -- TODO not used anymore
    k (ALApp ref ioref)

-----------------------------------------------------------
reactInitSoundstage ::
    (MonadIO m) =>
    a ->
    SF a Soundstage ->
    ALApp ->
    m (ReactHandle a Soundstage)
{-# INLINE reactInitSoundstage #-}
reactInitSoundstage a sf _ = liftIO do
    ver <- liftIO (get alVersion)
    ven <- liftIO (get alVendor)
    ren <- liftIO (get alRenderer)
    putStrLn (appName <> " Version: " <> ver)
    putStrLn (appName <> " Vendor : " <> ven)
    putStrLn (appName <> " Render : " <> ren)
    soundstageRef <- newMVar Nothing
    Yampa.reactInit (pure a) (actuate soundstageRef) sf
  where
    actuate ssRef _ updated s1 = do
        when updated do
            modifyMVar_ ssRef \ms0 -> do
                updateSoundstage ms0 s1
                return (Just s1)
        pure updated
