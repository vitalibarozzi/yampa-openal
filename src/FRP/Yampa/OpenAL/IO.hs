{-# LANGUAGE BlockArguments #-}
module FRP.Yampa.OpenAL.IO (
    withSoundstage,
    withAL,
    reactInitSoundstage,
)
where

import Control.Concurrent ( modifyMVar_, newMVar )
import Control.Monad ( when, (<=<) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.IORef ( newIORef )
import Data.StateVar ( HasGetter(get) )
import FRP.Yampa ( SF, ReactHandle )
import qualified FRP.Yampa as Yampa
import FRP.Yampa.OpenAL.Soundstage ( updateSoundstage, Soundstage )
import FRP.Yampa.OpenAL.Types ( ALApp(ALApp) )
import FRP.Yampa.OpenAL.Util ( appName )
import qualified Sound.ALUT.Initialization as AL
import Sound.OpenAL.AL.Extensions ( alVersion )
import Sound.OpenAL.AL.StringQueries ( alRenderer, alVendor )

-----------------------------------------------------------

-- | Main function of the library, to be used at the start of the program.
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

-----------------------------------------------------------

{- | This is used to start the resources needed for our
integration with the OpenAL API.
-}
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

-- | Used to reactimate the SF using the OpenAL backend.
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
    putStrLn (appName <> " Version : " <> ver)
    putStrLn (appName <> " Vendor  : " <> ven)
    putStrLn (appName <> " Render  : " <> ren)
    soundstageRef <- newMVar Nothing
    Yampa.reactInit (pure a) (actuate soundstageRef) sf
  where
    actuate ssRef _ updated s1 = do
        let updater ms0 = updateSoundstage True ms0 s1 >> return (Just s1)
        when updated (modifyMVar_ ssRef updater)
        pure updated
