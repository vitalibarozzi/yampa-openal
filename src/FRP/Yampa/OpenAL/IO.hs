{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FRP.Yampa.OpenAL.IO (
    withSoundstage,
    withAL,
    reactInitSoundstage,
    updateSoundstage,
    updateListener,
    updateSource,
)
where

import Data.Maybe
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.IORef
import Data.StateVar
import FRP.Yampa (ReactHandle, SF)
import qualified FRP.Yampa as Yampa
import FRP.Yampa.OpenAL.Types
import FRP.Yampa.OpenAL.Util
import qualified Sound.ALUT.Initialization as AL
import qualified Sound.OpenAL as AL
import qualified Sound.OpenAL.AL.Source as AL
import System.Timeout
import Data.Map (Map)
import qualified Data.Map as Map

-----------------------------------------------------------

-- | Main function of the library.
withSoundstage ::
    (MonadIO m) =>
    a ->
    SF a Soundstage ->
    (ReactHandle a Soundstage -> m b) ->
    m b
{-# INLINE withSoundstage #-}
withSoundstage a sf reactor = do
    withAL \alApp -> do
        reactor =<< reactInitSoundstage alApp a sf

-----------------------------------------------------------
withAL :: (MonadIO m) => (ALApp -> m a) -> m a
{-# INLINE withAL #-}
withAL k = do
    AL.runALUT name [] \_name _arguments -> do
        _ <- liftIO (forkIO (forever (threadDelay 1 >> runErrors)))
        ref <- liftIO (newIORef mempty)
        k (ALApp ref)
  where
    name = "[Yampa-OpenAL]"
    timeout_ = 5_000_000
    runErrors :: IO ()
    runErrors = do
        handle (\exc -> putStrLn (name <> ": " <> show (exc :: SomeException))) do
            void $ timeout timeout_ do
                errors <- AL.alErrors
                if not (null errors)
                    then putStrLn (name <> show errors)
                    else threadDelay (timeout_ `div` 2)

-----------------------------------------------------------
reactInitSoundstage ::
    (MonadIO m) =>
    ALApp ->
    a ->
    SF a Soundstage ->
    m (ReactHandle a Soundstage)
{-# INLINE reactInitSoundstage #-}
reactInitSoundstage alApp a sf = do
    liftIO do
        ref <- newIORef Nothing
        Yampa.reactInit (pure a) (actuate ref) sf
  where
    actuate ref _ updated s1 = do
        when updated do
            ms0 <- readIORef ref
            case ms0 of
                Nothing -> writeIORef ref (Just s1)
                Just s0 -> do
                    __ <- writeIORef ref (Just s1)
                    updateSoundstage alApp s1 s0
        pure updated

-----------------------------------------------------------
updateSoundstage :: (MonadIO m) => ALApp -> Soundstage -> Soundstage -> m ()
{-# INLINE updateSoundstage #-}
updateSoundstage app ss1 ss0 = do
    when True updateSpeedOfSound
    when True updateDistanceModel
    when True updateDopplerFactor
    --when True (updateListener (undefined ss1) (undefined ss0)) -- TODO
    when True (updateSources app ss1 ss0)
  where
    updateSpeedOfSound = AL.speedOfSound $= realToFrac (soundstageSpeedOfSound ss1)
    updateDistanceModel = AL.distanceModel $= soundstageDistanceModel ss1
    updateDopplerFactor = AL.dopplerFactor $= realToFrac (soundstageDopplerFactor ss1)

-----------------------------------------------------------
updateListener :: (MonadIO m) => Listener -> Listener -> m ()
{-# INLINE updateListener #-}
updateListener l1 _ = do
    when True updateListenerPos
    when True updateListenerVel
    when True updateListenerOri
    when True updateListenerGai
  where
    updateListenerPos = AL.listenerPosition $= _v3ToVertex (listenerPosition l1)
    updateListenerVel = AL.listenerVelocity $= _v3ToVector (listenerVelocity l1)
    updateListenerOri = AL.orientation $= bimap _v3ToVector _v3ToVector (listenerOrientation l1)
    updateListenerGai = AL.listenerGain $= realToFrac (listenerGain l1)

-----------------------------------------------------------
updateSources :: forall m. (MonadIO m) => ALApp -> Soundstage -> Soundstage -> m ()
{-# INLINE updateSources #-}
updateSources (ALApp ref) ss1 ss0 = do
    _sourceMap            <- liftIO $ readIORef ref
    (deleted, notDeleted) <- splitMissingSources (soundstageSources ss0) (soundstageSources ss1)
    (created, notCreated) <- splitCreatedSources (soundstageSources ss0) (soundstageSources ss1)
    (modified, _________) <- splitModifiedSources notDeleted notCreated
    --______________________<- undefined -- createNewSources created
    --______________________<- undefined -- updateSource modified -- updateModifiedSources
    _____________________ <- AL.play (playingFrom created <> playingFrom modified)
    _____________________ <- AL.stop (deleted <> stoppedFrom modified)
    _____________________ <- AL.pause (pausedFrom created <> pausedFrom modified)
    _____________________ <- AL.rewind (rewindingFrom created <> rewindingFrom modified)
    pure ()
  where

    splitMissingSources :: Map String Source -> Map String Source -> m ([AL.Source], [AL.Source])
    splitMissingSources s0 s1 = do
        -- all sources that are in the old one but not in the new one
        pure ([],[]) -- undefined -- TODO

    splitCreatedSources :: Map String Source -> Map String Source -> m ([AL.Source], [AL.Source])
    splitCreatedSources _ _ = do
        -- all sources there are in the new stage but not in the old one
        pure ([],[]) -- undefined -- TODO

    splitModifiedSources :: [AL.Source] -> [AL.Source] -> m ([AL.Source], [AL.Source])
    splitModifiedSources _ _ = do
        pure ([],[]) -- undefined -- TODO

    playingFrom :: [AL.Source] -> [AL.Source]
    playingFrom = id -- undefined -- filter ((/= undefined) . undefined)
    -- , sourceState {--------------} :: !AL.SourceState -- almost always playing.

    stoppedFrom :: [AL.Source] -> [AL.Source]
    stoppedFrom = id -- undefined -- filter ((/= undefined) . undefined)
    -- , sourceState {--------------} :: !AL.SourceState -- almost always playing.

    pausedFrom :: [AL.Source] -> [AL.Source]
    pausedFrom = id -- undefined -- filter ((/= undefined) . undefined)
    -- , sourceState {--------------} :: !AL.SourceState -- almost always playing.

    rewindingFrom :: [AL.Source] -> [AL.Source]
    rewindingFrom = id -- undefined -- filter ((/= undefined) . undefined)
    -- , sourceState {--------------} :: !AL.SourceState -- almost always playing.

-----------------------------------------------------------
updateSource :: (MonadIO m) => ALApp -> Source -> Source -> m ()
{-# INLINE updateSource #-}
updateSource app s1 s0 = do
    sid <- fromJust . Map.lookup (sourceID s1) <$> liftIO (readIORef (sourceMap app))
    when True $ AL.sourcePosition sid $= _v3ToVertex (sourcePosition s1)
    when True $ AL.sourceVelocity sid $= _v3ToVector (sourceVelocity s1)
    when True $ AL.sourceGain sid $= realToFrac (sourceGain s1)
    when True $ AL.gainBounds sid $= let foo = realToFrac (fst (sourceGainBounds s1)) in (foo, realToFrac (snd (sourceGainBounds s1)))
    when True $ AL.direction sid $= _v3ToVector (sourceDirection s1)
    when True $ AL.sourceRelative sid $= sourceRelative s1
    when True $ AL.rolloffFactor sid $= realToFrac (sourceRolloffFactor s1)
    when True $ AL.referenceDistance sid $= realToFrac (sourceReferenceDistance s1)
    when True $ AL.maxDistance sid $= realToFrac (sourceMaxDistance s1)
    when True $ AL.coneAngles sid $= (realToFrac $ fst $ sourceConeAngles s1, realToFrac $ snd (sourceConeAngles s1))
    when True $ AL.coneOuterGain sid $= realToFrac (sourceConeOuterGain s1)
    when (sourcePitch s1 /= sourcePitch s0) (AL.pitch sid $= realToFrac (sourcePitch s1))
    -- TODO we do change the offset if its going backwards, otherwise we don't

-----------------------------------------------------------

{- | Internal.
-}
data ALApp = ALApp { sourceMap :: IORef (Map String AL.Source) }
