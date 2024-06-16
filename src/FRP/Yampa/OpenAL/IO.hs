{-# LANGUAGE BlockArguments #-}
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

module FRP.Yampa.OpenAL.IO (
    withSoundstage,
    withAL,
    reactInitSoundstage,
    updateSoundstage,
    updateListener,
    updateSource,
)
where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.StateVar
import FRP.Yampa (ReactHandle, SF)
import qualified FRP.Yampa as Yampa
import FRP.Yampa.OpenAL.Types
import FRP.Yampa.OpenAL.Util
import qualified Sound.ALUT.Initialization as AL
import qualified Sound.OpenAL as AL
import System.Timeout
import qualified Control.Concurrent.MVar as MVar

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
withSoundstage a sf reactimate = withAL do
    reactimate <=< reactInitSoundstage a sf

-------------------------------------------------------------------------------
-- ADVANCED API ---------------------------------------------------------------
-------------------------------------------------------------------------------

-----------------------------------------------------------
withAL :: (MonadIO m) => (ALApp -> m a) -> m a
{-# INLINE withAL #-}
withAL k = AL.runALUT name [] \_ _ -> do
    reqMap <- liftIO (newIORef mempty)
    _ <- liftIO (forkIO (forever (threadDelay 1 >> runRequests reqMap >> threadDelay 1 >> runErrors)))
    ref <- liftIO (newMVar mempty)
    bufMap <- liftIO (newIORef mempty)
    k (ALApp ref bufMap reqMap) -- TODO
  where
    name = "[Yampa-OpenAL]"
    timeout_ = 5_000_000
    runRequests reqMap = do
        rm <- readIORef reqMap
        forM_ rm \case CreateReverseBuffer _buffer -> error "not supported yet"
    runErrors :: IO ()
    runErrors = handle (\exc -> putStrLn (name <> ": " <> show (exc :: SomeException))) do
        void $ timeout timeout_ do
            errors <- AL.alErrors
            if not (null errors)
                then putStrLn (name <> show errors)
                else threadDelay (timeout_ `div` 2)

-----------------------------------------------------------
reactInitSoundstage ::
    (MonadIO m) =>
    a ->
    SF a Soundstage ->
    ALApp ->
    m (ReactHandle a Soundstage)
{-# INLINE reactInitSoundstage #-}
reactInitSoundstage a sf alApp = liftIO do
    ref <- newIORef Nothing
    Yampa.reactInit (pure a) (actuate ref) sf
  where
    actuate ref _ updated s1 = do
        when updated do
            readIORef ref >>= \case
                Nothing -> pure ()
                Just s0 -> updateSoundstage alApp s1 s0
            writeIORef ref (Just s1)
        pure updated

-----------------------------------------------------------
updateSoundstage :: (MonadIO m) => ALApp -> Soundstage -> Soundstage -> m ()
{-# INLINE updateSoundstage #-}
updateSoundstage app ss0 ss1 = do
    when (soundstageSpeedOfSound ss1 /= soundstageSpeedOfSound ss0) updateSpeedOfSound
    when (soundstageDistanceModel ss1 /= soundstageDistanceModel ss0) updateDistanceModel
    when (soundstageDopplerFactor ss1 /= soundstageDopplerFactor ss0) updateDopplerFactor
    when (soundstageListener ss1 /= soundstageListener ss0) (updateListener (soundstageListener ss1) (soundstageListener ss0))
    when (soundstageSources ss1 /= soundstageSources ss0) (updateSources app ss0 ss1)
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
updateSources alApp ss1 ss0 = do
    let (deleted, notDeleted) = splitMissingSources (soundstageSources ss0) (soundstageSources ss1)
    let (created, notCreated) = splitCreatedSources (soundstageSources ss0) (soundstageSources ss1)
    let (modified, _________) = splitModifiedSources notDeleted notCreated
    __ <- updateNewSources alApp created
    __ <- updateModifiedSources alApp modified
    __ <- AL.play =<< lookupALSource alApp (playingFrom created <> playingFrom modified)
    --__ <- AL.stop =<< lookupALSource alApp (deleted <> stoppedFrom modified)
    --__ <- AL.pause =<< lookupALSource alApp (pausedFrom created <> pausedFrom modified)
    pure ()
  where
    splitMissingSources s0 s1 = (Map.difference s0 s1, Map.intersection s1 s0)
    splitCreatedSources s0 s1 = (Map.difference s1 s0, Map.intersection s1 s0)
    splitModifiedSources s0 s1 = (foo s0 (Map.intersection s1 s0), Map.intersection s1 s0)
    playingFrom = Map.filter ((Playing ==) . sourceState)
    stoppedFrom = Map.filter ((Stopped ==) . sourceState)
    pausedFrom = Map.filter ((Paused ==) . sourceState)
    foo s0 =
        foldr
            ( \y@Source{..} mp -> case Map.lookup sourceID s0 of
                Just x@Source{} ->
                    if x /= y
                        then undefined -- TODO
                        else mp
                Nothing -> undefined -- TODO
            )
            mempty

-----------------------------------------------------------
updateNewSources ::
    (MonadIO m) =>
    ALApp ->
    Map String Source ->
    m ()
{-# INLINE updateNewSources #-}
updateNewSources alApp newSources = do
    --when (not . null $ (Map.toList newSources)) (error . show $ newSources)
    forM_ newSources \src -> do
        (source :: AL.Source) <- AL.genObjectName
        AL.buffer source $= Just (head (sourceBufferQueue src))
        liftIO (modifyMVar_ (sourceMap alApp) (pure . Map.insert (sourceID src) source))
        updateSource alApp Nothing src

-----------------------------------------------------------
updateModifiedSources ::
    (MonadIO m) =>
    ALApp ->
    Map String Source ->
    m ()
{-# INLINE updateModifiedSources #-}
updateModifiedSources alApp modifiedSources = do
    forM_ modifiedSources  \src -> do
        updateSource alApp Nothing src

-----------------------------------------------------------
updateSource :: (MonadIO m) => ALApp -> Maybe Source -> Source -> m ()
{-# INLINE updateSource #-}
updateSource app ms0 s1 = do
    foo <- liftIO (takeMVar (sourceMap app))
    let msid = Map.lookup (sourceID s1) foo
    case msid of
        Nothing -> undefined -- pure ()
        Just sid -> do
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
            case ms0 of
                Nothing -> AL.pitch sid $= realToFrac (sourcePitch s1)
                Just s0 -> do
                    when (sourcePitch s1 /= sourcePitch s0) undefined -- (AL.pitch sid $= realToFrac (sourcePitch s1))
                    when (sourceOffset s1 /= sourceOffset s0) do
                        if {-going backwards-} 0 > sourceOffset s1 - sourceOffset s0
                            then useReverseBuffer s0 sid
                            else useFowardsBuffer s0 sid
                        correctOffset sid s0
    liftIO $ putMVar (sourceMap app) foo
  where
    dt s0 = sourceOffset s1 - sourceOffset s0
    tolerance = 0.1 -- seconds
    alReactionTime = 0.002
    correctOffset sid s0 =
        AL.secOffset sid $= realToFrac (abs (sourceOffset s1) + alReactionTime + abs (dt s0))

    useReverseBuffer s0 sid = do
        -- when already using reverse buffer, just update the offset case is too far apart.
        -- when not using reverse buffer, check if reverse buffer is ready,
        -- if reverse buffer is ready, fix the offset given the time it took to load and start using it with the source.
        error "not supported yet"

    useFowardsBuffer s0 sid = do
        -- checkIf the buffer been used is the not reversed one, if it isnt, make it be.
        error "TODO"

-----------------------------------------------------------
lookupALSource :: (MonadIO m) => ALApp -> Map String Source -> m [AL.Source]
{-# INLINE lookupALSource #-}
lookupALSource alApp =
    fmap (mapMaybe snd . Map.toList) . mapM lookup_
  where
    lookup_ Source{..} = do
        mp <- liftIO $ takeMVar (sourceMap alApp)
        case Map.lookup sourceID mp of
            Just sid -> do
                liftIO $ putMVar (sourceMap alApp) mp
                pure (Just sid)
            ________ -> error $ "bug: couldn't find '" <> sourceID <> "'"

-----------------------------------------------------------

-- | Internal.
data ALApp = ALApp
    { sourceMap :: !(MVar (Map String AL.Source))
    , bufferMap :: !(IORef (Map String BufferPayload))
    , requestMap :: !(IORef [Request])
    }

type BufferPayload = (AL.Buffer, Maybe AL.Buffer)

data Request = CreateReverseBuffer !String
