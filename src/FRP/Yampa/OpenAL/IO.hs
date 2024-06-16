{-# LANGUAGE BlockArguments #-}
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
withSoundstage a sf reactimate = do
    withAL do
        reactimate <=< reactInitSoundstage a sf

-------------------------------------------------------------------------------
-- ADVANCED API ---------------------------------------------------------------
-------------------------------------------------------------------------------

-----------------------------------------------------------
withAL :: (MonadIO m) => (ALApp -> m a) -> m a
{-# INLINE withAL #-}
withAL k = do
    AL.runALUT name [] \_name _arguments -> do
        reqMap <- liftIO (newIORef mempty)
        _ <- liftIO (forkIO (forever (threadDelay 1 >> runRequests reqMap >> threadDelay 1 >> runErrors)))
        ref <- liftIO (newIORef mempty)
        bufMap <- liftIO (newIORef mempty)
        k (ALApp ref bufMap reqMap) -- TODO
  where
    name = "[Yampa-OpenAL]"
    timeout_ = 5_000_000
    runRequests reqMap = do
        rm <- readIORef reqMap
        forM_ rm \case CreateReverseBuffer _buffer -> error "not supported yet"
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
    a ->
    SF a Soundstage ->
    ALApp ->
    m (ReactHandle a Soundstage)
{-# INLINE reactInitSoundstage #-}
reactInitSoundstage a sf alApp = do
    liftIO do
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
updateSoundstage app ss1 ss0 = do
    -- when True updateSpeedOfSound
    -- when True updateDistanceModel
    -- when True updateDopplerFactor
    -- when True (updateListener (soundstageListener ss1) (soundstageListener ss0))
    when True (updateSources app ss1 ss0)

-- where
-- updateSpeedOfSound = AL.speedOfSound $= realToFrac (soundstageSpeedOfSound ss1)
-- updateDistanceModel = AL.distanceModel $= soundstageDistanceModel ss1
-- updateDopplerFactor = AL.dopplerFactor $= realToFrac (soundstageDopplerFactor ss1)

-----------------------------------------------------------
updateListener :: (MonadIO m) => Listener -> Listener -> m ()
{-# INLINE updateListener #-}
updateListener l1 _ = do
    -- when True updateListenerPos
    -- when True updateListenerVel
    -- when True updateListenerOri
    -- when True updateListenerGai
    pure ()

-- where
-- updateListenerPos = AL.listenerPosition $= _v3ToVertex (listenerPosition l1)
-- updateListenerVel = AL.listenerVelocity $= _v3ToVector (listenerVelocity l1)
-- updateListenerOri = AL.orientation $= bimap _v3ToVector _v3ToVector (listenerOrientation l1)
-- updateListenerGai = AL.listenerGain $= realToFrac (listenerGain l1)

-----------------------------------------------------------
updateSources :: forall m. (MonadIO m) => ALApp -> Soundstage -> Soundstage -> m ()
{-# INLINE updateSources #-}
updateSources alApp@(ALApp ref _ _) ss1 ss0 = do
    _sourceMap <- liftIO $ readIORef ref
    let (deleted, notDeleted) = splitMissingSources (soundstageSources ss0) (soundstageSources ss1)
    let (created, notCreated) = splitCreatedSources (soundstageSources ss0) (soundstageSources ss1)
    let (modified, _________) = splitModifiedSources notDeleted notCreated
    _____________________ <- updateNewSources alApp created
    _____________________ <- updateModifiedSources alApp modified
    _____________________ <- AL.play =<< lookupALSource alApp (playingFrom created <> playingFrom modified)
    _____________________ <- AL.stop =<< lookupALSource alApp (deleted <> stoppedFrom modified)
    _____________________ <- AL.pause =<< lookupALSource alApp (pausedFrom created <> pausedFrom modified)
    pure ()
  where
    splitMissingSources s0 s1 = (Map.difference s0 s1, Map.intersection s1 s0)
    splitCreatedSources s0 s1 = (Map.difference s1 s0, Map.intersection s1 s0)
    splitModifiedSources s0 s1 = (Map.intersection s1 s0, Map.intersection s1 s0) -- (undefined (Map.intersection s1 s0), undefined (Map.intersection s1 s0)) -- TODO
    playingFrom = Map.filter (stateIs Playing)
    stoppedFrom = Map.filter (stateIs Stopped)
    pausedFrom = Map.filter (stateIs Paused)

-----------------------------------------------------------
updateNewSources ::
    (MonadIO m) =>
    ALApp ->
    Map String Source ->
    m ()
updateNewSources alApp newSources = do
    forM_ newSources \src -> do
        (source :: AL.Source) <- AL.genObjectName
        AL.buffer source $= Just (head $ sourceBufferQueue src)
        liftIO (modifyIORef (sourceMap alApp) (Map.insert (sourceID src) source))
        updateSource alApp src Nothing

-----------------------------------------------------------
updateModifiedSources ::
    (MonadIO m) =>
    ALApp ->
    Map String Source ->
    m ()
{-# INLINE updateModifiedSources #-}
updateModifiedSources alApp modifiedSources = do
    liftIO $ print ("mod:" <> show modifiedSources)
    pure ()

-----------------------------------------------------------
updateSource :: (MonadIO m) => ALApp -> Source -> Maybe Source -> m ()
{-# INLINE updateSource #-}
updateSource app s1 ms0 = do
    sid <- fromJust . Map.lookup (sourceID s1) <$> liftIO (readIORef (sourceMap app))
    -- when True $ AL.sourcePosition sid $= _v3ToVertex (sourcePosition s1)
    -- when True $ AL.sourceVelocity sid $= _v3ToVector (sourceVelocity s1)
    -- when True $ AL.sourceGain sid $= realToFrac (sourceGain s1)
    -- when True $ AL.gainBounds sid $= let foo = realToFrac (fst (sourceGainBounds s1)) in (foo, realToFrac (snd (sourceGainBounds s1)))
    -- when True $ AL.direction sid $= _v3ToVector (sourceDirection s1)
    -- when True $ AL.sourceRelative sid $= sourceRelative s1
    -- when True $ AL.rolloffFactor sid $= realToFrac (sourceRolloffFactor s1)
    -- when True $ AL.referenceDistance sid $= realToFrac (sourceReferenceDistance s1)
    -- when True $ AL.maxDistance sid $= realToFrac (sourceMaxDistance s1)
    -- when True $ AL.coneAngles sid $= (realToFrac $ fst $ sourceConeAngles s1, realToFrac $ snd (sourceConeAngles s1))
    -- when True $ AL.coneOuterGain sid $= realToFrac (sourceConeOuterGain s1)
    case ms0 of
        Nothing -> pure ()
        Just s0 -> do
            when (sourcePitch s1 /= sourcePitch s0) (AL.pitch sid $= realToFrac (sourcePitch s1))
            when (sourceOffset s1 /= sourceOffset s0) do
                if {-going backwards-} 0 > sourceOffset s1 - sourceOffset s0
                    then useReverseBuffer s0 sid
                    else useFowardsBuffer s0 sid
                correctOffset sid s0
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
lookupALSource alApp = do
    fmap (fmap snd . Map.toList) . mapM lookup_
  where
    lookup_ Source{..} = do
        mp <- liftIO $ readIORef (sourceMap alApp)
        case Map.lookup sourceID mp of
            Just sid -> pure sid
            ________ -> error "bug"

-----------------------------------------------------------
stateIs :: SourceState -> Source -> Bool
stateIs s Source{..} =
    case (sourceState, s) of
        (Playing, Playing) -> True
        (Playing, Paused) -> False
        (Playing, Stopped) -> False
        x -> error . show $ x

-----------------------------------------------------------

-- | Internal.
data ALApp = ALApp
    { sourceMap :: IORef (Map String AL.Source)
    , bufferMap :: IORef (Map String BufferPayload)
    , requestMap :: IORef [Request]
    }

type BufferPayload = (AL.Buffer, Maybe AL.Buffer)

data Request = CreateReverseBuffer String
