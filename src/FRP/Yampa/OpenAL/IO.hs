{-# LANGUAGE BlockArguments #-}
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
withSoundstage a sf reactimate =
    withAL do
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
    k (ALApp ref bufMap reqMap)
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
    ref <- newMVar Nothing
    Yampa.reactInit (pure a) (actuate ref) sf
  where
    actuate ssRef _ updated s1 = do
        when updated do
            takeMVar ssRef >>= \case
                Nothing -> updateSoundstage alApp Nothing s1
                Just s0 -> updateSoundstage alApp (Just s0) s1
            putMVar ssRef (Just s1)
        pure updated

-----------------------------------------------------------
updateSoundstage :: (MonadIO m) => ALApp -> Maybe Soundstage -> Soundstage -> m ()
{-# INLINE updateSoundstage #-}
updateSoundstage app mss0 ss1 = do
    case mss0 of
        Nothing -> do
            updateSpeedOfSound
            updateDistanceModel
            updateDopplerFactor
            (updateListener Nothing (soundstageListener ss1))
            updateAllSources app Nothing ss1
        Just ss0 -> do
            when (soundstageSpeedOfSound ss1 /= soundstageSpeedOfSound ss0) updateSpeedOfSound
            when (soundstageDistanceModel ss1 /= soundstageDistanceModel ss0) updateDistanceModel
            when (soundstageDopplerFactor ss1 /= soundstageDopplerFactor ss0) updateDopplerFactor
            when (soundstageListener ss1 /= soundstageListener ss0) (updateListener (Just (soundstageListener ss0)) (soundstageListener ss1) )
            updateAllSources app (Just ss0) ss1
  where
    updateSpeedOfSound = AL.speedOfSound $= realToFrac (soundstageSpeedOfSound ss1)
    updateDistanceModel = AL.distanceModel $= soundstageDistanceModel ss1
    updateDopplerFactor = AL.dopplerFactor $= realToFrac (soundstageDopplerFactor ss1)

-----------------------------------------------------------
updateListener :: (MonadIO m) => Maybe Listener -> Listener -> m ()
{-# INLINE updateListener #-}
updateListener ml0 l1 = do
    case ml0 of
        Nothing -> do
            updateListenerPos
            updateListenerVel
            updateListenerOri
            updateListenerGai
        Just l0 -> do
            -- TODO
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
updateAllSources :: forall m. (MonadIO m) => ALApp -> Maybe Soundstage -> Soundstage -> m ()
{-# INLINE updateAllSources #-}
updateAllSources alApp mss0 ss1 = do
    case mss0 of
        Nothing -> do
            __ <- forM_ (soundstageSources ss1) (updateSource alApp Nothing)
            __ <- AL.play =<< lookupALSource alApp (soundstageSources ss1)
            pure ()
        Just ss0 -> do
            let (deleted, notDeleted) = splitMissingSources (soundstageSources ss0) (soundstageSources ss1)
            let (created, notCreated) = splitCreatedSources (soundstageSources ss0) (soundstageSources ss1)
            let (modified, _________) = splitModifiedSources notDeleted notCreated
            -------------------------
            -- update new sourcs
            __ <- forM_ created \src -> do
                        (source :: AL.Source) <- AL.genObjectName
                        liftIO $ AL.buffer source $= Just (head (sourceBufferQueue src)) -- TODO
                        liftIO (modifyMVar_ (sourceMap alApp) (pure . Map.insert (sourceID src) source))
                        updateSource @m alApp Nothing src
            -- update modified sources
            --__ <- undefined
            -----------------------
            __ <- AL.play =<< lookupALSource alApp (playingFrom created <> playingFrom modified)
            __ <- AL.stop =<< lookupALSource alApp (deleted <> stoppedFrom modified)
            __ <- AL.pause =<< lookupALSource alApp (pausedFrom created <> pausedFrom modified)
            pure ()
  where
    splitMissingSources s0 s1 = (Map.difference s0 s1, Map.intersection s1 s0)
    splitCreatedSources s0 s1 = (Map.difference s1 s0, Map.intersection s1 s0)
    splitModifiedSources s0 s1 = (foo s0 (Map.intersection s1 s0), Map.intersection s1 s0)
    playingFrom = Map.filter ((Playing ==) . sourceState)
    stoppedFrom = Map.filter ((Stopped ==) . sourceState)
    pausedFrom = Map.filter ((Paused ==) . sourceState)
    -- TODO bad name
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
updateSource ::
    (MonadIO m) =>
    ALApp ->
    Maybe Source ->
    Source ->
    m ()
{-# INLINE updateSource #-}
updateSource alApp ms0 s1 = do
    foo <- liftIO (takeMVar (sourceMap alApp))
    let sid = fromJust (Map.lookup (sourceID s1) foo)
    case ms0 of
        Nothing -> do
            AL.gainBounds sid $= let foox = realToFrac (fst (sourceGainBounds s1)) in (foox, realToFrac (snd (sourceGainBounds s1)))
            AL.coneAngles sid $= (realToFrac $ fst $ sourceConeAngles s1, realToFrac $ snd (sourceConeAngles s1))
            AL.maxDistance sid $= realToFrac (sourceMaxDistance s1)
            AL.sourceRelative sid $= sourceRelative s1
            AL.rolloffFactor sid $= realToFrac (sourceRolloffFactor s1)
            AL.referenceDistance sid $= realToFrac (sourceReferenceDistance s1)
            AL.coneOuterGain sid $= realToFrac (sourceConeOuterGain s1)
            AL.pitch sid $= realToFrac (abs (sourcePitch s1))
            AL.sourcePosition sid $= _v3ToVertex (sourcePosition s1)
            AL.sourceVelocity sid $= _v3ToVector (sourceVelocity s1)
            AL.sourceGain sid $= realToFrac (abs (sourceGain s1))
            AL.direction sid $= _v3ToVector (sourceDirection s1)
            AL.secOffset sid $= realToFrac (abs (sourceStartOffset s1))
            case sourceOffset s1 of
                o1
                    | o1 > 0 -> useFowardsBuffer alApp Nothing sid
                    | o1 < 0 -> useReverseBuffer Nothing sid
                    | otherwise -> useFowardsBuffer alApp Nothing sid
        Just s0 -> do
            when (sourcePosition s0 /= sourcePosition s1) $ AL.sourcePosition sid $= _v3ToVertex (sourcePosition s1)
            when (sourceVelocity s0 /= sourceVelocity s1) $ AL.sourceVelocity sid $= _v3ToVector (sourceVelocity s1)
            when (sourceGain s0 /= sourceGain s1) $ AL.sourceGain sid $= realToFrac (abs (sourceGain s1))
            when (sourceDirection s0 /= sourceDirection s1) $ AL.direction sid $= _v3ToVector (sourceDirection s1)
            when (sourcePitch s1 /= sourcePitch s0) (AL.pitch sid $= realToFrac (abs (sourcePitch s1)))
            when (sourceStartOffset s1 /= sourceStartOffset s0) do
                AL.gainBounds sid $= let foox = realToFrac (fst (sourceGainBounds s1)) in (foox, realToFrac (snd (sourceGainBounds s1)))
                AL.coneAngles sid $= (realToFrac $ fst $ sourceConeAngles s1, realToFrac $ snd (sourceConeAngles s1))
                AL.maxDistance sid $= realToFrac (sourceMaxDistance s1)
                AL.sourceRelative sid $= sourceRelative s1
                AL.rolloffFactor sid $= realToFrac (sourceRolloffFactor s1)
                AL.referenceDistance sid $= realToFrac (sourceReferenceDistance s1)
                AL.coneOuterGain sid $= realToFrac (sourceConeOuterGain s1)
                AL.secOffset sid $= realToFrac (abs (sourceStartOffset s1))
            when (sourceOffset s1 /= sourceOffset s0 && abs (sourceOffset s1 - sourceOffset s0) > tolerance) do
                if {-going backwards-} 0 > sourceOffset s1 - sourceOffset s0
                    then useReverseBuffer s0 sid
                    else useFowardsBuffer alApp s0 sid
                correctOffset sid s0
    liftIO $ putMVar (sourceMap alApp) foo
  where
    dt s0 = sourceOffset s1 - sourceOffset s0
    tolerance = 0.1 -- seconds
    alReactionTime = 0.002
    correctOffset sid s0 =
        AL.secOffset sid $= realToFrac (abs (sourceOffset s1) - alReactionTime + abs (dt s0))

    -- TODO ---------------------------------

    useReverseBuffer s0 sid = do
        --x <- liftIO $ readIORef (bufferMap alApp)
        -- when already using reverse buffer, just update the offset case is too far apart.
        -- when not using reverse buffer, check if reverse buffer is ready,
        -- if reverse buffer is ready, fix the offset given the time it took to load and start using it with the source.
        error "not supported yet"
    useFowardsBuffer alApp ms0 sid = do
        --x <- liftIO $ readIORef (bufferMap alApp)
        -- checkIf the buffer been used is the not reversed one, if it isnt, make it be.
        pure () -- error "TODO"

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
    , bufferMap :: !(IORef (Map String (Maybe AL.Buffer)))
    , requestMap :: !(IORef [Request])
    }

data Request = CreateReverseBuffer !String
