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
        k ()
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
    when True (updateListener (undefined ss1) (undefined ss0)) -- TODO
    when True (updateSources app ss1 ss0)
  where
    updateSpeedOfSound = AL.speedOfSound $= realToFrac (soundstageSpeedOfSound ss1)
    updateDistanceModel = AL.distanceModel $= soundstageDistanceModel ss1
    updateDopplerFactor = AL.dopplerFactor $= realToFrac (soundstageDopplerFactor ss1)

---------------------

-----------------------------------------------------------
updateListener :: (MonadIO m) => Listener -> Listener -> m ()
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
updateSources :: (MonadIO m) => ALApp -> Soundstage -> Soundstage -> m ()
updateSources app ss1 ss0 = do
    (newSources, missingSources, pausedSources, stoppedSources, rewindedSources, modifiedSources) <- discriminateSources
    unless (null newSources) (AL.play newSources)
    unless (null modifiedSources) (forM_ modifiedSources (uncurry $ updateSource app))
    unless (null rewindedSources) (AL.rewind rewindedSources)
    unless (null pausedSources) (AL.pause pausedSources)
    unless (null (missingSources <> stoppedSources)) (AL.stop (missingSources <> stoppedSources))
  where
    discriminateSources :: (MonadIO m) => m (Set AL.Source, Set AL.Source, Set AL.Source, Set AL.Source, Set AL.Source, Set (Source, Source))
    discriminateSources = do
        newSources <- getNewSources (soundstageSources ss1) (soundstageSources ss0)
        pausedSources <- undefined (soundstageSources ss1)
        missingSources <- getMissingSources (soundstageSources ss1) (soundstageSources ss0)
        modifiedSources <- getModifiedSources (newSources, missingSources) (soundstageSources ss1, soundstageSources ss0)
        stoppedSources <- undefined (soundstageSources ss1)
        rewindedSources <- undefined (soundstageSources ss1)
        return (newSources, missingSources, pausedSources, stoppedSources, rewindedSources, modifiedSources)
      where
        getNewSources = undefined -- TODO
        getMissingSources = undefined -- TODO
        getModifiedSources = undefined -- TODO

-----------------------------------------------------------
updateSource :: (MonadIO m) => ALApp -> Source -> Source -> m ()
{-# INLINE updateSource #-}
updateSource app s1 s0 = do
    when (sourceState s1 /= sourceState s0) updateState
    when (sourcePitch s1 /= sourcePitch s0) updatePitch
  where
    -- , sourceState {--------------} :: !AL.SourceState -- almost always playing.
    -- , sourceLoopingMode {--------} :: !AL.LoopingMode -- almost always one-shot.
    -- , sourceBufferQueue {--------} :: ![AL.Buffer]
    -- , sourcePitch {--------------} :: !Pitch -- almost always one.
    -- , sourceStartOffset {--------} :: !Float
    -- , sourceOffset {-------------} :: !Float
--                        AL.sourcePosition sid $= _v3ToVertex (sourcePosition source_)
--                        AL.sourceVelocity sid $= _v3ToVector (sourceVelocity source_)
--                        AL.sourceGain sid $= realToFrac (sourceGain source_)
--                        AL.gainBounds sid $= let foo = realToFrac (fst (sourceGainBounds source_)) in (foo, realToFrac (snd (sourceGainBounds source_)))
--                        AL.direction sid $= _v3ToVector (sourceDirection source_)
--                        AL.sourceRelative sid $= sourceRelative source_
--                        AL.rolloffFactor sid $= realToFrac (sourceRolloffFactor source_)
--                        AL.referenceDistance sid $= realToFrac (sourceReferenceDistance source_)
--                        AL.maxDistance sid $= realToFrac (sourceMaxDistance source_)
--                        AL.coneAngles sid $= (realToFrac $ fst $ sourceConeAngles source_, realToFrac $ snd (sourceConeAngles source_))
--                        AL.coneOuterGain sid $= realToFrac (sourceConeOuterGain source_)

    updateState = case sourceState s1 of
        AL.Playing -> AL.play [undefined] -- TODO
        __________ -> return ()
    updatePitch = case sourceState s1 of
        AL.Playing -> AL.play [undefined] -- TODO
        __________ -> return ()

type Set a = [a]

-----------------------------------------------------------

-- | Internal.
-- TODO needs to contain source and buffer maps or caches
type ALApp = ()

{-
-----------------------------------------------------------
{-# INLINE dataConvert #-}
dataConvert = \case
    -- File filePath _ -> Just (ALUT.File filePath)
    -- Memory ptr size _ -> Just (ALUT.FileImage (AL.MemoryRegion (unsafeCoerce ptr) (fromIntegral size)))
    -- HelloWorld -> Just ALUT.HelloWorld
    WhiteNoise -> (ALUT.WhiteNoise 0.5)
    Sine hz ph -> (ALUT.Sine (realToFrac hz) ph 0.5)
    Square hz ph -> (ALUT.Square (realToFrac hz) ph 0.5)
    Sawtooth hz ph -> (ALUT.Sawtooth (realToFrac hz) ph 0.5)
    Impulse hz ph -> (ALUT.Impulse (realToFrac hz) ph 0.5)

-----------------------------------------------------------
{-# INLINE dataLooping #-}
dataLooping = \case
    File{} -> AL.OneShot
    Memory{} -> AL.OneShot
    HelloWorld -> AL.OneShot
    WhiteNoise -> AL.Looping
    Sine{} -> AL.Looping
    Square{} -> AL.Looping
    Sawtooth{} -> AL.Looping
    Impulse{} -> AL.Looping


    -- check if this source exists in the past
        Just !sid -> do
            sameSoundData_ sid source_
            soundDataChanged sid source_
            sameSoundData_ sid source_
            soundDataChanged sid source_

    Nothing -> pure ()
    Just !csource -> do
        bufferx <- case Map.lookup (sourceSoundData source_) buffers of
            Nothing -> ALUT.createBuffer csource
            Just bufferx -> return bufferx
        !xxx <- ObjectName.genObjectName
        ________ <- AL.buffer xxx $= Just bufferx
        ________ <- AL.loopingMode xxxjb $= dataLooping (head $ sourceSoundData source_)
        ________ <- AL.play [xxx]
        ________ <- modifyMVar_ (alAppBuffers alApp) (pure . Map.insert (head $ sourceSoundData source_) bufferx)
        return ()

AL.stop [sid]
case dataConvert (head $ sourceSoundData source_) of
    Nothing -> pure ()
    Just !csource -> do
        !bufferx <- ALUT.createBuffer csource
        AL.buffer sid $= Just bufferx
        AL.play [sid]
        modifyMVar_ (alAppBuffers alApp) (pure . Map.insert (head $ sourceSoundData source_) bufferx)

    [File _ (minOffset, maxOffset) offset]
        | offset < abs minOffset -> AL.stop [sid]
        | offset == abs minOffset -> AL.play [sid]
        | otherwise -> do
            if abs offset >= maxOffset
                then AL.stop [sid]
                else do
                    updateRest sid source_
                    offsetSec0 <- realToFrac <$> get (AL.secOffset sid)
                    if abs (offset - offsetSec0) > (abs maxOffset / 100) || abs (offset - offsetSec0) > 5
                        then AL.secOffset sid $= realToFrac (offset + (2 * offsetSec0)) / 3
                        else
                            when
                                (abs (offset - offsetSec0) > (abs maxOffset / 200))
                                (AL.secOffset sid $= realToFrac (offset + offsetSec0) / 2)
    _ -> updateRest sid source_
--ss xs = soundscape $ Map.fromList (zip [1 ..] xs)

-}
