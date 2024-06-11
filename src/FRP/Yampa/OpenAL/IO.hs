{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FRP.Yampa.OpenAL.IO
where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.StateVar
import FRP.Yampa.OpenAL.Types
import FRP.Yampa.OpenAL.Util
import qualified Sound.ALUT.Initialization as AL
import qualified Sound.OpenAL as AL
import System.Timeout

-----------------------------------------------------------

-- | Internal.
type ALApp = ()

-- TODO needs to contain source and buffer maps or caches

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
updateSoundstage :: (MonadIO m) => ALApp -> Soundstage -> Soundstage -> m ()
{-# INLINE updateSoundstage #-}
updateSoundstage app ss1 ss0 = do
    when True updateSpeedOfSound
    when True updateDistanceModel
    when True updateDopplerFactor
    when True updateListenerPos
    when True updateListenerVel
    when True updateListenerOri
    when True updateListenerGai
    when True updateSources
  where
    updateSpeedOfSound = AL.speedOfSound $= realToFrac (soundstageSpeedOfSound ss1)
    updateDistanceModel = AL.distanceModel $= soundstageDistanceModel ss1
    updateDopplerFactor = AL.dopplerFactor $= realToFrac (soundstageDopplerFactor ss1)
    updateListenerPos = AL.listenerPosition $= _v3ToVertex (soundstageListenerPosition ss1)
    updateListenerVel = AL.listenerVelocity $= _v3ToVector (soundstageListenerVelocity ss1)
    updateListenerOri = AL.orientation $= bimap _v3ToVector _v3ToVector (soundstageListenerOrientation ss1)
    updateListenerGai = AL.listenerGain $= realToFrac (soundstageListenerGain ss1)
    -- TODO
    updateSources = forM_ ([] {-undefined ss1 ss0-}) (uncurry (updateSource app))

-----------------------------------------------------------
updateListener = undefined

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

    updateState = case sourceState s1 of
        AL.Playing -> AL.play [undefined]
        __________ -> return ()
    updatePitch = case sourceState s1 of
        AL.Playing -> AL.play [undefined]
        __________ -> return ()

{-

            sourceWasCreated = do
                undefined
            --                if sid == nullSource alApp
            ----                    then do
            --                        let soundData = dataConvert (sourceSoundData source_)
            --                        bufferx <- case Map.lookup (sourceSoundData source_) buffers of
            --                            Nothing -> ALUT.createBuffer soundData
            --                            Just bufferx -> return bufferx
            --                        !src <- ObjectName.genObjectName
            --                        AL.buffer src $= Just bufferx
            --                        AL.play [src]
            --                        AL.loopingMode src $= dataLooping (sourceSoundData source_)
            --                        modifyMVar_ (alAppBuffers alApp) (pure . Map.insert (sourceSoundData source_) bufferx)
            --                        modifyMVar_ (alAppSources alApp) (\sources__ -> pokeElemOff sources__ n src >> pure sources__)

            sourceWasModified = do
                undefined
            --                sources <- readMVar (alAppSources alApp)
            --                sid <- peekElemOff sources n
            --                    else do
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
            --
            -- TODO we need both the dt from the soundscape
            --                  and the dt from the source
            --                  so if there is a difference in rate
            --                  we change the pitch accordingly
            --
            -- TODO AL.pitch sid $= realToFrac (sourcePitch source_)

data ALApp = ALApp
    { --alAppBuffers :: !(MVar (Map DataSource AL.Buffer)) -- fat cache, good for now
      alAppSources :: !(MVar (Ptr AL.Source)) -- associates ints with al.sources
    , alAppSourceGroups :: !(MVar (Ptr AL.Source)) -- associates ints with al.sources
    , alAppNullSource :: AL.Source
    }

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
    -- File{} -> AL.OneShot
    -- Memory{} -> AL.OneShot
    -- HelloWorld -> AL.OneShot
    WhiteNoise -> AL.Looping
    Sine{} -> AL.Looping
    Square{} -> AL.Looping
    Sawtooth{} -> AL.Looping
    Impulse{} -> AL.Looping
            let new = Map.difference (soundscapeSources s1) (soundscapeSources s0)
            let gone = Map.difference (soundscapeSources s0) (soundscapeSources s1)
            let rest = undefined
            !sources <- readMVar (alAppSources alApp)
            !buffers <- readMVar (alAppBuffers alApp)

            ________ <- undefined -- freeDeadSources (soundscapeSources s1) sources -- TODO

            -- forConcurrently_ (zip (Map.toList (soundscapeSources s0)) (Map.toList (soundscapeSources s1))) $
            --   \((isid0, source0),(isid1, source1)) -> do

            forConcurrently_ (Map.toList (soundscapeSources s0)) \(!isid, !source_) -> do
                -- TODO dont turn it into a list
                -- check if this source exists in the past
                case undefined of -- Map.lookup isid sources of -- TODO
                    Nothing -> undefined -- withNewSource buffers source_
                    Just !sid -> do
                        sameSoundData_ sid source_
                        soundDataChanged sid source_
                        sameSoundData_ sid source_
                        soundDataChanged sid source_

        withNewSource buffers source_ = do
            case dataConvert (head $ sourceSoundData source_) of
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

        soundDataChanged sid source_ = do
            AL.stop [sid]
            case dataConvert (head $ sourceSoundData source_) of
                Nothing -> pure ()
                Just !csource -> do
                    !bufferx <- ALUT.createBuffer csource
                    AL.buffer sid $= Just bufferx
                    AL.play [sid]
                    modifyMVar_ (alAppBuffers alApp) (pure . Map.insert (head $ sourceSoundData source_) bufferx)

        sameSoundData_ sid source_ = do
            case sourceSoundData source_ of
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
        --let pos = soundscapeSourceGroups prev /= soundscapeSourceGroups next_
        --returnA -< (if pos then Event (listenerPosition (soundscapeListener next_)) else NoEvent, next_)
            --        buffers <- readMVar (alAppBuffers alApp)
            --        forM_ (soundscapeSources ss1) \sourceGroup_ -> do
            --            forM_ (Map.toList sourceGroup_) \(n, source_) -> do

tick :: IORef Integer -> IO ()
tick ref = getCPUTime >>= writeIORef ref

tack :: IORef Integer -> IO Double
tack ref = do
    t1 <- getCPUTime
    t0 <- readIORef ref
    return (realToFrac (t1 - t0) / 10000)
--ss xs = soundscape $ Map.fromList (zip [1 ..] xs)

-}
