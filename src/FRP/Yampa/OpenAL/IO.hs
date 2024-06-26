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
import Data.IORef
import Data.Bifunctor
import FRP.Yampa.OpenAL.Soundstage
import FRP.Yampa.OpenAL.Listener
import FRP.Yampa.OpenAL.Source

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
   Maybe ([AL.ALError] -> IO ()) ->  -- ^ Error handler.
   (ALApp -> m a) -> -- ^ App cont.
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
                Nothing -> soundstageIO alApp Nothing s1
                Just s0 -> soundstageIO alApp (Just s0) s1
            putMVar ssRef (Just s1)
        pure updated

-----------------------------------------------------------
soundstageIO :: 
    (MonadIO m) => 
    ALApp -> 
    Maybe Soundstage -> 
    Soundstage -> 
    m ()
soundstageIO alApp mss0 ss1 = do
    ($=?) AL.speedOfSound     True (abs (realToFrac $ soundstageSpeedOfSound ss1))
    ($=?) AL.distanceModel    True (soundstageDistanceModel ss1)
    ($=?) AL.dopplerFactor    True (abs (realToFrac $ soundstageDopplerFactor ss1))
    ($=?) AL.listenerPosition True (_v3ToVertex (listenerPosition $ soundstageListener ss1))
    ($=?) AL.listenerVelocity True (_v3ToVector (listenerVelocity $ soundstageListener ss1))
    ($=?) AL.orientation      True (bimap _v3ToVector _v3ToVector (listenerOrientation $ soundstageListener ss1))
    ($=?) AL.listenerGain     True (abs (realToFrac $ listenerGain (soundstageListener ss1)))


    case mss0 of
        Nothing -> do
            let sources1 = soundstageSources ss1
            forM_ sources1 \src -> do
                error . show $ sources1
            
        Just ss0 -> do
            let sources0 = soundstageSources ss0
            let sources1 = soundstageSources ss1
            forM_ sources1 \src1 -> do
                case Map.lookup (readSourceID src1) sources0 of
                    Just src0 -> updateSource src0 src1
                    Nothing -> updateSource (emptySource (readSourceID src1)) src1




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
-- signals of the soundstage at some point. (You probably will.)
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
        <$> fmap (Map.fromList . fmap (\x -> (readSourceID x, x))) (drpSwitchB [])
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

{-

    
sourcesByID = undefined
createSource = undefined
deleteSource = undefined
updateSource = undefined
        {-
        let sid = undefined    
        ($=?) (AL.gainBounds sid)         True (let foox = fst (sourceGainBounds s1) in (foox, snd (sourceGainBounds s1)))
        ($=?) (AL.coneAngles sid)         True (realToFrac $ fst $ sourceConeAngles s1, realToFrac $ snd (sourceConeAngles s1))
        ($=?) (AL.maxDistance sid)        True (realToFrac (sourceMaxDistance s1))
        ($=?) (AL.sourceRelative sid)     True (sourceRelative s1)
        ($=?) (AL.rolloffFactor sid)      True (realToFrac (sourceRolloffFactor s1))
        ($=?) (AL.referenceDistance sid)  True (realToFrac (sourceReferenceDistance s1))
        ($=?) (AL.coneOuterGain sid)      True (sourceConeOuterGain s1)
        ($=?) (AL.pitch sid)              True (realToFrac (abs (sourcePitch s1)))
        ($=?) (AL.sourcePosition sid)     True (_v3ToVertex (sourcePosition s1))
        ($=?) (AL.sourceVelocity sid)     True (_v3ToVector (sourceVelocity s1))
        ($=?) (AL.sourceGain sid)         True (realToFrac (abs (sourceGain s1)))
        ($=?) (AL.direction sid)          True (_v3ToVector (sourceDirection s1))
        ($=?) (AL.secOffset sid)          True (realToFrac (abs (sourceStartOffset s1)))
        -}

-----------------------------------------------------------

    -- sid <- genObjectName
    -- AL.buffer sid $= Just (sourceBuffer s1) 
        --let previousSrcs   :: Set String = undefined -- maybe mempty soundstageSources mss0
        --let currentSrcs    :: Set String = undefined
        --let deletedSources :: Set String = Set.difference previousSrcs currentSrcs
        --unless (null deletedSources) (AL.stop $ undefined deletedSources)
        --let createdSources :: Set String = Set.difference currentSrcs previousSrcs 
        --unless (null createdSources) do
        --    liftIO $ _updateFooSources (undefined $ soundstageSources ss1) True createdSources
        --    let pausedSources :: Set String = Set.filter ((== Paused) . undefined) createdSources
        --    let stoppedSources :: Set String = Set.filter ((== Stopped) . undefined) createdSources
        --    liftIO $ unless (null pausedSources) (AL.stop  $ undefined stoppedSources)
        --    liftIO $ unless (null pausedSources) (AL.pause $ undefined pausedSources)
        --let updatedSources :: Set String = Set.intersection previousSrcs currentSrcs
        --unless (null updatedSources) do
        --    let stoppedSources :: Set String = Set.filter ((== Stopped) . undefined) updatedSources
        --    let pausedSources  :: Set String = Set.filter ((== Paused) . undefined) updatedSources
        --    liftIO $ unless (null stoppedSources) (AL.stop $ undefined stoppedSources)
        --    liftIO $ unless (null pausedSources)  (AL.pause $ undefined pausedSources)
        --    liftIO $ _updateFooSources (undefined $ soundstageSources ss1) force updatedSources
        --let playingSources :: Set String = Set.filter ((== Playing) . undefined) currentSrcs -- undefined :: Map String Source
        --unless (null playingSources) do
        --    liftIO $ AL.play (undefined playingSources)
                            --Just (_s0,sid,_buff) -> do
                            --    when (force || True) $ AL.gainBounds sid $= let foox = fst (sourceGainBounds s1) in (foox, snd (sourceGainBounds s1))
                            --    when (force || True) $ AL.coneAngles sid $= (realToFrac $ fst $ sourceConeAngles s1, realToFrac $ snd (sourceConeAngles s1))
                            --    when (force || True) $ AL.maxDistance sid $= realToFrac (sourceMaxDistance s1)
                            --    when (force || True) $ AL.sourceRelative sid $= sourceRelative s1
                            ----    when (force || True) $ AL.rolloffFactor sid $= realToFrac (sourceRolloffFactor s1)
                            --    when (force || True) $ AL.referenceDistance sid $= realToFrac (sourceReferenceDistance s1)
                            --    when (force || True) $ AL.coneOuterGain sid $= sourceConeOuterGain s1
                            --    when (force || True) $ AL.pitch sid $= realToFrac (abs (sourcePitch s1))
                            --    when (force || True) $ AL.sourcePosition sid $= _v3ToVertex (sourcePosition s1)
                            --    when (force || True) $ AL.sourceVelocity sid $= _v3ToVector (sourceVelocity s1)
                            --    when (force || True) $ AL.sourceGain sid $= realToFrac (abs (sourceGain s1))
                            --    when (force || True) $ AL.direction sid $= _v3ToVector (sourceDirection s1)
                            --    when force $ AL.secOffset sid $= realToFrac (abs (sourceStartOffset s1))
                            --    writeIORef undefined srcMap
            --srcMap <- liftIO $ takeMVar (sourceMap alApp)
            -- TODO use ST here to pass this map around below
            --runST do 
            --    srcMapSTRef <- undefined
            --forM_ sourcePool \sname -> do
                    --s1 <- fromMaybe (error "bug") (Map.lookup sname sources)
                    --liftIO do
                        --case Map.lookup sname srcMap of
                         --   Nothing -> do 
                          --      sid <- genObjectName
                           --     AL.buffer sid $= Just (sourceBuffer s1) 
                            --    AL.gainBounds sid $= let foox = fst (sourceGainBounds s1) in (foox, snd (sourceGainBounds s1))
                           --     AL.coneAngles sid $= (realToFrac $ fst $ sourceConeAngles s1, realToFrac $ snd (sourceConeAngles s1))
                            --    AL.maxDistance sid $= realToFrac (sourceMaxDistance s1)
                            --    AL.sourceRelative sid $= sourceRelative s1
                             --   AL.rolloffFactor sid $= realToFrac (sourceRolloffFactor s1)
                              --  AL.referenceDistance sid $= realToFrac (sourceReferenceDistance s1)
                            --    AL.coneOuterGain sid $= sourceConeOuterGain s1
                             --   AL.pitch sid $= realToFrac (abs (sourcePitch s1))
                             --   AL.sourcePosition sid $= _v3ToVertex (sourcePosition s1)
                             --   AL.sourceVelocity sid $= _v3ToVector (sourceVelocity s1)
                             --   AL.sourceGain sid $= realToFrac (abs (sourceGain s1))
                             --   AL.direction sid $= _v3ToVector (sourceDirection s1)
                             --   AL.secOffset sid $= realToFrac (abs (sourceStartOffset s1))
                             --   writeIORef undefined srcMap
                             --   undefined
                    --pure srcMap
                    ---}
    --playingRef <- liftIO (newIORef [])
    --stoppedRef <- liftIO (newIORef [])
    --pausedRef  <- liftIO (newIORef [])
    --playing <- liftIO (readIORef playingRef)
    --AL.play (undefined <$> playing)

    --paused <- liftIO (readIORef pausedRef)
    --AL.pause (undefined <$> paused)

    --stopped <- liftIO (readIORef stoppedRef)
    --AL.stop (undefined <$> stopped)
    -- TODO
    {-
    let allSources = fromMaybe [] (sourcesByID <$> mss0) <> sourcesByID (soundstageSources ss1) -- both from the past and from now, by id
    forM_ allSources \s1 -> do
        case sourceStatus s1 of 
            Created  -> createSource >> updateSource
            Deleted  -> deleteSource
            Modified -> updateSource

        case sourceState s1 of
            Just Playing -> liftIO (modifyIORef playingRef (s1 :))
            Just Stopped -> liftIO (modifyIORef stoppedRef (s1 :))
            Just Paused  -> liftIO (modifyIORef pausedRef  (s1 :))
            ____________ -> pure ()
    -}
