{-# LANGUAGE BlockArguments #-}
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

module FRP.Yampa.OpenAL.IO.Update where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Map (Map,difference)
import qualified Data.Map as Map
import Data.Maybe
import Data.StateVar
import FRP.Yampa.OpenAL.Types
import FRP.Yampa.OpenAL.Util
import qualified Sound.OpenAL as AL
import Data.Set (Set)
import qualified Data.Set as Set
import Data.ObjectName
import Data.IORef
import Control.Monad.ST


soundstage :: 
    (MonadIO m) => 
    ALApp -> 
    Maybe Soundstage -> 
    Soundstage -> 
    m ()
soundstage alApp mss0 ss1
    | newSoundstage = updateForce True
    | otherwise     = updateForce False
  where
    newSoundstage = isNothing mss0
    updateForce force = do
        when (force || True) _updateSpeedOfSound
        when (force || True) _updateDistanceModel
        when (force || True) _updateDopplerFactor
        when (force || True) _updateListenerPos
        when (force || True) _updateListenerVel
        when (force || True) _updateListenerOri
        when (force || True) _updateListenerGai
        when (force || True) (_updateSources force)
    _updateSources force = do
        let previousSrcs   :: Set String = undefined -- maybe mempty soundstageSources mss0
        let currentSrcs    :: Set String = undefined
        let deletedSources :: Set String = Set.difference previousSrcs currentSrcs
        unless (null deletedSources) (AL.stop $ undefined deletedSources)
        let createdSources :: Set String = Set.difference currentSrcs previousSrcs 
        unless (null createdSources) do
            liftIO $ _updateFooSources (undefined $ soundstageSources ss1) True createdSources
            let pausedSources :: Set String = Set.filter ((== Paused) . undefined) createdSources
            let stoppedSources :: Set String = Set.filter ((== Stopped) . undefined) createdSources
            liftIO $ unless (null pausedSources) (AL.stop  $ undefined stoppedSources)
            liftIO $ unless (null pausedSources) (AL.pause $ undefined pausedSources)
        let updatedSources :: Set String = Set.intersection previousSrcs currentSrcs
        unless (null updatedSources) do
            let stoppedSources :: Set String = Set.filter ((== Stopped) . undefined) updatedSources
            let pausedSources  :: Set String = Set.filter ((== Paused) . undefined) updatedSources
            liftIO $ unless (null stoppedSources) (AL.stop $ undefined stoppedSources)
            liftIO $ unless (null pausedSources)  (AL.pause $ undefined pausedSources)
            liftIO $ _updateFooSources (undefined $ soundstageSources ss1) force updatedSources
        let playingSources :: Set String = Set.filter ((== Playing) . undefined) currentSrcs -- undefined :: Map String Source
        unless (null playingSources) do
            liftIO $ AL.play (undefined playingSources)
    _updateSpeedOfSound = AL.speedOfSound $= realToFrac (abs (soundstageSpeedOfSound ss1))
    _updateDistanceModel = AL.distanceModel $= soundstageDistanceModel ss1
    _updateDopplerFactor = AL.dopplerFactor $= realToFrac (abs (soundstageDopplerFactor ss1))
    _updateListenerPos = AL.listenerPosition $= _v3ToVertex (listenerPosition $ soundstageListener ss1)
    _updateListenerVel = AL.listenerVelocity $= _v3ToVector (listenerVelocity $ soundstageListener ss1)
    _updateListenerOri = AL.orientation $= bimap _v3ToVector _v3ToVector (listenerOrientation $ soundstageListener ss1)
    _updateListenerGai = AL.listenerGain $= realToFrac (abs (listenerGain (soundstageListener ss1)))
    _updateFooSources sources force sourcePool = do
        srcMap <- liftIO $ takeMVar (sourceMap alApp)
        -- TODO use ST here to pass this map around below
        --runST do 
        --    srcMapSTRef <- undefined
        forM_ sourcePool \sname -> do
                s1 <- fromMaybe (error "bug") (Map.lookup sname sources)
                liftIO do
                    case Map.lookup sname srcMap of
                        Nothing -> do 
                            sid <- genObjectName
                            AL.buffer sid $= Just (sourceBufferQueue s1) 
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
                            writeIORef undefined srcMap
                        Just (_s0,sid,_buff,_revBuff) -> do
                            when (force || True) $ AL.gainBounds sid $= let foox = realToFrac (fst (sourceGainBounds s1)) in (foox, realToFrac (snd (sourceGainBounds s1)))
                            when (force || True) $ AL.coneAngles sid $= (realToFrac $ fst $ sourceConeAngles s1, realToFrac $ snd (sourceConeAngles s1))
                            when (force || True) $ AL.maxDistance sid $= realToFrac (sourceMaxDistance s1)
                            when (force || True) $ AL.sourceRelative sid $= sourceRelative s1
                            when (force || True) $ AL.rolloffFactor sid $= realToFrac (sourceRolloffFactor s1)
                            when (force || True) $ AL.referenceDistance sid $= realToFrac (sourceReferenceDistance s1)
                            when (force || True) $ AL.coneOuterGain sid $= realToFrac (sourceConeOuterGain s1)
                            when (force || True) $ AL.pitch sid $= realToFrac (abs (sourcePitch s1))
                            when (force || True) $ AL.sourcePosition sid $= _v3ToVertex (sourcePosition s1)
                            when (force || True) $ AL.sourceVelocity sid $= _v3ToVector (sourceVelocity s1)
                            when (force || True) $ AL.sourceGain sid $= realToFrac (abs (sourceGain s1))
                            when (force || True) $ AL.direction sid $= _v3ToVector (sourceDirection s1)
                            when force $ AL.secOffset sid $= realToFrac (abs (sourceStartOffset s1))
                            writeIORef undefined srcMap
                pure srcMap


-----------------------------------------------------------

-- | Internal.
data ALApp = ALApp
    { sourceMap :: !(MVar (Map String (Source, AL.Source, AL.Buffer, Maybe AL.Buffer)))
    , createdMap :: !(IORef (Set String))
    }
