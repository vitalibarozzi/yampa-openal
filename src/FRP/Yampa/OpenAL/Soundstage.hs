{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module FRP.Yampa.OpenAL.Soundstage
where

import Control.Concurrent ()
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import Data.Bifunctor (Bifunctor (bimap))
import Data.IORef ()
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe ()
import FRP.Yampa (
    Arrow (arr),
    Event (..),
    SF,
    Time,
    drpSwitchB,
    initially,
    (<<<),
 )
import qualified FRP.Yampa as Yampa
import FRP.Yampa.OpenAL.Listener
import FRP.Yampa.OpenAL.Source (
    Source,
    emptySource,
    readSourceID,
    updateSource,
 )
import FRP.Yampa.OpenAL.Types (Factor, MetersPerSecond)
import FRP.Yampa.OpenAL.Util (($=?), _v3ToVector, _v3ToVertex)
import Linear as L (V3 (..))
import qualified Sound.OpenAL as AL

-----------------------------------------------------------

{- | A model of the how the sound elements are defined at
one specific point in time.
-}
data Soundstage = Soundstage
    { soundstageSources {--------} :: !(Map AL.Source Source)
    , soundstageDopplerFactor {--} :: !Factor
    , soundstageSpeedOfSound {---} :: !MetersPerSecond
    , soundstageDistanceModel {--} :: !AL.DistanceModel
    , soundstageListener {-------} :: !Listener
    , soundstageTime {-----------} :: !(Maybe Time)
    }

-----------------------------------------------------------
updateSoundstage ::
    (MonadIO m) =>
    Maybe Soundstage ->
    Soundstage ->
    m ()
updateSoundstage mss0 ss1 = do
    ($=?) AL.speedOfSound True (abs (realToFrac $ soundstageSpeedOfSound ss1))
    ($=?) AL.distanceModel True (soundstageDistanceModel ss1)
    ($=?) AL.dopplerFactor True (abs (realToFrac $ soundstageDopplerFactor ss1))
    ($=?) AL.listenerPosition True (_v3ToVertex (listenerPosition $ soundstageListener ss1))
    ($=?) AL.listenerVelocity True (_v3ToVector (listenerVelocity $ soundstageListener ss1))
    ($=?) AL.orientation True (bimap _v3ToVector _v3ToVector (listenerOrientation $ soundstageListener ss1))
    ($=?) AL.listenerGain True (abs (realToFrac $ listenerGain (soundstageListener ss1)))
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
