module FRP.Yampa.OpenAL.Soundstage (
    Soundstage (..),
    updateSoundstage,
    soundstage_,
    soundstage,
)
where

import Control.Concurrent ()
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef ()
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe ()
import Data.StateVar
import FRP.Yampa (
    Arrow (arr),
    Event (..),
    SF,
    Time,
    drpSwitchB,
    identity,
    (&&&),
    (<<<),
 )
import qualified FRP.Yampa as Yampa
import FRP.Yampa.OpenAL.Listener
import FRP.Yampa.OpenAL.Source (
    Source,
    emptySource,
    sourceID,
    updateSource,
 )
import FRP.Yampa.OpenAL.Types (Factor, MetersPerSecond)
import FRP.Yampa.OpenAL.Util (appName, setWhen)
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
    , soundstageTime {-----------} :: !Time
    }

-----------------------------------------------------------

{- | Constructor with default values. To be used when you
 - don't need to change the sources signals of the soundstage
 - during its execution.
-}
soundstage ::
    Map AL.Source (SF a Source) ->
    SF a Soundstage
{-# INLINE soundstage #-}
soundstage sources =
    soundstage_
        sources
        1
        AL.InverseDistance
        0
        0
        1
        (const NoEvent)

-----------------------------------------------------------
-- For when you want to change the collection of source
-- signals of the soundstage at some point. (You probably will.)
-- TODO add maybes so we can create this using default values if needed
soundstage_ ::
    Map AL.Source (SF a Source) ->
    Float ->
    AL.DistanceModel ->
    V3 Float ->
    V3 Float ->
    Double ->
    (a -> Event (Map AL.Source (SF a Source) -> Map AL.Source (SF a Source))) ->
    SF a Soundstage
{-# INLINE soundstage_ #-}
soundstage_ initialSources factor model pos vel gain k =
    Soundstage
        <$> (drpSwitchB initialSources <<< (identity &&& arr k))
        <*> pure factor
        <*> pure 343.3
        <*> pure model
        <*> listener_ (Just pos) (Just vel) (Just (V3 0 0 (-1), V3 0 1 0)) (Just (realToFrac gain))
        <*> Yampa.time

-----------------------------------------------------------

{- | Updates only what have changed in the Soundstage. Also
handles OpenAL errors.
-}
updateSoundstage ::
    (MonadIO m) =>
    Maybe Soundstage ->
    Soundstage ->
    m ()
{-# INLINEABLE updateSoundstage #-}
updateSoundstage mss0 ss1 = do
    case mss0 of
        Nothing -> do
            ($=) AL.speedOfSound (abs (realToFrac $ soundstageSpeedOfSound ss1))
            ($=) AL.distanceModel (soundstageDistanceModel ss1)
            ($=) AL.dopplerFactor (abs (realToFrac $ soundstageDopplerFactor ss1))
            let sources1 = soundstageSources ss1
            updateListener Nothing (soundstageListener ss1)
            forM_ sources1 $ \src1 -> updateSource (emptySource (sourceID src1)) src1
        Just ss0 -> do
            setWhen AL.speedOfSound (soundstageSpeedOfSound ss1 /= soundstageSpeedOfSound ss0) (abs (realToFrac $ soundstageSpeedOfSound ss1))
            setWhen AL.distanceModel (soundstageDistanceModel ss1 /= soundstageDistanceModel ss0) (soundstageDistanceModel ss1)
            setWhen AL.dopplerFactor (soundstageDopplerFactor ss1 /= soundstageDopplerFactor ss0) (abs (realToFrac $ soundstageDopplerFactor ss1))
            updateListener (Just (soundstageListener ss0)) (soundstageListener ss1)
            forM_ (soundstageSources ss1) $ \src1 -> do
                let sources0 = soundstageSources ss0
                case Map.lookup (sourceID src1) sources0 of
                    Just src0 -> updateSource src0 src1
                    Nothing -> updateSource (emptySource (sourceID src1)) src1
    liftIO $ internalErrorHandler =<< AL.alErrors
  where
    internalErrorHandler errors = do
        unless (null errors) (error . show $ (appName <> show errors))
