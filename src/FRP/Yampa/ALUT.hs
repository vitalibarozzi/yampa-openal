{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module FRP.Yampa.ALUT
    ( Soundscape(..)
    , Listener(..)
    , Source(..)
    , reactInitALUT
    , source
    , listener
    )
where

import qualified Sound.OpenAL.AL.Listener as AL
import qualified Sound.OpenAL.AL.Buffer as AL
import qualified Sound.OpenAL.AL.Source as AL
import qualified Sound.ALUT.Loaders as AL
import qualified Sound.ALUT.Initialization as AL
import qualified Sound.OpenAL as AL
import qualified Data.ObjectName as ObjectName
import Linear.V2
import Linear.V3
import qualified FRP.Yampa as Yampa
import FRP.Yampa (DTime, SF, Event(..), returnA, ReactHandle, reactInit)
import Data.IORef
import Control.Concurrent
import Control.Monad
import Foreign.Storable
import Data.StateVar
import qualified Data.Map as Map


-----------------------------------------------------------
-- | A model of the how the sound elements change overtime.
data Soundscape = Soundscape
    { soundscapeSources     :: !(Map.Map AL.Source Source)
    , soundscapeListener    :: !Listener
    , soundscapeShouldClose :: !Bool
    }
  deriving
    (Eq, Show)


-----------------------------------------------------------
-- | Like a POV, but for audio. A floating ear.
data Listener = Listener
   { listenerOrientation :: !(V3 Float, V3 Float)
   , listenerPosition    :: !(V3 Float)
   , listenerVelocity    :: !(V3 Float)
   , listenerGain        :: !Float
   }
  deriving
    (Eq, Show)


-----------------------------------------------------------
-- | A source of audio, like a radio, playing stuff from a
-- queue.
data Source = Source
    { sourceID            :: !AL.Source
    , sourcePosition      :: !(V3 Float)
    , sourceVelocity      :: !(V3 Float)
    , sourceGain          :: !Float
    , sourcePitch         :: !Float
    , sourceDirection     :: !(V3 Float)
    , sourceConeAngles    :: !(V2 Float)
    , sourceConeOuterGain :: !Float
    , sourceState         :: !AL.SourceState
    -- , sourceRelative      :: ()
    }
  deriving
    (Eq, Show)


-----------------------------------------------------------
source :: AL.Source -> Source

{-# INLINE source #-}

source sourceID = 
    Source
        { sourceID            = sourceID
        , sourcePosition      = 0
        , sourceVelocity      = 0
        , sourceGain          = 1
        , sourcePitch         = 1.8
        , sourceDirection     = 1
        , sourceConeAngles    = 1
        , sourceConeOuterGain = 1
        , sourceState         = AL.Playing
        -- TODO add relative, loop mode, etc
        }


-----------------------------------------------------------
listener :: Listener

{-# INLINE listener #-}

listener = 
    Listener 
         (V3 0 0 (-1), V3 0 1 0) 
         (V3 0 0 0) 
         (V3 0 0 0) 
         1


-----------------------------------------------------------
reactInitALUT :: 

    s ->

    SF s Soundscape -> 

    IO (ReactHandle s Soundscape)

{-# INLINABLE reactInitALUT #-}

reactInitALUT s sf = do
    loh <- reactInitListenerHelper listenerOrientation  actuateLOH
    lph <- reactInitListenerHelper listenerPosition     actuateLPH
    lvh <- reactInitListenerHelper listenerVelocity     actuateLVH
    lgh <- reactInitListenerHelper listenerGain         actuateLGH
    sxh <- reactInitSourceHelper   0                    actuateSXH
    svh <- reactInitSourceHelper   0                    actuateSVH
    sgh <- reactInitSourceHelper   1                    actuateSGH
    sph <- reactInitSourceHelper   1                    actuateSPH
    sdh <- reactInitSourceHelper   1                    actuateSDH
    sah <- reactInitSourceHelper   1                    actuateSAH
    soh <- reactInitSourceHelper   1                    actuateSOH
    ssh <- reactInitSourceHelper (Map.empty, AL.Playing)actuateSSH
    reactInit (pure s) (actuate (sxh,svh,sgh,sph,sdh,sah,soh,ssh) (lgh,lvh,lph,loh)) sf
  where
    fsv = fooStateVar
    actuateLOH _ updated (shouldClose, (vA, vB)) = fsv updated shouldClose AL.orientation      (v3ToVector vA, v3ToVector vB)
    actuateLPH _ updated (shouldClose,       vA) = fsv updated shouldClose AL.listenerPosition (v3ToVertex vA)
    actuateLVH _ updated (shouldClose,       vA) = fsv updated shouldClose AL.listenerVelocity (v3ToVector vA)
    actuateLGH _ updated (shouldClose,        g) = fsv updated shouldClose AL.listenerGain     (realToFrac g)
    actuateSXH _ updated (shouldClose, msid, sourcePosition) = do 
        case msid of
            Nothing -> pure ()
            Just sid -> when updated do case AL.sourcePosition sid of StateVar get set -> set (v3ToVertex sourcePosition)
        pure shouldClose
    actuateSVH _ updated (shouldClose, msid, sourceVelocity) = do
        case msid of
            Nothing -> pure ()
            Just sid -> when updated do case AL.sourceVelocity sid of StateVar get set -> set (v3ToVector sourceVelocity)
        pure shouldClose
    actuateSGH _ updated (shouldClose, msid, sourceGain) = do
        case msid of
            Nothing -> pure ()
            Just sid -> when updated do case AL.sourceGain sid of StateVar get set -> set (realToFrac sourceGain)
        pure shouldClose
    actuateSPH _ updated (shouldClose, msid, sourcePitch) = do
        case msid of
            Nothing -> pure ()
            Just sid -> when updated do case AL.pitch sid of StateVar get set -> set (realToFrac sourcePitch)
        pure shouldClose
    actuateSDH _ updated (shouldClose, msid, sourceDirection) = do
        case msid of
            Nothing -> pure ()
            Just sid -> when updated do case AL.direction sid of StateVar get set -> set (v3ToVector sourceDirection)
        pure shouldClose
    actuateSAH _ updated (shouldClose, msid, coneAngles) = do
        case msid of
            Nothing -> pure ()
            Just sid -> when updated do case AL.coneAngles sid of StateVar get set -> set (v2ToVectorPair coneAngles)
        pure shouldClose
    actuateSOH _ updated (shouldClose, msid, coneOuterGain) = do
        case msid of
            Nothing -> pure ()
            Just sid -> when updated do case AL.coneOuterGain sid of StateVar get set -> set (realToFrac coneOuterGain)
        pure shouldClose
    actuateSSH _ updated (shouldClose, msid, (sources, sourceState)) = do
        case msid of
            Nothing -> pure ()
            Just sid -> do
                when updated do
                    let usource Source{..} = do
                                 case AL.sourceState sid of 
                                    get -> do
                                        current <- get
                                        if sourceState == current
                                            then return mempty
                                            else case current of
                                                    AL.Initial ->
                                                        case sourceState of
                                                            AL.Playing -> return (mempty { actionPlay = [sid] })
                                                            __________ -> return mempty
                                                    AL.Playing ->
                                                        case sourceState of
                                                            AL.Stopped -> return (mempty { actionStop   = [sid] })
                                                            AL.Playing -> return mempty
                                                            __________ -> undefined
                                                    AL.Paused  ->
                                                        case sourceState of
                                                            AL.Playing -> return (mempty { actionPlay   = [sid] })
                                                            AL.Initial -> return (mempty { actionRewind = [sid] })
                                                            __________ -> undefined
                                                    AL.Stopped ->
                                                        case sourceState of
                                                            AL.Playing -> return (mempty { actionPlay = [sid] })
                                                            __________ -> undefined

                    let concatActions = foldr (<>) action

                    let wsources = forM sources usource

                    runActions =<< (concatActions <$> wsources)

        pure shouldClose

    actuate (sxh,svh,sgh,sph,sdh,sah,soh,ssh) (lgh,lvh,lxh,loh) handle updated Soundscape{..} = do 
        when updated do
            let Listener{..} = soundscapeListener
            Yampa.react loh (0, Just (soundscapeShouldClose, listenerOrientation))
            Yampa.react lxh (0, Just (soundscapeShouldClose, listenerPosition))
            Yampa.react lvh (0, Just (soundscapeShouldClose, listenerVelocity))
            Yampa.react lgh (0, Just (soundscapeShouldClose, listenerGain))
            -- TODO we need to look for the right source?
            let temp = Map.toList soundscapeSources
            forM_ temp \(sid, Source{..}) -> do
                Yampa.react sxh (0, Just (soundscapeShouldClose, Just sid, sourcePosition))
                Yampa.react svh (0, Just (soundscapeShouldClose, Just sid, sourceVelocity))
                Yampa.react sgh (0, Just (soundscapeShouldClose, Just sid, realToFrac sourceGain))
                Yampa.react sph (0, Just (soundscapeShouldClose, Just sid, realToFrac sourcePitch))
                Yampa.react sdh (0, Just (soundscapeShouldClose, Just sid, sourceDirection))
                Yampa.react sah (0, Just (soundscapeShouldClose, Just sid, sourceConeAngles))
                Yampa.react soh (0, Just (soundscapeShouldClose, Just sid, realToFrac sourceConeOuterGain))
                Yampa.react ssh (0, Just (soundscapeShouldClose, Just sid, (soundscapeSources, sourceState)))
        pure soundscapeShouldClose


    fooStateVar updated shouldClose get_ set_ = do 
        when updated do case get_ of StateVar _ set -> set set_
        pure shouldClose

    reactInitListenerHelper component actuate = 
        reactInit (pure (False, component listener)) actuate Yampa.identity

    reactInitSourceHelper component actuate = 
        reactInit (pure (False, Nothing,component)) actuate Yampa.identity


    
-------------------------------------------------------------------------------
-- INTERNALS ------------------------------------------------------------------
-------------------------------------------------------------------------------
    

------------------------------------------------------------
data Action = Action
    { actionPlay   :: [AL.Source]
    , actionStop   :: [AL.Source]
    , actionPause  :: [AL.Source]
    , actionRewind :: [AL.Source]
    }
  deriving
    (Eq, Show)

instance Semigroup Action where
    Action pl0 st0 pa0 re0 <> Action pl1 st1 pa1 re1 =
        Action (pl0 <> pl1) (st0 <> st1) (pa0 <> pa1) (re0 <> re1)

instance Monoid Action where
    mempty = action


------------------------------------------------------------
action :: Action
action = Action [] [] [] []


------------------------------------------------------------
runActions :: Action -> IO ()
runActions Action{..} = do
    AL.stop   actionStop
    AL.pause  actionPause
    AL.play   actionPlay
    AL.rewind actionRewind


------------------------------------------------------------
v3ToVertex :: V3 Float -> AL.Vertex3 AL.ALfloat
v3ToVertex (V3 x y z) = 
    AL.Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)


------------------------------------------------------------
v3ToVector :: V3 Float -> AL.Vector3 AL.ALfloat
v3ToVector (V3 x y z) =
    AL.Vector3 (realToFrac x) (realToFrac y) (realToFrac z)


------------------------------------------------------------
v2ToVectorPair :: V2 Float -> (AL.ALfloat, AL.ALfloat)
v2ToVectorPair (V2 a b) =
    (realToFrac a, realToFrac b)
