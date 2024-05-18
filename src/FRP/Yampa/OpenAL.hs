{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module FRP.Yampa.OpenAL
    ( -- *
      Soundscape(..)
    , Listener(..)
    , Source(..)
    -- , SourceRelative(..)
    -- , SourceState(..)

    -- * Constructors
    , soundscape
    , source
    , listener

    -- *
    , reactInitOpenAL
    
    , runSoundscape
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
import Data.TreeDiff.Expr
import Data.TreeDiff.Class
import Data.TreeDiff.Pretty
import Data.TreeDiff.OMap
import GHC.Generics


-----------------------------------------------------------
-- | A model of the how the sound elements change overtime.
data Soundscape = Soundscape
    { soundscapeSources       :: !(Map.Map AL.Source Source)
    , soundscapeListener      :: !Listener
    , soundscapeShouldClose   :: !Bool
    , soundscapeDopplerFactor :: !Float
    , soundscapeSpeedOfSound  :: !Float
    , soundscapeDistanceModel :: !() -- TODO
    }
  deriving
    (Eq, Show) -- , Generic, ToExpr)


-----------------------------------------------------------
-- | Like a POV, but for audio. A floating ear.
data Listener = Listener
   { listenerOrientation :: !(V3 Float, V3 Float)
   , listenerPosition    :: !(V3 Float)
   , listenerVelocity    :: !(V3 Float)
   , listenerGain        :: !Float
   }
  deriving
    (Eq, Show) -- , Generic, ToExpr)


-----------------------------------------------------------
-- | A source of audio playing stuff from a queue.
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
    , sourceRelative      :: !AL.SourceRelative
    }
  deriving
    (Eq, Show) -- , Generic, ToExpr)


-----------------------------------------------------------
instance ToExpr  (V3 Float)


-----------------------------------------------------------
instance ToExpr  (V2 Float)


-----------------------------------------------------------
-- | Constructor.
soundscape :: Soundscape
{-# INLINE soundscape #-}
soundscape = 
    Soundscape 
        mempty
        listener
        False
        1
        1
        ()


-----------------------------------------------------------
-- | Constructor.
listener :: Listener
{-# INLINE listener #-}
listener = 
    Listener 
         (V3 0 0 (-1), V3 0 1 0) 
         (V3 0 0 0) 
         (V3 0 0 0) 
         1


-----------------------------------------------------------
-- | Constructor.
source :: AL.Source -> Source
{-# INLINE source #-}
source sourceID = 
    Source
        { sourceID            = sourceID
        , sourcePosition      = 0
        , sourceVelocity      = 0
        , sourceGain          = 1
        , sourcePitch         = 1
        , sourceDirection     = 1
        , sourceConeAngles    = 1
        , sourceConeOuterGain = 1
        , sourceState         = AL.Playing
        , sourceRelative      = AL.World
        }


-----------------------------------------------------------
-- | Assumes ALUT was already started.
runSoundscape ::

    Soundscape ->

    IO ()

{-# INLINEABLE runSoundscape #-}

runSoundscape s1 = do
    threadDelay 1
    let Listener{..} = soundscapeListener s1           
    let (vA,vB) = listenerOrientation
    AL.orientation      $= (_v3ToVector vA, _v3ToVector vB)
    AL.listenerPosition $= _v3ToVertex vA
    AL.listenerVelocity $= _v3ToVector vA
    AL.listenerGain     $= realToFrac listenerGain
    forM_ (Map.toList (soundscapeSources s1)) \(sid,source) -> do
        threadDelay 1
        AL.sourcePosition sid $= _v3ToVertex (sourcePosition source)
        AL.sourceVelocity sid $= _v3ToVector (sourceVelocity source)
        AL.pitch          sid $= (realToFrac (sourcePitch source))
        AL.sourceGain     sid $= realToFrac (sourceGain  source)
        AL.direction      sid $= _v3ToVector (sourceDirection source)
        AL.sourceRelative sid $= sourceRelative source
        AL.coneAngles     sid $= _v2ToVectorPair (sourceConeAngles source)
        AL.coneOuterGain  sid $= realToFrac (sourceConeOuterGain source)
        current <- AL.sourceState sid
        case (sourceState source, current) of
            (AL.Playing, AL.Playing) -> pure ()
            (AL.Playing, __________) -> AL.play [sid]
            (AL.Stopped, AL.Stopped) -> pure ()
            (AL.Stopped, __________) -> AL.stop [sid]
            (AL.Initial, AL.Initial) -> pure ()
            (AL.Initial, __________) -> AL.stop [sid]


-----------------------------------------------------------
-- | Assumes ALUT was already started.
reactInitOpenAL :: 

    IO s -> 

    SF s Soundscape -> 

    IO (ReactHandle s Soundscape)

{-# INLINABLE reactInitOpenAL #-}

reactInitOpenAL initS = do
    reactInit initS actuate
  where
    actuate handle updated ss = do
        runSoundscape ss
        pure updated


------------------------------------------------------------
-- | Number conversion thing.
_v3ToVertex :: V3 Float -> AL.Vertex3 AL.ALfloat
{-# INLINE _v3ToVertex #-}
_v3ToVertex (V3 x y z) = 
    AL.Vertex3 
        (realToFrac x) 
        (realToFrac y) 
        (realToFrac z)


------------------------------------------------------------
-- | Number conversion thing.
_v3ToVector :: V3 Float -> AL.Vector3 AL.ALfloat
{-# INLINE _v3ToVector #-}
_v3ToVector (V3 x y z) =
    AL.Vector3 
        (realToFrac x) 
        (realToFrac y) 
        (realToFrac z)


------------------------------------------------------------
-- | Number conversion thing.
_v2ToVectorPair :: V2 Float -> (AL.ALfloat, AL.ALfloat)
{-# INLINE _v2ToVectorPair #-}
_v2ToVectorPair (V2 a b) =
    ( realToFrac a
    , realToFrac b
    )

{-
    case exprDiff (toExpr s0) (toExpr s1) of
        Cpy a -> 
           case a of
                EditRec constructorName omap          -> mapM_ handleField (toList omap)
  where
    handleField = \case
        ("soundscapeSources" , expr) -> handleSources  expr
        ("soundscapeListener", expr) -> handleListener expr
        ____________________________ -> pure ()

    handleSources = \case-- error . show $ prettyEditExpr a-- \case
        Cpy a   -> 
           case a of
                EditApp "Map.fromList"  editEditExprs -> mapM_ handleSource editEditExprs

    handleSource = \case
        Cpy a   -> 
           case a of
                --EditApp constructorName  editEditExprs -> error . show $ editEditExprs
                EditLst editEditExprs	               -> mapM_ handleSourceFields editEditExprs
      where
        handleSourceFields = \case
            Cpy a   ->
                case a of
                    EditApp cn xs -> error "editApp"
                    EditExp expr -> pure ()
            Ins a -> 
                case a of
                    EditExp expr ->
                        case expr of
                            App _ xs -> 
                                forM_ xs \case
                                    App _ _ -> pure ()
                                    Rec constructorName fields -> mapM_ (handleField a) (toList fields)
          where
            handleField sid = \case
                ("sourceID"            , expr) -> pure ()
                ("sourcePosition"      , expr) -> AL.sourcePosition (unsafeCoerce 0) $= (AL.Vertex3 0 0 0)
                ("sourceVelocity"      , expr) -> AL.sourceVelocity (unsafeCoerce 0) $= (AL.Vertex3 0 0 0)
                ("sourceGain"          , expr) -> AL.sourceGain (unsafeCoerce 0) $= (AL.Vertex3 0 0 0)
                ("sourcePitch"         , expr) -> pure ()
                ("sourceDirection"     , expr) -> pure ()
                ("sourceConeAngles"    , expr) -> pure ()
                ("sourceConeOuterGain" , expr) -> pure ()
                ("sourceState"         , expr) -> pure ()
                ("sourceRelative"      , expr) -> pure ()
            
    handleListener = \case
        _ -> do
            let Listener{..} = soundscapeListener s1           
            let (vA,vB) = listenerOrientation
            AL.orientation      $= (_v3ToVector vA, _v3ToVector vB)
            AL.listenerPosition $= _v3ToVertex vA
            AL.listenerVelocity $= _v3ToVector vA
            AL.listenerGain     $= _realToFrac listenerGain
-----------------------------------------------------------
--data SourceState
--    = Initial
----    | Playing
--    | Paused
--    | Stopped
--  deriving
--    (Eq, Show, Generic, ToExpr)


-----------------------------------------------------------
--data SourceRelative 
--    = RelativeToListener 
----    | RelativeToWorld
--  deriving
--    (Eq, Show, Generic, ToExpr)
-}

