{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
-- | No supporte for Capture.
module FRP.Yampa.OpenAL
    ( -- *
      Soundscape(..)
    , Listener(..)
    , Source(..)

    -- * Constructors
    , soundscape
    , source
    , listener

    -- *
    , runSoundscape
    , AL.runALUT
    )
where

import qualified Sound.OpenAL.AL.Listener as AL
import qualified Sound.OpenAL.AL.Attenuation as AL
import qualified Sound.ALUT.Initialization as AL
import qualified Sound.ALUT.Loaders as AL
import qualified Sound.OpenAL.AL.Source as AL
import qualified Sound.OpenAL as AL
import Linear.V2
import Linear.V3
import Control.Monad
import Data.StateVar
import qualified Data.Map as Map
import Data.Map (Map)
import Data.IORef


-----------------------------------------------------------
-- | A model of the how the sound elements change overtime.
data Soundscape = Soundscape
    { soundscapeListener      :: !Listener
    , soundscapeDopplerFactor :: !Float -- 
    , soundscapeSpeedOfSound  :: !Float
    , soundscapeDistanceModel :: !AL.DistanceModel
    , soundscapeSources       :: !(Map Int Source)
    , soundscapeShouldClose   :: !Bool
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
-- | A source of audio in space.
data Source = Source
    { sourcePosition          :: !(V3 Float)
    , sourceVelocity          :: !(V3 Float)
    , sourceGain              :: !Float
    , sourceGainBounds        :: !(Float, Float)
    , sourcePitch             :: !Float
    , sourceDirection         :: !(V3 Float)
    , sourceConeAngles        :: !(Float, Float) -- Outer cone, inner cone, in degrees.
    , sourceConeOuterGain     :: !Float          -- Gain on the outer cone
    , sourceState             :: !AL.SourceState -- TODO in lose more than we gain by using this instead of mirroring our own since we dont wanna expose anything from the backend anyway
    , sourceRelative          :: !AL.SourceRelative
    , sourceRolloffFactor     :: !Float
    , sourceReferenceDistance :: !Float
    , sourceMaxDistance       :: !Float
    , sourceSoundData         :: !(AL.SoundDataSource ())
    }
  deriving
    (Eq, Show)


-----------------------------------------------------------
-- | Constructor with default values.
soundscape :: Soundscape
{-# INLINE soundscape #-}
soundscape = 
    Soundscape 
        { soundscapeListener      = listener
        , soundscapeDopplerFactor = 1
        , soundscapeSpeedOfSound  = 343.3
        , soundscapeDistanceModel = AL.InverseDistance
        , soundscapeSources       = mempty
        , soundscapeShouldClose   = False
        }


-----------------------------------------------------------
-- | Constructor with default values.
listener :: Listener
{-# INLINE listener #-}
listener = 
    Listener 
       { listenerOrientation = (V3 0 0 (-1), V3 0 1 0) 
       , listenerPosition    = V3 0 0 0
       , listenerVelocity    = V3 0 0 0
       , listenerGain        = 1
       }


-----------------------------------------------------------
-- | Constructor.
source :: AL.SoundDataSource () -> Source
{-# INLINE source #-}
source sd = 
    Source
        { sourcePosition          = V3 0 0 0
        , sourceVelocity          = V3 0 0 0
        , sourceGain              = 1
        , sourceGainBounds        = (0,1)
        , sourcePitch             = 1
        , sourceDirection         = V3 0 0 0
        , sourceConeAngles        = (360,360)
        , sourceConeOuterGain     = 1
        , sourceState             = AL.Playing
        , sourceRelative          = AL.World
        , sourceRolloffFactor     = 1
        , sourceReferenceDistance = 1
        , sourceMaxDistance       = 10000
        , sourceSoundData         = sd
        }


--withALUT :: (ALApp -> IO a) -> IO a 
--withALUT k = do
--    AL.runALUT "testing-yampa-alut" [] \x y -> do
--       k alApp
data ALApp = ALApp 
    { alutInitiated :: Bool
    , stateRef      :: IORef () 
    -- TODO we need bookkeeping for the sources
    --      and we need a chace or middleware for the buffers
    }


-----------------------------------------------------------
-- | Assumes ALUT was already started.
runSoundscape :: ALApp -> Soundscape -> IO ()
{-# INLINEABLE runSoundscape #-}
runSoundscape alApp s1 = do
    let Listener{..} = soundscapeListener s1           
    let (vA,vB) = listenerOrientation
    AL.orientation      $= (_v3ToVector vA, _v3ToVector vB)
    AL.listenerPosition $= _v3ToVertex vA
    AL.listenerVelocity $= _v3ToVector vA
    AL.listenerGain     $= realToFrac listenerGain

    -- TODO
    let getSourceFromID = undefined alApp -- TODO

    forM_ (Map.toList (soundscapeSources s1)) \(isid, source_) -> do -- note: may be parallelizable
        let sid = getSourceFromID isid
        AL.sourcePosition sid $= _v3ToVertex (sourcePosition source_)
        AL.sourceVelocity sid $= _v3ToVector (sourceVelocity source_)
        AL.pitch          sid $= realToFrac (sourcePitch source_)
        AL.sourceGain     sid $= realToFrac (sourceGain  source_)
        --, sourceGainBounds        :: !(Float, Float)
        AL.direction      sid $= _v3ToVector (sourceDirection source_)
        AL.sourceRelative sid $= sourceRelative source_
        --, sourceRolloffFactor     :: !Float
        --, sourceReferenceDistance :: !Float
        --, sourceMaxDistance       :: !Float
        AL.coneAngles     sid $= (realToFrac $ fst $ sourceConeAngles source_, realToFrac $ snd (sourceConeAngles source_))
        AL.coneOuterGain  sid $= realToFrac (sourceConeOuterGain source_)
    -- sourceSoundData         :: !(AL.SoundDataSource ())
        -- TODO data
            -- client stuff
        -- TODO we need to solve this in here
        --helloSource <- ObjectName.genObjectName
        --bufferDS    <- AL.createBuffer AL.HelloWorld
        --()          <- AL.buffer helloSource StateVar.$= Just bufferDS
        current <- AL.sourceState sid
        case (sourceState source_, current) of
            (AL.Playing, AL.Playing) -> pure ()
            (AL.Playing, __________) -> AL.play [sid]
            (AL.Stopped, AL.Stopped) -> pure ()
            (AL.Stopped, __________) -> AL.stop [sid]
            (AL.Initial, AL.Initial) -> pure ()
            (AL.Initial, __________) -> AL.stop [sid]
            (AL.Paused , AL.Playing) -> AL.pause [sid]
            (AL.Paused , __________) -> pure ()
    AL.dopplerFactor    $= realToFrac (soundscapeDopplerFactor s1)
    AL.speedOfSound     $= realToFrac (soundscapeSpeedOfSound s1)
    AL.distanceModel    $= soundscapeDistanceModel s1


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
