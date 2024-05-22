{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}


module FRP.Yampa.OpenAL
    ( -- *
      Soundscape(..)
    , Listener(..)
    , Source(..)
    , DataSource(..)

    -- * Constructors
    , soundscape
    , source
    , listener

    -- *
    , runSoundscape
    , withALUT
    )
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Map (Map)
import Data.Maybe
import Data.StateVar
import Foreign.Ptr
import Linear.V2
import Linear.V3
import Unsafe.Coerce
import qualified Data.Map as Map
import qualified Data.ObjectName as ObjectName
import qualified Sound.ALUT.Initialization as ALUT
import qualified Sound.ALUT.Loaders as ALUT
import qualified Sound.OpenAL as AL


-------------------------------------------------------------------------------
-- PUBLIC ---------------------------------------------------------------------
-------------------------------------------------------------------------------


-----------------------------------------------------------
-- | A model of the how the sound elements change overtime.
data Soundscape = Soundscape
    { soundscapeListener      :: !Listener
    , soundscapeDopplerFactor :: !Float -- 
    , soundscapeSpeedOfSound  :: !MetersPerSecond
    , soundscapeDistanceModel :: !AL.DistanceModel
    , soundscapeSources       :: !(Map Int Source)
    , soundscapeShouldClose   :: !Bool
    }
  deriving
    (Eq, Show)


-----------------------------------------------------------
-- | Like a POV, but for audio. A floating ear.
data Listener = Listener
   { listenerOrientation :: !(V3 Meters, V3 Meters)
   , listenerPosition    :: !(V3 Meters)
   , listenerVelocity    :: !(V3 MetersPerSecond)
   , listenerGain        :: !Float
   }
  deriving
    (Eq, Show)


-----------------------------------------------------------
-- | A source of audio in space.
data Source = Source

    { sourcePosition          :: !(V3 Meters)
    , sourceVelocity          :: !(V3 MetersPerSecond)
    , sourceDirection         :: !(V3 Meters)
    , sourceRelative          :: !AL.SourceRelative

    , sourceGain              :: !Float
    , sourceGainBounds        :: !(Float, Float)

    , sourceConeAngles        :: !(Float, Float) -- Outer cone, inner cone, in degrees.
    , sourceConeOuterGain     :: !Float          -- Gain on the outer cone

    , sourceRolloffFactor     :: !Float
    , sourceReferenceDistance :: !Meters
    , sourceMaxDistance       :: !Meters

    , sourcePitch             :: !Float
    , sourceSoundData         :: !DataSource
    }
  deriving
    (Eq, Show)


-----------------------------------------------------------
data DataSource
    = Silence

    | HelloWorld
    | WhiteNoise

    | Sine     !Hz !Phase
    | Square   !Hz !Phase
    | Sawtooth !Hz !Phase
    | Impulse  !Hz !Phase

    | Memory !(Ptr ()) !Int !(Maybe Seconds)
    | File   !FilePath      !(Maybe Seconds)
    -- TODO | Signal Hz (SF () Float) -- lazily creates buffer as needed

  deriving
    (Eq, Show, Ord)


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
source :: DataSource -> Source
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
        , sourceRelative          = AL.World
        , sourceRolloffFactor     = 1
        , sourceReferenceDistance = 1
        , sourceMaxDistance       = 10000
        , sourceSoundData         = sd
        }


-----------------------------------------------------------
withALUT :: (ALApp -> IO a) -> IO a 
{-# INLINE withALUT #-}
withALUT !k = do
    ALUT.runALUT "Yampa-OpenAL" [] \_name _arguments -> do
       !buffers <- newMVar mempty
       !dataSrc <- newMVar mempty
       !sources <- newMVar mempty
       ________ <- forkIO (forever runErrors)
       k (ALApp buffers dataSrc sources)
  where
    runErrors = do
        threadDelay 1010101
        !errors <- AL.alErrors
        unless (null errors) (print errors)


-----------------------------------------------------------
-- | Assumes ALUT was already started.
runSoundscape :: ALApp -> Soundscape -> IO ()
{-# INLINEABLE runSoundscape #-}
runSoundscape !alApp s1 = do

    threadDelay 1
    void (concurrently runSources (runMisc >> runListener))

  where 

    runMisc = do
        AL.dopplerFactor $= realToFrac (soundscapeDopplerFactor s1)
        AL.speedOfSound  $= realToFrac (soundscapeSpeedOfSound s1)
        AL.distanceModel $= soundscapeDistanceModel s1

    runListener = do
        let Listener{..} = soundscapeListener s1           
        let (vA,vB) = listenerOrientation
        AL.orientation      $= (_v3ToVector vA, _v3ToVector vB)
        AL.listenerPosition $= _v3ToVertex vA
        AL.listenerVelocity $= _v3ToVector vA
        AL.listenerGain     $= realToFrac listenerGain

    runSources = do
        !dataSrcs <- readMVar (alAppDataSource alApp)
        !sources  <- readMVar (alAppSources    alApp)
        _________ <- freeDeadSources (soundscapeSources s1) sources
        forConcurrently_ (Map.toList (soundscapeSources s1)) \(!isid, !source_) -> do
            let sameSoundData = Just (sourceSoundData source_) == Map.lookup isid dataSrcs
            case Map.lookup isid sources of
               -- creates source
               Nothing -> do
                   case dataConvert (sourceSoundData source_) of
                       Nothing -> pure ()
                       Just !csource -> do
                           !buffers  <- readMVar (alAppBuffers    alApp)
                           !bufferx <- case Map.lookup (sourceSoundData source_) buffers of
                                           Nothing      -> ALUT.createBuffer csource
                                           Just bufferx -> return bufferx 
                           !sourcex <- ObjectName.genObjectName
                           ________ <- AL.buffer sourcex $= Just bufferx
                           ________ <- AL.loopingMode sourcex $= dataLooping (sourceSoundData source_)
                           ________ <- AL.play [sourcex]
                           ________ <- modifyMVar_ (alAppSources    alApp) (pure . Map.insert isid sourcex)
                           ________ <- modifyMVar_ (alAppBuffers    alApp) (pure . Map.insert (sourceSoundData source_) bufferx)
                           ________ <- modifyMVar_ (alAppDataSource alApp) (pure . Map.insert isid (sourceSoundData source_))
                           return ()
               -- source already created
               Just !sid -> do
                    if sameSoundData
                        -- sounddata is the same
                        then do
                            AL.sourcePosition    sid $= _v3ToVertex (sourcePosition source_)
                            AL.sourceVelocity    sid $= _v3ToVector (sourceVelocity source_)
                            AL.pitch             sid $= realToFrac (sourcePitch source_)
                            AL.sourceGain        sid $= realToFrac (sourceGain  source_)
                            AL.gainBounds        sid $= (realToFrac (fst (sourceGainBounds source_)), realToFrac (snd (sourceGainBounds source_)))
                            AL.direction         sid $= _v3ToVector (sourceDirection source_)
                            AL.sourceRelative    sid $= sourceRelative source_
                            AL.rolloffFactor     sid $= realToFrac (sourceRolloffFactor source_)
                            AL.referenceDistance sid $= realToFrac (sourceReferenceDistance source_)
                            AL.maxDistance       sid $= realToFrac (sourceMaxDistance source_)
                            AL.coneAngles        sid $= (realToFrac $ fst $ sourceConeAngles source_, realToFrac $ snd (sourceConeAngles source_))
                            AL.coneOuterGain     sid $= realToFrac (sourceConeOuterGain source_)
                        -- sounddata changed, create new buffer
                        else do 
                           AL.stop [sid]
                           case dataConvert (sourceSoundData source_) of
                               Nothing -> pure ()
                               Just !csource -> do
                                    !bufferx <- ALUT.createBuffer csource
                                    AL.buffer sid $= Just bufferx
                                    AL.play [sid]
                                    modifyMVar_ (alAppDataSource alApp) (pure . Map.insert isid (sourceSoundData source_))
                                    modifyMVar_ (alAppBuffers    alApp) (pure . Map.insert (sourceSoundData source_) bufferx)

    freeDeadSources :: Map Int Source -> Map Int AL.Source -> IO ()
    freeDeadSources a b = do
        let after    = fmap fst (Map.toList a) -- TODO dont transform to list
        let before   = fmap fst (Map.toList b) -- TODO dont transform to list
        let isAlive  = zip before ((`elem` after) <$> before)
        let deadOnes = mapMaybe (flip Map.lookup b . fst) (filter (not . snd) isAlive)
        AL.stop deadOnes 


-------------------------------------------------------------------------------
-- INTERNAL / HELPERS ---------------------------------------------------------
-------------------------------------------------------------------------------

-----------------------------------------------------------
-- Internal.
data ALApp = ALApp 
    { alAppBuffers    :: !(MVar (Map DataSource AL.Buffer))
    , alAppDataSource :: !(MVar (Map Int        DataSource)) -- TODO we have an id problem
    , alAppSources    :: !(MVar (Map Int        AL.Source)) -- TODO we have an id problem
    }


-----------------------------------------------------------
type Hz = Float


-----------------------------------------------------------
type Phase = Float


-----------------------------------------------------------
type MetersPerSecond = Float


-----------------------------------------------------------
type Meters = Float


-----------------------------------------------------------
type Seconds = Float


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


-----------------------------------------------------------
dataConvert :: DataSource -> Maybe (ALUT.SoundDataSource a)
{-# INLINE dataConvert #-}
dataConvert = \case
    Silence                -> Nothing
    File filePath _        -> Just (ALUT.File filePath)
    Memory ptr size _      -> Just (ALUT.FileImage (AL.MemoryRegion (unsafeCoerce ptr) (fromIntegral size)))
    HelloWorld             -> Just ALUT.HelloWorld
    WhiteNoise             -> Just (ALUT.WhiteNoise     0.1)
    Sine             hz ph -> Just (ALUT.Sine     hz ph 0.1)
    Square           hz ph -> Just (ALUT.Square   hz ph 0.1)
    Sawtooth         hz ph -> Just (ALUT.Sawtooth hz ph 0.1)  
    Impulse          hz ph -> Just (ALUT.Impulse  hz ph 0.1)


-----------------------------------------------------------
dataLooping :: DataSource -> AL.LoopingMode
{-# INLINE dataLooping #-}
dataLooping = \case
    Silence     -> AL.OneShot
    File {}     -> AL.OneShot
    Memory {}   -> AL.OneShot
    HelloWorld  -> AL.OneShot
    WhiteNoise  -> AL.Looping
    Sine     {} -> AL.Looping
    Square   {} -> AL.Looping
    Sawtooth {} -> AL.Looping   
    Impulse  {} -> AL.Looping   


-----------------------------------------------------------
-----------------------------------------------------------
-----------------------------------------------------------
-----------------------------------------------------------
-----------------------------------------------------------


--type Wave a = SF a DataSource

--wave :: Wave -> Hz -> DataSource
--wave = undefined
