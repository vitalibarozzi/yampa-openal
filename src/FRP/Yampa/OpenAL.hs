{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
-- | No supporte for Capture.
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
    , AL.runALUT
    )
where

import qualified Data.ObjectName as ObjectName
import qualified Sound.OpenAL.AL.Listener as AL
import qualified Sound.OpenAL.AL.Errors as AL
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
import Data.Maybe
import Control.Concurrent.Async
import Control.Concurrent


-------------------------------------------------------------------------------
-- PUBLIC ---------------------------------------------------------------------
-------------------------------------------------------------------------------


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
    , sourceOffset            :: !(Maybe (Float,Float))
    , sourceDirection         :: !(V3 Float)
    , sourceConeAngles        :: !(Float, Float) -- Outer cone, inner cone, in degrees.
    , sourceConeOuterGain     :: !Float          -- Gain on the outer cone
    , sourceState             :: !AL.SourceState -- TODO in lose more than we gain by using this instead of mirroring our own since we dont wanna expose anything from the backend anyway
    , sourceRelative          :: !AL.SourceRelative
    , sourceRolloffFactor     :: !Float
    , sourceReferenceDistance :: !Float
    , sourceMaxDistance       :: !Float
    , sourceSoundData         :: !DataSource
    }
  deriving
    (Eq, Show)


-----------------------------------------------------------
data DataSource
    = File FilePath
    | FileImage ()
    | HelloWorld
    | WhiteNoise
    | Sine     Float Float --Frequency Phase
    | Square   Float Float --Frequency Phase
    | Sawtooth Float Float --Frequency Phase
    | Impulse  Float Float --Frequency Phase
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
        , sourceOffset            = Nothing
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


-----------------------------------------------------------
withALUT :: (ALApp -> IO a) -> IO a 
withALUT k = do
    AL.runALUT "Yampa-OpenAL" [] \_name _arguments -> do
       buffers <- newIORef mempty
       dataSrc <- newIORef mempty
       sources <- newIORef mempty
       dtime   <- newIORef 0
       k (ALApp buffers dataSrc sources dtime)


-----------------------------------------------------------
-- | Assumes ALUT was already started.
runSoundscape :: ALApp -> Soundscape -> IO ()
{-# INLINEABLE runSoundscape #-}
runSoundscape alApp s1 = do

    void do
        concurrently runErrors do
            sources <- readIORef (alAppSources alApp)
            dataSrcs <- readIORef (alAppDataSource alApp)
            concurrently (runSources dataSrcs sources) do
                concurrently runMisc do
                    runListener

  where 
    runErrors = do
        threadDelay 1
        errors <- AL.alErrors
        unless (null errors) (print (errors,s1))

    runMisc = do
        AL.dopplerFactor    $= realToFrac (soundscapeDopplerFactor s1)
        AL.speedOfSound     $= realToFrac (soundscapeSpeedOfSound s1)
        AL.distanceModel    $= soundscapeDistanceModel s1

    runListener = do
        let Listener{..} = soundscapeListener s1           
        let (vA,vB) = listenerOrientation
        AL.orientation      $= (_v3ToVector vA, _v3ToVector vB)
        AL.listenerPosition $= _v3ToVertex vA
        AL.listenerVelocity $= _v3ToVector vA
        AL.listenerGain     $= realToFrac listenerGain

    runSources dataSrcs sources = do
        forConcurrently_ (Map.toList (soundscapeSources s1)) \(isid, source_) -> do
            let sameSoundData = sourceSoundData source_ == fromJust (Map.lookup isid dataSrcs)
            case Map.lookup isid sources of
               -- creates source
               Nothing -> do
                   sourcex <- ObjectName.genObjectName
                   bufferx <- AL.createBuffer (dataConvert $ sourceSoundData source_) -- TODO try creating the buffer only once and saving it in that map so we dont create it again
                   -- TODO look for buffer before creating another one
                   _______ <- AL.buffer sourcex $= Just bufferx
                   _______ <- modifyIORef (alAppSources    alApp) (Map.insert isid sourcex) -- dont use ioref here it will datarace for sure
                   _______ <- modifyIORef (alAppBuffers    alApp) (Map.insert (sourceSoundData source_) bufferx) -- dont use ioref here it will datarace for sure
                   _______ <- modifyIORef (alAppDataSource alApp) (Map.insert isid (sourceSoundData source_)) -- dont use ioref here it will datarace for sure
                   return ()
               -- source already created
               Just sid -> do
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
                            case sourceOffset source_ of
                                Nothing        -> pure ()
                                Just (l,of1) -> do
                                    let clamp a = if a > realToFrac l then 0 else a
                                    of0 <- case AL.secOffset sid of StateVar g _ -> g
                                    when (l > (1/3) && abs (of1 - realToFrac of0) > 0.1) (AL.secOffset sid $= clamp (realToFrac (of1 + 0.05)))
                            current <- AL.sourceState sid
                            case (sourceState source_, current) of
                                (AL.Playing, AL.Playing) -> pure ()
                                (AL.Stopped, AL.Stopped) -> pure ()
                                (AL.Initial, AL.Initial) -> pure ()
                                (AL.Paused , AL.Playing) -> AL.pause [sid]
                                (AL.Playing, __________) -> AL.play [sid]
                                (AL.Stopped, __________) -> AL.stop [sid]
                                (AL.Initial, __________) -> AL.stop [sid]
                                (AL.Paused , __________) -> pure ()
                        -- sounddata changed, create new buffer
                        else do
                            AL.stop [sid]
                            -- TODO look for buffer before creating another one
                            bufferx <- AL.createBuffer (dataConvert $ sourceSoundData source_)
                            AL.buffer sid $= Just bufferx
                            modifyIORef (alAppDataSource alApp) (Map.insert isid (sourceSoundData source_)) -- dont use ioref here it will datarace for sure
                            modifyIORef (alAppBuffers    alApp) (Map.insert (sourceSoundData source_) bufferx) -- dont use ioref here it will datarace for sure


-------------------------------------------------------------------------------
-- INTERNAL / HELPERS ---------------------------------------------------------
-------------------------------------------------------------------------------


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
-- Internal.
data ALApp = ALApp 
    { alAppBuffers    :: IORef (Map DataSource AL.Buffer)
    , alAppDataSource :: IORef (Map Int DataSource)
    , alAppSources    :: IORef (Map Int AL.Source)
    , alAppDTime      :: IORef Integer -- in seconds
    }


-----------------------------------------------------------
dataConvert :: DataSource -> AL.SoundDataSource ()
dataConvert = \case
    File filePath -> AL.File filePath
    FileImage () -> undefined
    HelloWorld -> AL.HelloWorld
    WhiteNoise -> AL.WhiteNoise 1
    Sine     double double1 -> AL.Sine     double double1 10
    Square   double double1 -> AL.Square   double double1 10
    Sawtooth double double1 -> AL.Sawtooth double double1 10   
    Impulse  double double1 -> AL.Impulse  double double1 10
