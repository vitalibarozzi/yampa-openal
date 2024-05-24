{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module FRP.Yampa.OpenAL (
    Soundscape (..),
    Listener (..),
    Source (..),
    DataSource (..),

    -- * Constructors
    soundscape,
    source,
    listener,
    runSoundscape,
    withALUT,

    -- * Source Effects
    tremolo,
    phaser,
)
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.ObjectName as ObjectName
import Data.StateVar
import FRP.Yampa
import qualified FRP.Yampa as Yampa
import Foreign.Ptr
import Foreign.Storable
import Linear.V2
import Linear.V3
import qualified Sound.ALUT.Initialization as ALUT
import qualified Sound.ALUT.Loaders as ALUT
import qualified Sound.OpenAL as AL
import Unsafe.Coerce

-------------------------------------------------------------------------------
-- PUBLIC ---------------------------------------------------------------------
-------------------------------------------------------------------------------

-----------------------------------------------------------

-- | A model of the how the sound elements change overtime.
data Soundscape = Soundscape
    { soundscapeListener :: !Listener
    , soundscapeDopplerFactor :: !Float --
    , soundscapeSpeedOfSound :: !MetersPerSecond
    , soundscapeDistanceModel :: !AL.DistanceModel
    , soundscapeSources :: !(Map Int Source)
    , soundscapeShouldClose :: !Bool
    }
    deriving
        (Eq, Show)

-----------------------------------------------------------

-- | Like a POV, but for audio. A floating ear.
data Listener = Listener
    { listenerOrientation :: !(V3 Meters, V3 Meters)
    , listenerPosition :: !(V3 Meters)
    , listenerVelocity :: !(V3 MetersPerSecond)
    , listenerGain :: !Float
    }
    deriving
        (Eq, Show)

-----------------------------------------------------------

-- | A source of audio in space.
data Source = Source
    -- spatial
    { sourcePosition :: !(V3 Meters)
    , sourceVelocity :: !(V3 MetersPerSecond)
    , sourceDirection :: !(V3 Meters)
    , sourceRelative :: !AL.SourceRelative
    , sourceConeAngles :: !(Float, Float) -- Outer cone, inner cone, in degrees.
    , sourceConeOuterGain :: !Float -- Gain on the outer cone
    , sourceRolloffFactor :: !Float
    , sourceReferenceDistance :: !Meters
    , sourceMaxDistance :: !Meters
    , -- misc
      sourceGain :: !Double
    , sourceGainBounds :: !(Float, Float) -- TODO maybe this shouldnt be exposed or should be a function instead
    , sourcePitch :: !Double
    , sourceSoundData :: !DataSource
    , sourceOffset :: !(Maybe Double)
    }
    deriving
        (Eq, Show)

-----------------------------------------------------------
data DataSource
    = Silence
    | HelloWorld
    | WhiteNoise
    | Sine !Hz !Phase
    | Square !Hz !Phase
    | Sawtooth !Hz !Phase
    | Impulse !Hz !Phase
    | Memory !(Ptr ()) !Int !(Maybe Seconds)
    | File !FilePath
    -- TODO | Signal Hz (SF () Float) -- lazily creates buffer as needed

    deriving
        (Eq, Show, Ord)

-----------------------------------------------------------

-- | Constructor with default values.
soundscape :: Soundscape
{-# INLINE soundscape #-}
soundscape =
    Soundscape
        { soundscapeListener = listener
        , soundscapeDopplerFactor = 1
        , soundscapeSpeedOfSound = 343.3
        , soundscapeDistanceModel = AL.InverseDistance
        , soundscapeSources = mempty
        , soundscapeShouldClose = False
        }

-----------------------------------------------------------

-- | Constructor with default values.
listener :: Listener
{-# INLINE listener #-}
listener =
    Listener
        { listenerOrientation = (V3 0 0 (-1), V3 0 1 0)
        , listenerPosition = V3 0 0 0
        , listenerVelocity = V3 0 0 0
        , listenerGain = 1
        }

-----------------------------------------------------------

-- | Constructor.
source :: DataSource -> SF a Source
source sd = proc a -> do
    t <- Yampa.time -< a
    offset <-
        returnA
            -< case sd of
                File ____ -> Just t
                Memory{} -> Just t
                _________ -> Nothing
    returnA
        -<
            Source
                { sourcePosition = V3 0 0 0
                , sourceVelocity = V3 0 0 0
                , sourceGain = 1
                , sourceGainBounds = (0, 1)
                , sourcePitch = 1
                , sourceDirection = V3 0 0 0
                , sourceConeAngles = (360, 360) -- sounds go in all directions
                , sourceConeOuterGain = 0 -- we hear nothing outside the cone
                , sourceRelative = AL.World
                , sourceRolloffFactor = 1
                , sourceReferenceDistance = 1
                , sourceMaxDistance = 10000
                , sourceSoundData = sd
                , sourceOffset = offset
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
    void (concurrently runSources (runMisc >> runListener))
  where
    runMisc = do
        AL.dopplerFactor $= realToFrac (soundscapeDopplerFactor s1)
        AL.speedOfSound $= realToFrac (soundscapeSpeedOfSound s1)
        AL.distanceModel $= soundscapeDistanceModel s1

    runListener = do
        let Listener{..} = soundscapeListener s1
        let (vA, vB) = listenerOrientation
        AL.orientation $= (_v3ToVector vA, _v3ToVector vB)
        AL.listenerPosition $= _v3ToVertex vA
        AL.listenerVelocity $= _v3ToVector vA
        AL.listenerGain $= realToFrac listenerGain

    runSources = do
        !dataSrcs <- readMVar (alAppDataSource alApp)
        !sources <- readMVar (alAppSources alApp)
        !buffers <- readMVar (alAppBuffers alApp)
        ________ <- freeDeadSources (soundscapeSources s1) sources
        forConcurrently_ (Map.toList (soundscapeSources s1)) \(!isid, !source_) -> do
            let sameSoundData = Just (sourceSoundData source_) == Map.lookup isid dataSrcs
            case Map.lookup isid sources of
                -- creates source
                Nothing -> do
                    case dataConvert (sourceSoundData source_) of
                        Nothing -> pure ()
                        Just !csource -> do
                            (size,bufferx) <- case Map.lookup (sourceSoundData source_) buffers of
                                Nothing -> do
                                    buffer <- ALUT.createBuffer csource
                                    return (sizeOf buffer, buffer)
                                Just (size, bufferx) -> return (size,bufferx)
                            !sourcex <- ObjectName.genObjectName
                            ________ <- AL.buffer sourcex $= Just bufferx
                            ________ <- AL.loopingMode sourcex $= dataLooping (sourceSoundData source_)
                            ________ <- AL.play [sourcex]
                            ________ <- modifyMVar_ (alAppSources alApp) (pure . Map.insert isid sourcex)
                            ________ <- modifyMVar_ (alAppBuffers alApp) (pure . Map.insert (sourceSoundData source_) (size, bufferx))
                            ________ <- modifyMVar_ (alAppDataSource alApp) (pure . Map.insert isid (sourceSoundData source_))
                            return ()
                -- source already created
                Just !sid -> do
                    if sameSoundData
                        then -- sounddata is the same
                        do
                            AL.sourcePosition sid $= _v3ToVertex (sourcePosition source_)
                            AL.sourceVelocity sid $= _v3ToVector (sourceVelocity source_)
                            AL.pitch sid $= realToFrac (sourcePitch source_)
                            AL.sourceGain sid $= realToFrac (sourceGain source_)
                            AL.gainBounds sid $= (realToFrac (fst (sourceGainBounds source_)), realToFrac (snd (sourceGainBounds source_)))
                            AL.direction sid $= _v3ToVector (sourceDirection source_)
                            AL.sourceRelative sid $= sourceRelative source_
                            AL.rolloffFactor sid $= realToFrac (sourceRolloffFactor source_)
                            AL.referenceDistance sid $= realToFrac (sourceReferenceDistance source_)
                            AL.maxDistance sid $= realToFrac (sourceMaxDistance source_)
                            AL.coneAngles sid $= (realToFrac $ fst $ sourceConeAngles source_, realToFrac $ snd (sourceConeAngles source_))
                            AL.coneOuterGain sid $= realToFrac (sourceConeOuterGain source_)
                            case sourceOffset source_ of
                                Nothing -> pure ()
                                Just offset -> do
                                    bufferSize <- case Map.lookup (sourceSoundData source_) buffers of
                                                       Nothing -> return 0
                                                       Just (size,buff) -> return size
                                    -- TODO if offset < 0 || offset > (realToFrac bufferSize)
                                    if offset < 0
                                        then AL.stop [sid]
                                        else do
                                            if offset == 0
                                                then AL.play [sid]
                                                else do
                                                    offset0 <- case AL.secOffset sid of StateVar get set -> get
                                                    let diff = offset - realToFrac offset0
                                                    if offset > realToFrac offset0 && abs diff > 0.1
                                                        then do
                                                            AL.secOffset sid $= realToFrac offset
                                                            AL.play [sid]
                                                        else
                                                            if offset < realToFrac offset0 && abs diff > 0.1
                                                                then pure ()
                                                                else AL.secOffset sid $= realToFrac offset
                        else -- sounddata changed, create new buffer
                        do
                            AL.stop [sid]
                            case dataConvert (sourceSoundData source_) of
                                Nothing -> pure ()
                                Just !csource -> do
                                    !bufferx <- ALUT.createBuffer csource
                                    AL.buffer sid $= Just bufferx
                                    AL.play [sid]
                                    modifyMVar_ (alAppDataSource alApp) (pure . Map.insert isid (sourceSoundData source_))
                                    modifyMVar_ (alAppBuffers alApp) (pure . Map.insert (sourceSoundData source_) (sizeOf bufferx,bufferx))

    freeDeadSources :: Map Int Source -> Map Int AL.Source -> IO ()
    freeDeadSources a b = do
        let after_ = fmap fst (Map.toList a) -- TODO dont transform to list
        let before = fmap fst (Map.toList b) -- TODO dont transform to list
        let isAlive = zip before ((`elem` after_) <$> before)
        let deadOnes = mapMaybe (flip Map.lookup b . fst) (filter (not . snd) isAlive)
        AL.stop deadOnes

-------------------------------------------------------------------------------
-- LISTENER EFFECTS ------------------------------------------------------
-------------------------------------------------------------------------------

-- listenerFadeIn and out?
-- listenerTremolo?
-- listenerOrientation :: !(V3 Meters, V3 Meters)
-- listenerPosition    :: !(V3 Meters)
-- listenerVelocity    :: !(V3 MetersPerSecond)
-- listenerGain        :: !Float

-------------------------------------------------------------------------------
-- FOO -------------------------------------------------------------
-------------------------------------------------------------------------------

-----------------------------------------------------------

-- | Changes playback speed trying to keep the pitch the same.
playbackSpeed :: Double -> SF Source Source
playbackSpeed = undefined

-------------------------------------------------------------------------------
-- NATIVE SOURCE EFFECTS ------------------------------------------------------
-------------------------------------------------------------------------------

-----------------------------------------------------------
fadeIn :: Depth -> Seconds -> SF Source Source
fadeIn (abs -> depth) hz = proc s -> do
    undefined -< s

-----------------------------------------------------------
fadeOut :: Depth -> Seconds -> SF Source Source
fadeOut (abs -> depth) hz = proc s -> do
    undefined -< s

-----------------------------------------------------------
slideIn :: Depth -> Seconds -> SF Source Source
slideIn (abs -> depth) = proc s -> do
    undefined -< s

-----------------------------------------------------------
slideOut :: Depth -> Seconds -> SF Source Source
slideOut (abs -> depth) = proc s -> do
    undefined -< s

-----------------------------------------------------------
tremolo :: Depth -> Hz -> SF Source Source
tremolo (abs -> depth) (abs -> hz) = proc s -> do
    deltaGain <- arr (sin . (* hz)) <<< Yampa.time -< s
    returnA -< s{sourceGain = abs (sourceGain s + (deltaGain * depth))}

-----------------------------------------------------------
phaser :: Depth -> Hz -> SF Source Source
phaser (abs -> depth) (abs -> hz) = proc s -> do
    deltaPitch <- arr (sin . (* hz)) <<< Yampa.time -< s
    returnA -< s{sourcePitch = abs (sourcePitch s + (deltaPitch * depth))}

-----------------------------------------------------------
-- rotary ?

-------------------------------------------------------------------------------
-- EXTENSION SOURCE EFFECTS ---------------------------------------------------
-------------------------------------------------------------------------------

reverb :: Int -> SF Source Source
reverb = undefined

{-
"AL_EXT_ALAW"
"AL_EXT_BFORMAT"
"AL_EXT_DOUBLE"
"AL_EXT_EXPONENT_DISTANCE"
"AL_EXT_FLOAT32"
"AL_EXT_IMA4"
"AL_EXT_LINEAR_DISTANCE"
"AL_EXT_MCFORMATS"
"AL_EXT_MULAW"
"AL_EXT_MULAW_BFORMAT"
"AL_EXT_MULAW_MCFORMATS"
"AL_EXT_OFFSET"
"AL_EXT_SOURCE_RADIUS" <----------- nice
"AL_EXT_STATIC_BUFFER"
"AL_EXT_source_distance_model"

"AL_SOFTX_convolution_reverb" <------ nice
"AL_SOFTX_hold_on_disconnect"
"AL_SOFTX_map_buffer" <------ nice

"AL_SOFT_MSADPCM"

"AL_SOFT_UHJ"
"AL_SOFT_UHJ_ex"

"AL_SOFT_callback_buffer"
"AL_SOFT_deferred_updates"
"AL_SOFT_direct_channels"
"AL_SOFT_direct_channels_remix"
"AL_SOFT_effect_target" <------ ?
"AL_SOFT_events"
"AL_SOFT_gain_clamp_ex"
"AL_SOFT_loop_points" <------ ?
"AL_SOFT_source_latency" <------ ?
"AL_SOFT_source_length" <------ ?
"AL_SOFT_source_resampler" <------ ?
"AL_SOFT_source_spatialize" <------ ?
"AL_SOFT_source_start_delay" <------ nice

"ALC_ENUMERATE_ALL_EXT"
"ALC_ENUMERATION_EXT"

"ALC_EXT_CAPTURE"
"ALC_EXT_DEDICATED"
"ALC_EXT_EFX"
"ALC_EXT_disconnect"
"ALC_EXT_thread_local_context"

"ALC_SOFT_HRTF"
"ALC_SOFT_device_clock"
"ALC_SOFT_loopback"
"ALC_SOFT_loopback_bformat"
"ALC_SOFT_output_limiter"
"ALC_SOFT_output_mode"
"ALC_SOFT_pause_device"
-}

-------------------------------------------------------------------------------
-- INTERNAL / HELPERS ---------------------------------------------------------
-------------------------------------------------------------------------------

-----------------------------------------------------------
-- Internal.
data ALApp = ALApp
    { alAppBuffers :: !(MVar (Map DataSource (Int,AL.Buffer)))
    , alAppDataSource :: !(MVar (Map Int DataSource)) -- TODO we have an id problem
    , alAppSources :: !(MVar (Map Int AL.Source)) -- TODO we have an id problem
    }

-----------------------------------------------------------
type Hz = Double

-----------------------------------------------------------
type Phase = Float

-----------------------------------------------------------
type MetersPerSecond = Float

-----------------------------------------------------------
type Meters = Float

-----------------------------------------------------------
type Seconds = Float

-----------------------------------------------------------
type Depth = Double

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
    Silence -> Nothing
    File filePath -> Just (ALUT.File filePath)
    Memory ptr size _ -> Just (ALUT.FileImage (AL.MemoryRegion (unsafeCoerce ptr) (fromIntegral size)))
    HelloWorld -> Just ALUT.HelloWorld
    WhiteNoise -> Just (ALUT.WhiteNoise 1.1)
    Sine hz ph -> Just (ALUT.Sine (realToFrac hz) ph 1.1)
    Square hz ph -> Just (ALUT.Square (realToFrac hz) ph 1.1)
    Sawtooth hz ph -> Just (ALUT.Sawtooth (realToFrac hz) ph 1.1)
    Impulse hz ph -> Just (ALUT.Impulse (realToFrac hz) ph 1.1)

-----------------------------------------------------------
dataLooping :: DataSource -> AL.LoopingMode
{-# INLINE dataLooping #-}
dataLooping = \case
    Silence -> AL.OneShot
    File{} -> AL.OneShot
    Memory{} -> AL.OneShot
    HelloWorld -> AL.OneShot
    WhiteNoise -> AL.Looping
    Sine{} -> AL.Looping
    Square{} -> AL.Looping
    Sawtooth{} -> AL.Looping
    Impulse{} -> AL.Looping

{-
sourcePosition' :: V3 Float -> SF Source Source
sourcePosition' = undefined

sourceFadeout :: Float -> SF Source Source
sourceFadeout = undefined

sourceFadein :: Float -> SF Source Source
sourceFadein = undefined

pitchSlideIn :: Float -> SF Source Source
pitchSlideIn = undefined

pitchSlideOut :: Float -> SF Source Source
pitchSlideOut = undefined

-----------------------------------------------------------
-----------------------------------------------------------
-----------------------------------------------------------
-----------------------------------------------------------
-----------------------------------------------------------

--type Wave a = SF a DataSource

--wave :: Wave -> Hz -> DataSource
--wave = undefined

-- setter
sourceGain_ :: Float -> SF Source Source
sourceGain_ = undefined

-- setter
sourcePosition_ :: V3 Float -> SF Source Source
sourcePosition_ = undefined

foo :: SF a Source
foo = proc foo -> do
    random <- undefined 0 1 -< ()
    source <- sourceGain' 0 -< source Silence
    source <- sourcePosition' 0 -< source
    returnA -< source

bar :: SF a Source
bar =
    Source
        <$> sourcePositionSF
        <*> sourceVelocitySF
        <*> sourceDirectionSF
        <*> sourceRelativeSF
        <*> sourceGainSF
        <*> sourceGainBoundsSF
        <*> sourceConeAnglesSF
        <*> sourceConeOuterGainSF
        <*> sourceRolloffFactorSF
        <*> sourceReferenceDistanceSF
        <*> sourceMaxDistanceSF
        <*> sourcePitchSF
        <*> sourceSoundDataSF
  where
    sourcePositionSF = undefined
    sourceVelocitySF = undefined
    sourceDirectionSF = undefined
    sourceRelativeSF = undefined
    sourceGainSF = undefined
    sourceGainBoundsSF = undefined
    sourceConeAnglesSF = undefined
    sourceConeOuterGainSF = undefined
    sourceRolloffFactorSF = undefined
    sourceReferenceDistanceSF = undefined
    sourceMaxDistanceSF = undefined
    sourcePitchSF = undefined
    sourceSoundDataSF = undefined
    -}

-- sourceGain' :: Float -> SF Source Source
-- sourceGain' = undefined
