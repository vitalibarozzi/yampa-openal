
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FRP.Yampa.OpenAL.IO
where


import Data.Map (Map)
import Linear.V3 (V3)
import qualified Sound.OpenAL as AL
import qualified Sound.ALUT.Loaders as AL


update :: Soundstage -> Soundstage -> IO ()
update = undefined
    -- TODO works foward and backwards in time


updateSource :: Source -> Source -> IO ()
updateSource = undefined

{-
withAL :: Int -> (ALApp -> IO a) -> IO a
{-# INLINE withAL #-}
withAL maxSources !k = do
    ALUT.runALUT "Yampa-OpenAL" [] \_name _arguments -> do
        undefined -- buffers <- newMVar mempty
        nullSource <- ObjectName.genObjectName
        ptr <- Array.newArray @AL.Source (replicate maxSources nullSource)
        sources <- newMVar ptr
        groups <- newMVar undefined
        ________ <- forkIO (forever runErrors)
        undefined -- k (ALApp buffers sources groups nullSource)
  where
    runErrors = do
        errors <- AL.alErrors
        if null errors
            then putStrLn ("[yampa-openal]: "<> show errors)
            else threadDelay 5_010_101

-----------------------------------------------------------
-- reactInitSoundscape :: ALApp -> a -> SF a Soundscape -> IO (ReactHandle a (Event (Soundscape, Soundscape)))
{-# INLINEABLE reactInitSoundscape #-}
reactInitSoundscape alApp a soundscapeSF = do
    Yampa.reactInit (pure a) actuate
        $ loopPre emptySoundscape
        $ proc (a', prev) -> do
            xs <- _updateRoutine alApp <<< soundscapeSF -< a'
            returnA -< (Event xs, prev)
  where
    actuate _ _ = \case
        NoEvent -> pure False
        Event actions -> do
            forM_ actions id
            return (not (null actions))

-----------------------------------------------------------
_updateRoutine :: ALApp -> SF Soundscape [IO ()]
_updateRoutine alApp =
    join <$> parB
        [ updateListenerPos <<< listenerPositionChanged
        , updateListenerVel <<< listenerVelocityChanged
        , updateListenerOri <<< listenerOrientationChanged
        , updateListenerGai <<< listenerGainChanged
        , updateSpeedOfSound  <<< speedOfSoundChanged
        , updateDistanceModel <<< distanceModelChanged
        , updateDopplerFactor <<< dopplerFactorChanged
        , updateSourceGroups <<< sourceGroupsChanged
        ]

  where

    updateSpeedOfSound = undefined
    speedOfSoundChanged= undefined -- (AL.speedOfSound $= realToFrac (soundscapeSpeedOfSound s1))

    updateDistanceModel= undefined
    distanceModelChanged= undefined -- (AL.distanceModel $= soundscapeDistanceModel s1)

    updateDopplerFactor= undefined
    dopplerFactorChanged = undefined-- undefined -- undefined -- undefined -- undefined -- undefined -- undefined -- undefined -- soundscapeDopplerFactor

    sourceGroupsChanged = loopPre emptySoundscape $ proc (next_, prev) -> do
        undefined -< next_
    updateSourceGroups = proc groups -> do
        returnA -< fmap foo (Map.toList groups)
      where
        foo = \case
            (_, (Nothing,Nothing)) -> error "impossible"
            (_, (Just prev,Nothing)) -> sourceGroupWasDeleted prev
            (_, (Nothing,Just next)) -> sourceGroupWasCreated  next
            (_, (Just prev,Just next)) -> sourceGroupWasModified prev next
          where
            sourceGroupWasDeleted (prev) = do
                undefined

            sourceGroupWasCreated (next) = do
                undefined

            sourceGroupWasModified (prev) (next) = do
                undefined

    sourceChanged = undefined
    updateSource = undefined -- proc sources -> do
        --returnA -< fmap foo (Map.toList sources)
      where
        foo = \case
            (_, (Nothing,Nothing)) -> error "impossible"
            (_, (Just prev,Nothing)) -> sourceWasDeleted prev
            (_, (Nothing,Just next)) -> sourceWasCreated  next
            (_, (Just prev,Just next)) -> sourceWasModified prev next
          where
            sourceWasDeleted = do
                undefined

            sourceWasCreated = do
                undefined
            --                if sid == nullSource alApp
            ----                    then do
            --                        let soundData = dataConvert (sourceSoundData source_)
            --                        bufferx <- case Map.lookup (sourceSoundData source_) buffers of
            --                            Nothing -> ALUT.createBuffer soundData
            --                            Just bufferx -> return bufferx
            --                        !src <- ObjectName.genObjectName
            --                        AL.buffer src $= Just bufferx
            --                        AL.play [src]
            --                        AL.loopingMode src $= dataLooping (sourceSoundData source_)
            --                        modifyMVar_ (alAppBuffers alApp) (pure . Map.insert (sourceSoundData source_) bufferx)
            --                        modifyMVar_ (alAppSources alApp) (\sources__ -> pokeElemOff sources__ n src >> pure sources__)

            sourceWasModified = do
                undefined
            --                sources <- readMVar (alAppSources alApp)
            --                sid <- peekElemOff sources n
            --                    else do
            --                        AL.sourcePosition sid $= _v3ToVertex (sourcePosition source_)
            --                        AL.sourceVelocity sid $= _v3ToVector (sourceVelocity source_)
            --                        AL.sourceGain sid $= realToFrac (sourceGain source_)
            --                        AL.gainBounds sid $= let foo = realToFrac (fst (sourceGainBounds source_)) in (foo, realToFrac (snd (sourceGainBounds source_)))
            --                        AL.direction sid $= _v3ToVector (sourceDirection source_)
            --                        AL.sourceRelative sid $= sourceRelative source_
            --                        AL.rolloffFactor sid $= realToFrac (sourceRolloffFactor source_)
            --                        AL.referenceDistance sid $= realToFrac (sourceReferenceDistance source_)
            --                        AL.maxDistance sid $= realToFrac (sourceMaxDistance source_)
            --                        AL.coneAngles sid $= (realToFrac $ fst $ sourceConeAngles source_, realToFrac $ snd (sourceConeAngles source_))
            --                        AL.coneOuterGain sid $= realToFrac (sourceConeOuterGain source_)
            --
            -- TODO we need both the dt from the soundscape
            --                  and the dt from the source
            --                  so if there is a difference in rate
            --                  we change the pitch accordingly
            --
            -- TODO AL.pitch sid $= realToFrac (sourcePitch source_)

    -----------------------------------------------------------
    listenerPositionChanged :: SF Soundscape (Event (V3 Float))
    listenerPositionChanged = loopPre emptySoundscape $ proc (next_, prev) -> do
        let pos = listenerPosition (soundscapeListener prev) /= listenerPosition (soundscapeListener next_)
        returnA -< (if pos then Event (listenerPosition (soundscapeListener next_)) else NoEvent, next_)
    updateListenerPos :: SF (Event (V3 Float)) [IO ()]
    updateListenerPos = proc ev -> do
        case ev of
            Event v3 -> returnA -< [AL.listenerPosition $= _v3ToVertex v3]
            NoEvent  -> returnA -< []

    -----------------------------------------------------------
    listenerVelocityChanged :: SF Soundscape (Event (V3 Float))
    listenerVelocityChanged = loopPre emptySoundscape $ proc (next_, prev) -> do
        let vel = listenerVelocity (soundscapeListener prev) /= listenerVelocity (soundscapeListener next_)
        returnA -< (if vel then Event (listenerVelocity (soundscapeListener next_)) else NoEvent, next_)
    updateListenerVel :: SF (Event (V3 Float)) [IO ()]
    updateListenerVel = proc ev -> do
        case ev of
            Event v3 -> returnA -< [AL.listenerVelocity $= _v3ToVector v3]
            NoEvent  -> returnA -< []

    -----------------------------------------------------------
    listenerOrientationChanged = loopPre emptySoundscape $ proc (next_, prev) -> do
        let ori = listenerOrientation (soundscapeListener prev) /= listenerOrientation (soundscapeListener next_)
        returnA -< (if ori then Event (listenerOrientation (soundscapeListener next_)) else NoEvent, next_)
    updateListenerOri = proc ev -> do
        case ev of
            Event (v3a,v3b) -> returnA -< [AL.orientation $= (_v3ToVector v3a, _v3ToVector v3b)]
            NoEvent  -> returnA -< []

    -----------------------------------------------------------
    listenerGainChanged :: SF Soundscape (Event Float)
    listenerGainChanged = loopPre emptySoundscape $ proc (next_, prev) -> do
        let g = listenerGain (soundscapeListener prev) /= listenerGain (soundscapeListener next_)
        returnA -< undefined -- (if g then Event (listenerGain (soundscapeListener next_)) else NoEvent, next_)
    updateListenerGai :: SF (Event Float) [IO ()]
    updateListenerGai = proc ev -> do
        case ev of
            Event g -> returnA -< [AL.listenerGain $= realToFrac g]
            NoEvent -> returnA -< []

-------------------------------------------------------------------------------
-- INTERNAL / HELPERS ---------------------------------------------------------
-------------------------------------------------------------------------------

-----------------------------------------------------------
-- Internal.
data ALApp = ALApp
    { --alAppBuffers :: !(MVar (Map DataSource AL.Buffer)) -- fat cache, good for now

      alAppSources :: !(MVar (Ptr AL.Source)) -- associates ints with al.sources

    , alAppSourceGroups :: !(MVar (Ptr AL.Source)) -- associates ints with al.sources

    , alAppNullSource :: AL.Source
    }

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
_vertexToV3 :: AL.Vertex3 AL.ALfloat -> V3 Float
{-# INLINE _vertexToV3 #-}
_vertexToV3 (AL.Vertex3 x y z) =
    V3
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
{-# INLINE dataConvert #-}
dataConvert = \case
    -- File filePath _ -> Just (ALUT.File filePath)
    -- Memory ptr size _ -> Just (ALUT.FileImage (AL.MemoryRegion (unsafeCoerce ptr) (fromIntegral size)))
    -- HelloWorld -> Just ALUT.HelloWorld
    WhiteNoise -> (ALUT.WhiteNoise 0.5)
    Sine hz ph -> (ALUT.Sine (realToFrac hz) ph 0.5)
    Square hz ph -> (ALUT.Square (realToFrac hz) ph 0.5)
    Sawtooth hz ph -> (ALUT.Sawtooth (realToFrac hz) ph 0.5)
    Impulse hz ph -> (ALUT.Impulse (realToFrac hz) ph 0.5)

-----------------------------------------------------------
{-# INLINE dataLooping #-}
dataLooping = \case
    -- File{} -> AL.OneShot
    -- Memory{} -> AL.OneShot
    -- HelloWorld -> AL.OneShot
    WhiteNoise -> AL.Looping
    Sine{} -> AL.Looping
    Square{} -> AL.Looping
    Sawtooth{} -> AL.Looping
    Impulse{} -> AL.Looping

{-
            let new = Map.difference (soundscapeSources s1) (soundscapeSources s0)
            let gone = Map.difference (soundscapeSources s0) (soundscapeSources s1)
            let rest = undefined

            -- todo create new
            -- todo update rest
            -- todo stop dead

            !sources <- readMVar (alAppSources alApp)
            !buffers <- readMVar (alAppBuffers alApp)

            ________ <- undefined -- freeDeadSources (soundscapeSources s1) sources -- TODO

            -- forConcurrently_ (zip (Map.toList (soundscapeSources s0)) (Map.toList (soundscapeSources s1))) $
            --   \((isid0, source0),(isid1, source1)) -> do

            forConcurrently_ (Map.toList (soundscapeSources s0)) \(!isid, !source_) -> do
                -- TODO dont turn it into a list
                -- check if this source exists in the past
                case undefined of -- Map.lookup isid sources of -- TODO
                    Nothing -> undefined -- withNewSource buffers source_
                    Just !sid -> do
                        sameSoundData_ sid source_
                        soundDataChanged sid source_
                        sameSoundData_ sid source_
                        soundDataChanged sid source_

        withNewSource buffers source_ = do
            case dataConvert (head $ sourceSoundData source_) of
                Nothing -> pure ()
                Just !csource -> do
                    bufferx <- case Map.lookup (sourceSoundData source_) buffers of
                        Nothing -> ALUT.createBuffer csource
                        Just bufferx -> return bufferx
                    !xxx <- ObjectName.genObjectName
                    ________ <- AL.buffer xxx $= Just bufferx
                    ________ <- AL.loopingMode xxxjb $= dataLooping (head $ sourceSoundData source_)
                    ________ <- AL.play [xxx]
                    ________ <- modifyMVar_ (alAppBuffers alApp) (pure . Map.insert (head $ sourceSoundData source_) bufferx)
                    return ()

        soundDataChanged sid source_ = do
            AL.stop [sid]
            case dataConvert (head $ sourceSoundData source_) of
                Nothing -> pure ()
                Just !csource -> do
                    !bufferx <- ALUT.createBuffer csource
                    AL.buffer sid $= Just bufferx
                    AL.play [sid]
                    modifyMVar_ (alAppBuffers alApp) (pure . Map.insert (head $ sourceSoundData source_) bufferx)

        sameSoundData_ sid source_ = do
            case sourceSoundData source_ of
                [File _ (minOffset, maxOffset) offset]
                    | offset < abs minOffset -> AL.stop [sid]
                    | offset == abs minOffset -> AL.play [sid]
                    | otherwise -> do
                        if abs offset >= maxOffset
                            then AL.stop [sid]
                            else do
                                updateRest sid source_
                                offsetSec0 <- realToFrac <$> get (AL.secOffset sid)
                                if abs (offset - offsetSec0) > (abs maxOffset / 100) || abs (offset - offsetSec0) > 5
                                    then AL.secOffset sid $= realToFrac (offset + (2 * offsetSec0)) / 3
                                    else
                                        when
                                            (abs (offset - offsetSec0) > (abs maxOffset / 200))
                                            (AL.secOffset sid $= realToFrac (offset + offsetSec0) / 2)
                _ -> updateRest sid source_
        --let pos = soundscapeSourceGroups prev /= soundscapeSourceGroups next_
        --returnA -< (if pos then Event (listenerPosition (soundscapeListener next_)) else NoEvent, next_)
            --        buffers <- readMVar (alAppBuffers alApp)
            --        forM_ (soundscapeSources ss1) \sourceGroup_ -> do
            --            forM_ (Map.toList sourceGroup_) \(n, source_) -> do
-}
tick :: IORef Integer -> IO ()
tick ref = getCPUTime >>= writeIORef ref

tack :: IORef Integer -> IO Double
tack ref = do
    t1 <- getCPUTime
    t0 <- readIORef ref
    return (realToFrac (t1 - t0) / 10000)
--ss xs = soundscape $ Map.fromList (zip [1 ..] xs)

-}
