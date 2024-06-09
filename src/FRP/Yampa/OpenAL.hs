{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module FRP.Yampa.OpenAL where

import qualified Data.Map as Map
import FRP.Yampa
import qualified FRP.Yampa as Yampa
import FRP.Yampa.OpenAL.Types
import Linear.V3
import qualified Sound.OpenAL as AL


-----------------------------------------------------------
-- | To be used when you don't need to change the sources signals
-- of the soundstage during its execution.
soundstage :: [SourceSignal a] -> SF a Soundstage
soundstage sources =
    Soundstage
        <$> fmap Map.fromList (parB (fmap (fmap (\x -> (sourceID x,x))) sources))
        <*> pure 1
        <*> pure 343.3
        <*> pure AL.InverseDistance
        <*> constant (V3 0 0 0)
        <*> constant (V3 0 0 0)
        <*> constant (V3 0 0 (-1), V3 0 1 0)
        <*> constant 1



-----------------------------------------------------------
-- For when you want to change the collection of source 
-- signals of the soundstage at some point.
soundstageDynamic :: SF (a, Event ([SourceSignal a] -> [SourceSignal a])) Soundstage
soundstageDynamic =
    Soundstage
        <$> fmap (\xs -> Map.fromList $ fmap (\x -> (sourceID x, x)) xs) (drpSwitchB [])
        <*> pure 1
        <*> pure 343.3
        <*> pure AL.InverseDistance
        <*> constant (V3 0 0 0)
        <*> constant (V3 0 0 0)
        <*> constant (V3 0 0 (-1), V3 0 1 0)
        <*> constant 1


-----------------------------------------------------------
source :: String -> [Buffer] -> DTime -> SourceSignal a
source name queue startAt = proc a -> do
    t <- Yampa.time -< a
    returnA
        -<
            Source
                { sourceID = name
                , sourcePosition = V3 0 0 0
                , sourceVelocity = V3 0 0 0
                , sourceGain = 1
                , sourcePitch = 1
                , sourceGainBounds = (0, 1)
                , sourceDirection = V3 0 0 0
                , sourceConeAngles = (360, 360) -- sounds go in all directions
                , sourceConeOuterGain = 0 -- we hear nothing outside the cone
                , sourceRelative = AL.World
                , sourceRolloffFactor = 1
                , sourceReferenceDistance = 1
                , sourceMaxDistance = 10_000 -- try some others
                , sourceLoopingMode = OneShot
                , sourceState = Playing
                , sourceBufferQueue = queue
                , sourceOffset = realToFrac (startAt + t)
                }
