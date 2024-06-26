{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FRP.Yampa.OpenAL.Util (
    _v3ToVertex,
    _vertexToV3,
    _v3ToVector,
    _vectorToV3,
    smooth,
    appName,
    ($=?),
)
where

import FRP.Yampa
import Control.Monad.IO.Class
import Control.Monad
import Linear as L
import qualified Sound.OpenAL as AL
import Data.StateVar

-----------------------------------------------------------
($=?) :: (MonadIO m) => StateVar x -> Bool -> x -> m ()
($=?) field cond value = 
    when cond (field $= value)

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
_vectorToV3 :: AL.Vector3 AL.ALfloat -> V3 Float
{-# INLINE _vectorToV3 #-}
_vectorToV3 (AL.Vector3 x y z) =
    V3
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
smooth :: (Ord c, Fractional c) => Time -> c -> c -> SF a c
smooth for max_ value = do
    let dtb = for / 2
    let dts = dtb / 3
    if value > max_
        then
            delay dts max_
                <<< delay dts ((max_ + value) / 2)
                <<< delay dts (max_ + (value / 2))
                <<< delay dtb value
                <<< constant (max_ + (value / 2))
        else constant value

-----------------------------------------------------------
instance (Eq a, Floating a) => VectorSpace (V2 a) a where
    zeroVector = L.zero
    (*^) = (L.*^)
    negateVector = L.negated
    (^+^) = (L.^+^)
    (^-^) = (L.^-^)
    dot = L.dot

-----------------------------------------------------------
instance (Eq a, Floating a) => VectorSpace (V3 a) a where
    zeroVector = L.zero
    (*^) = (L.*^)
    negateVector = L.negated
    (^+^) = (L.^+^)
    (^-^) = (L.^-^)
    dot = L.dot

appName = "[Yampa-OpenAL]"
