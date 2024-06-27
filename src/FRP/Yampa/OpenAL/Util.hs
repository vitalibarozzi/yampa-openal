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
    appName,
    ($=?),
)
where

import Control.Monad
import Control.Monad.IO.Class
import Data.StateVar
import FRP.Yampa
import Linear as L
import qualified Sound.OpenAL as AL

-----------------------------------------------------------
($=?) :: (MonadIO m) => StateVar x -> Bool -> x -> m ()
{-# INLINE ($=?) #-}
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
instance (Eq a, Floating a) => VectorSpace (V2 a) a where
    {-# INLINE zeroVector #-}
    zeroVector = L.zero
    {-# INLINE (*^) #-}
    (*^) = (L.*^)
    {-# INLINE negateVector #-}
    negateVector = L.negated
    {-# INLINE (^+^) #-}
    (^+^) = (L.^+^)
    {-# INLINE (^-^) #-}
    (^-^) = (L.^-^)
    {-# INLINE dot #-}
    dot = L.dot

-----------------------------------------------------------
instance (Eq a, Floating a) => VectorSpace (V3 a) a where
    {-# INLINE zeroVector #-}
    zeroVector = L.zero
    {-# INLINE (*^) #-}
    (*^) = (L.*^)
    {-# INLINE negateVector #-}
    negateVector = L.negated
    {-# INLINE (^+^) #-}
    (^+^) = (L.^+^)
    {-# INLINE (^-^) #-}
    (^-^) = (L.^-^)
    {-# INLINE dot #-}
    dot = L.dot

appName = "[Yampa-OpenAL]"
