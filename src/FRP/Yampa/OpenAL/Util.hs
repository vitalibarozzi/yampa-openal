{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FRP.Yampa.OpenAL.Util where

import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import qualified Sound.OpenAL as AL

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
