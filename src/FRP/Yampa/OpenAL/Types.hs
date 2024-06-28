{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FRP.Yampa.OpenAL.Types
-- TODO add export list
where

import Control.Concurrent.MVar
import Data.IORef
import Data.Map (Map)
import Data.Set (Set)
import qualified Sound.OpenAL as AL

-- | Internal.
data ALApp = ALApp
    { sourceMap :: !(MVar (Map String AL.Source))
    , createdMap :: !(IORef (Set String))
    }

-----------------------------------------------------------
type MetersPerSecond = Float

-----------------------------------------------------------
type Meters = Float

-----------------------------------------------------------
type Factor = Float

-----------------------------------------------------------
type Gain = Double

-----------------------------------------------------------
type Angle = Double

-----------------------------------------------------------
type Pitch = Double

-----------------------------------------------------------
type Depth = Double {- 0 to 1 -}

-----------------------------------------------------------
type Rate = Double
