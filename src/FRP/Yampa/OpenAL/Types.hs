module FRP.Yampa.OpenAL.Types
    ( ALApp(..)
    , Angle
    , Pitch
    , Meters
    , MetersPerSecond
    , Factor
    , ChannelType(..)
    , Magnitude(..)
    )
where

import Control.Concurrent.MVar
import Data.IORef
import Data.Map (Map)
import Data.Set (Set)
import qualified Sound.OpenAL as AL

-- | Internal. and probably temporary. currently not been used
data ALApp = ALApp
    { sourceMap :: !(MVar (Map String AL.Source))
    , createdMap :: !(IORef (Set String))
    }

-----------------------------------------------------------
data ChannelType = Stereo | Mono deriving (Eq,Show)

-----------------------------------------------------------
data Magnitude = Bit16 | Bit8 deriving (Eq,Show)

-----------------------------------------------------------
type MetersPerSecond = Float

-----------------------------------------------------------
type Meters = Float

-----------------------------------------------------------
type Factor = Float

-----------------------------------------------------------
type Angle = Double

-----------------------------------------------------------
type Pitch = Double

