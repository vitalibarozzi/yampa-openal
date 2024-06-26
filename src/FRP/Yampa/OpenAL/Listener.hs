{-# LANGUAGE BlockArguments #-}


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}

{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module FRP.Yampa.OpenAL.Listener
where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Map ( Map, Map )
import qualified Data.Map as Map
import Data.Maybe
import qualified FRP.Yampa as Yampa
import FRP.Yampa.OpenAL.Types
import FRP.Yampa.OpenAL.Util
import qualified Sound.ALUT.Initialization as AL
import qualified Sound.OpenAL as AL
import FRP.Yampa.OpenAL.Source
import FRP.Yampa
import Linear as L
import qualified Sound.ALUT.Loaders as AL
import qualified Sound.OpenAL.AL.Buffer as AL
import Data.IORef
import Data.Bifunctor

-----------------------------------------------------------
data Listener = Listener_
    { listenerPosition {-----} :: !(V3 Float) -- meters
    , listenerVelocity {-----} :: !(V3 Float) -- meters per second
    , listenerOrientation {--} :: !(V3 Float, V3 Float) -- meters
    , listenerGain {---------} :: !Gain
    }
    deriving
        (Eq, Show)

