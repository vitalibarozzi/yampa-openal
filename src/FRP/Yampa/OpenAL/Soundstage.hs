{-# LANGUAGE BlockArguments #-}


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}

{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module FRP.Yampa.OpenAL.Soundstage
where

import FRP.Yampa.OpenAL.Listener
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

{- | A model of the how the sound elements are defined at
one specific point in time.
-}
data Soundstage = Soundstage
    { soundstageSources {--------} :: !(Map AL.Source Source)
    , soundstageDopplerFactor {--} :: !Factor
    , soundstageSpeedOfSound {---} :: !MetersPerSecond
    , soundstageDistanceModel {--} :: !AL.DistanceModel
    , soundstageListener {-------} :: !Listener
    , soundstageTime {-----------} :: !(Maybe Time)
    }
