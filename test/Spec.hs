{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import FRP.Yampa
import FRP.Yampa.OpenAL
import qualified Data.Map as Map
import qualified FRP.Yampa as Yampa


main :: IO ()
main = do
    withALUT \alApp -> do
      let tdelay = 16660
      let dt = realToFrac tdelay / 1000000
      handle <- Yampa.reactInit (pure 0) (\_ updated ss -> when updated (runSoundscape alApp ss) >> pure updated) sf
      forever do
          concurrently
              (threadDelay tdelay)
              (Yampa.react handle (dt, Nothing))
  where
    rS hz = (source (Sine hz 1)) { sourceGain = 0.4 }
    fooss = source (File "test/audio.wav" Nothing)
    sf = proc _ -> do
        xs <- mkSrc <<< Yampa.time -< ()
        returnA -< soundscape { soundscapeSources = Map.fromList xs }
      where
        id2 = 200
        d   = 0.2
        src1 = (100,)
        mkSrc = arr 
            \case t | t <= (0 * d) -> pure (src1 (rS 840))
                    | t <= (1 * d) -> mempty
                    | t <= (2 * d) -> pure (src1 (rS 940))
                    | t <= (3 * d) -> mempty
                    | t <= (4 * d) -> pure (src1 (rS 1040))
                    | t <= (5 * d) -> mempty
                    | t <= (6 * d) -> pure (src1 (rS 1120))
                    | t <= (7 * d) -> mempty
                    | t <= (8 * d) -> pure (id2, rS 840)
                    | otherwise    -> pure (src1 fooss)
