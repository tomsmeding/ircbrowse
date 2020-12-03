{-# LANGUAGE FlexibleContexts #-}
module Ircbrowse.PerfStats (
  -- * Types
  PerfStatsCtx,
  PerfStats(..),
  Stats(..),

  -- * Contexts
  newPerfContext,
  currentPerfStats,

  -- * Record statistics
  wrapTimedRoute
) where

import Control.Exception.Lifted (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Monoid (Sum(..))
import Data.Semigroup (Min(..), Max(..))
import qualified System.Clock as Clock


-- | Numeric statistics semigroup.
data Stats a =
      Stats { sMin :: Min a
            , sMax :: Max a
            , sTotal :: Sum a
            , sCount :: Sum Int }
  deriving (Show)

instance (Ord a, Num a) => Semigroup (Stats a) where
  Stats a b c d <> Stats a' b' c' d' = Stats (a <> a') (b <> b') (c <> c') (d <> d')

singletonStats :: a -> Stats a
singletonStats x = Stats { sMin = Min x, sMax = Max x, sTotal = Sum x, sCount = Sum 1 }


-- | Performance statistics context, collects statistics.
data PerfStats =
      PerfStats { cRouteTimes :: Map ByteString (Stats Double) }
  deriving (Show)

newtype PerfStatsCtx = PerfStatsCtx (TVar PerfStats)

-- | Create a new context for recording performance statistics.
newPerfContext :: MonadIO m => m PerfStatsCtx
newPerfContext = liftIO (PerfStatsCtx <$> newTVarIO (PerfStats mempty))

currentPerfStats :: MonadIO m => PerfStatsCtx -> m PerfStats
currentPerfStats (PerfStatsCtx statsvar) = liftIO (readTVarIO statsvar)

-- | Log the time taken for a route handler.
wrapTimedRoute :: (MonadIO m, MonadBaseControl IO m) => PerfStatsCtx -> m ByteString -> m a -> m a
wrapTimedRoute ctx geturi action = do
  bracket (liftIO (Clock.getTime Clock.Monotonic))
          (\tStart -> do
              tEnd <- liftIO (Clock.getTime Clock.Monotonic)
              uri <- geturi
              let difft = fromInteger (Clock.toNanoSecs (tEnd - tStart)) / (10 ** 9)
              liftIO (submitRouteTiming ctx uri difft))
          (const action)

submitRouteTiming :: PerfStatsCtx -> ByteString -> Double -> IO ()
submitRouteTiming (PerfStatsCtx statsvar) uri time =
  atomically . modifyTVar' statsvar $ \stats ->
    stats { cRouteTimes = Map.insertWith (<>) uri (singletonStats time) (cRouteTimes stats) }
