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
  wrapTimedRoute,
  wrapTimedFunc,
  wrapTimedFunc'
) where

import Control.Exception.Lifted (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
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
      PerfStats { cRouteTimes :: Map ByteString (Stats Double)
                , cFuncTimes :: Map ByteString (Stats Double) }
  deriving (Show)

newtype PerfStatsCtx = PerfStatsCtx (TVar PerfStats)

-- Lenses are for smart people
type GetSet a b = (a -> b, a -> b -> a)

cRouteTimes' :: GetSet PerfStats (Map ByteString (Stats Double))
cRouteTimes' = (cRouteTimes, \s m -> s { cRouteTimes = m })

cFuncTimes' :: GetSet PerfStats (Map ByteString (Stats Double))
cFuncTimes' = (cFuncTimes, \s m -> s { cFuncTimes = m })

-- | Create a new context for recording performance statistics.
newPerfContext :: MonadIO m => m PerfStatsCtx
newPerfContext = liftIO (PerfStatsCtx <$> newTVarIO (PerfStats mempty mempty))

currentPerfStats :: MonadIO m => PerfStatsCtx -> m PerfStats
currentPerfStats (PerfStatsCtx statsvar) = liftIO (readTVarIO statsvar)

wrapTimed :: (MonadIO m, MonadBaseControl IO m)
          => PerfStatsCtx -> m ByteString -> (PerfStatsCtx -> ByteString -> Double -> IO ())
          -> m a -> m a
wrapTimed ctx getkey submitter action =
  bracket (liftIO (Clock.getTime Clock.Monotonic))
          (\tStart -> do
              tEnd <- liftIO (Clock.getTime Clock.Monotonic)
              uri <- getkey
              let difft = fromInteger (Clock.toNanoSecs (tEnd - tStart)) / (10 ** 9)
              liftIO (submitter ctx uri difft))
          (const action)

-- | Does not log upon exception.
wrapTimedPlain :: MonadIO m
               => PerfStatsCtx -> m ByteString -> (PerfStatsCtx -> ByteString -> Double -> IO ())
               -> m a -> m a
wrapTimedPlain ctx getkey submitter action = do
  tStart <- liftIO (Clock.getTime Clock.Monotonic)
  result <- action
  tEnd <- liftIO (Clock.getTime Clock.Monotonic)
  key <- getkey
  let difft = fromInteger (Clock.toNanoSecs (tEnd - tStart)) / (10 ** 9)
  liftIO (submitter ctx key difft)
  return result

-- | Log the time taken for a route handler. Exceptions are counted as function completion.
wrapTimedRoute :: (MonadIO m, MonadBaseControl IO m) => PerfStatsCtx -> m ByteString -> m a -> m a
wrapTimedRoute ctx geturi = wrapTimed ctx geturi (submitTiming cRouteTimes')

-- | Log the time taken for a monadic action. Exceptions are counted as function completion.
wrapTimedFunc :: (MonadIO m, MonadBaseControl IO m) => String -> PerfStatsCtx -> m a -> m a
wrapTimedFunc key ctx = wrapTimed ctx (return (BS8.pack key)) (submitTiming cFuncTimes')

-- | Log the time taken for a monadic action, cancelling the log on exception.
wrapTimedFunc' :: MonadIO m => String -> m PerfStatsCtx -> m a -> m a
wrapTimedFunc' key getctx action =
  getctx >>= \ctx -> wrapTimedPlain ctx (return (BS8.pack key)) (submitTiming cFuncTimes') action

submitTiming :: GetSet PerfStats (Map ByteString (Stats Double))
             -> PerfStatsCtx -> ByteString -> Double -> IO ()
submitTiming (get, set) (PerfStatsCtx statsvar) key time =
  atomically . modifyTVar' statsvar $ \stats ->
    set stats (Map.insertWith (<>) key (singletonStats time) (get stats))
