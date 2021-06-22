module Ircbrowse.Model.Data.Recent (
    Recent,
    recentEmpty,
    recentAddNew,
    recentRecent,
    recentAlltime,
) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Time (Day, addDays)


-- | Container for keeping stats on both the past 31 days, as well as all time.
data Recent a = Recent (Map Day a) a

-- | Given an initial stats value.
recentEmpty :: Monoid a => Recent a
recentEmpty = Recent mempty mempty

-- | Add a new stats item into the cache. The existing value will be combined
-- with the newly added value using '(<>)', in the order @existing_value <>
-- new_value@.
recentAddNew :: Semigroup a => Day -> a -> Recent a -> Recent a
recentAddNew day val (Recent mp alltime) =
    Recent (pruneRecentCache day (Map.insertWith (flip (<>)) day val mp))
           (alltime <> val)

-- | Get stats for the last 31 days.
recentRecent :: Monoid a => Recent a -> a
recentRecent (Recent mp _) = mconcat (Map.elems mp)

-- | Get stats for all time.
recentAlltime :: Recent a -> a
recentAlltime (Recent _ alltime) = alltime

pruneRecentCache :: Day -> Map Day a -> Map Day a
pruneRecentCache today mp
  | Just ((day, _), mp') <- Map.minViewWithKey mp
  , day < addDays (-31) today
  = pruneRecentCache day mp'
  | otherwise
  = mp
