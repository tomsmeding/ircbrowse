{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Ircbrowse.Model.Data (
    StatData(..),
    StatDataPer(..),
    Activity(..),
    sdpTalksPerDay,
    statDataPerEmpty,
    mget,
    parseRemember,
) where

import Data.IRC.Provider
import Ircbrowse.Event
import Ircbrowse.Model.Data.Recent
-- import Ircbrowse.Monads
import Ircbrowse.Types.Import

import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
-- import Snap.App


-- | Per-channel statistics.
data StatDataPer = StatDataPer
    { -- | Total number of events ever
      sdpTotalEvents :: Integer
    , -- | Total events per year
      sdpYearEvents :: Map Integer Integer
    , -- | Total @talk@ and @act@ events per year
      sdpYearTalkActs :: Map Integer Integer
    , -- | Total events per day
      sdpDayEvents :: Map Day Integer
    , -- | Grouped by hour-of-day, number of @talk@ events
      sdpHourTalks :: Map Day (Map Int Int)
    , -- | Per day per nick, their number of @talk@ events.
      sdpNickTalks :: Map Day (Map Text Int)
    , -- | List of "@remember " quotes in reverse chronological order (newest first).
      sdpRemembers :: [Event]
    }

sdpTalksPerDay :: StatDataPer -> Map Day Integer
sdpTalksPerDay stats = Map.map (sum . map fromIntegral . Map.elems) (sdpNickTalks stats)

-- | This activity considers an event only relevant if it is {talk,act} and the
-- text does NOT match /^@|^> |^:t |^:k |^% /.
data Activity = Activity
    { actNumMsg :: Int
    , actNumWords :: Int
    }

data StatData = StatData
    { -- | Per-channel statistics
      sdPerChan :: Map Channel StatDataPer
    , -- | Per nick, aggregated per hour, its recent activity
      sdNickHourActivity :: Map Text (Map Int (Recent Activity))
    , -- | Per nick, aggregated per year, its activity
      sdNickYearActivity :: Map Text (Map Integer Activity)
    , -- | Per channel, its recent activity per nick
      sdChannelNickActivity :: Map Channel (Map Text (Recent Activity))
    , -- | Quotes per day per nick ({talk,act} messages matching
      -- /^@remember ([^ ]+) (.*)/ with \1 = nick; \2 is the quote); ordered
      -- reverse-chronologically (newest first).
      sdQuotes :: Map Text [Event]
    , -- | Karma of each nick (incremented by ++nick messages, decremented by
      -- nick messages)
      sdKarma :: Map Text Integer
    }

instance Semigroup Activity where
    Activity a b <> Activity a' b' = Activity (a + a') (b + b')
instance Monoid Activity where
    mempty = Activity 0 0

instance ProviderStats StatData where
    statsEmpty = StatData
        { sdPerChan = mempty
        , sdNickHourActivity = mempty
        , sdNickYearActivity = mempty
        , sdChannelNickActivity = mempty
        , sdQuotes = mempty
        , sdKarma = mempty
        }

    -- TODO: optimise for batch indexing
    statsAddBatch l s = foldl' (flip statsAdd) s l

    statsAdd e st =
        st { sdPerChan = upsert (eventChannel e) statDataPerEmpty (statsPerAdd e) (sdPerChan st)
           , sdNickHourActivity =
                withNickTalkAct (\nick -> upsert nick mempty (upsert hour recentEmpty (recentAddNew day (eventActivity e)))) (sdNickHourActivity st)
           , sdNickYearActivity =
                withNickTalkAct (\nick -> upsert nick mempty (upsert year mempty (<> eventActivity e))) (sdNickYearActivity st)
           , sdChannelNickActivity =
                withNickTalkAct (\nick -> upsert (eventChannel e) mempty (upsert nick recentEmpty (recentAddNew day (eventActivity e)))) (sdChannelNickActivity st)
           , sdQuotes =
               if eventType e == "talk" || eventType e == "act"
                   then case parseRemember (eventText e) of
                          Just (nick, _) ->
                              upsert nick [] (e :) (sdQuotes st)
                          _ -> sdQuotes st
                   else sdQuotes st
           , sdKarma =
               if | Just nick <- T.stripPrefix "++" (eventText e) -> upsert nick 0 succ (sdKarma st)
                  | Just nick <- T.stripPrefix "--" (eventText e) -> upsert nick 0 pred (sdKarma st)
                  | otherwise -> sdKarma st
           }
      where
        utc = zonedTimeToUTC (eventTimestamp e)
        day = utctDay utc
        (year, _, _) = toGregorian day
        hour = floor (realToFrac (utctDayTime utc) :: Double)

        withNickTalkAct :: (Text -> a -> a) -> a -> a
        withNickTalkAct f val
          | Just nick <- eventNick e
          , eventType e == "talk" || eventType e == "act"
          = f nick val
          | otherwise
          = val

statDataPerEmpty :: StatDataPer
statDataPerEmpty = StatDataPer
    { sdpTotalEvents = 0
    , sdpYearEvents = mempty
    , sdpYearTalkActs = mempty
    , sdpDayEvents = mempty
    , sdpHourTalks = mempty
    , sdpRemembers = []
    , sdpNickTalks = mempty
    }

statsPerAdd :: Event -> StatDataPer -> StatDataPer
statsPerAdd ev sdp =
    sdp { sdpTotalEvents = succ (sdpTotalEvents sdp)
        , sdpYearEvents = upsert year 0 succ (sdpYearEvents sdp)
        , sdpYearTalkActs = if eventType ev == "talk" || eventType ev == "act"
                                then upsert year 0 succ (sdpYearTalkActs sdp)
                                else sdpYearTalkActs sdp
        , sdpDayEvents = upsert day 0 succ (sdpDayEvents sdp)
        , sdpHourTalks = if eventType ev == "talk"
                             then upsert day mempty (upsert hour 0 succ) (sdpHourTalks sdp)
                             else sdpHourTalks sdp
        , sdpRemembers = if (eventType ev == "talk" || eventType ev == "act")
                                && "@remember " `T.isPrefixOf` eventText ev
                             then ev : sdpRemembers sdp
                             else sdpRemembers sdp
        , sdpNickTalks = case eventNick ev of
                           Just nick | eventType ev == "talk" ->
                               upsert day mempty (upsert nick 0 succ) (sdpNickTalks sdp)
                           _ -> sdpNickTalks sdp
        }
  where
    utc = zonedTimeToUTC (eventTimestamp ev)
    day = utctDay utc
    (year, _, _) = toGregorian day
    hour = floor (realToFrac (utctDayTime utc) :: Double)

eventActivity :: Event -> Activity
eventActivity ev =
    Activity { actNumMsg = 1
             , actNumWords = length (T.words (eventText ev))}

upsert :: Ord k => k -> v -> (v -> v) -> Map k v -> Map k v
upsert k emptyv vf m = Map.insert k (vf (fromMaybe emptyv (Map.lookup k m))) m

mget :: Ord k => v -> k -> Map k v -> v
mget v k = fromMaybe v . Map.lookup k

parseRemember :: Text -> Maybe (Text, Text)
parseRemember line = do
  suf <- T.stripPrefix "@remember " line
  return (fmap (T.dropWhile (== ' ')) (T.breakOn " " suf))

-- TODO: Migrate this to new logs reading model
-- | Generate everything.
-- generateData :: Model c s ()
-- generateData = do
--   void $ do
--     exec ["delete from conversation_by_year"] ()
--     exec ["delete from general_activity_by_year"] ()
--     forM_ [toEnum 0..] $ \channel -> do
--       io (putStrLn ("Generating data for " ++ prettyChanWithNetwork channel ++ " ..."))
--       let cid = showChanInt channel

--       io (putStrLn ("  Generating stats ..."))
--       exec ["insert into conversation_by_year select date_part('year',timestamp),count(*),? from event where channel = ? and type in ('talk','act') group by date_part('year',timestamp) order by 1;"] (cid,cid)
--       exec ["insert into general_activity_by_year select date_part('year',timestamp),count(*),? from event where channel = ? group by date_part('year',timestamp) order by 1;"] (cid,cid)

--       io (putStrLn ("  Generating PDF indexes ..."))
--       exec ["delete from event_order_index where idx = (? * 1000) + 1"]
--            (Only (showChanInt channel))
--       exec ["insert into event_order_index"
--            ,"SELECT RANK() OVER(ORDER BY id desc) AS id,"
--            ,"id as origin, "
--            ,"(channel * 1000) + 1 as idx "
--            ,"FROM event "
--            ,"WHERE channel = ? and "
--            ,"text LIKE '%http%.pdf%' AND "
--            ,"text ~ ?"
--            ,"order by id asc;"]
--            (showChanInt channel,"https?://[^ ]+\\.pdf")
