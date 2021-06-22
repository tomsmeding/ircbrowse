{-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Ircbrowse.Model.Data (
    StatData(..),
    StatDataPer(..),
    NickActivity(..),
    Activity(..),
    sdpTalksPerDay,
    statDataPerEmpty,
    mget,
) where

import Data.IRC.Provider
import Ircbrowse.Event
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

data NickActivity = NickActivity
    { -- | Grouped by hour-of-day, the nick's global activity
      naHourActivity :: Map Int Activity
    , -- | Grouped by year, the nick's global activity
      naYearActivity :: Map Integer Activity
    , -- | Quotes of this nick ({talk,act} messages matching
      -- /^@remember ([^ ]+) (.*)/ with \1 = nick; \2 is the quote)
      naQuotes :: [Text]
    }

data StatData = StatData
    { -- | Per-channel statistics
      sdPerChan :: Map Channel StatDataPer
    , -- | Per day, per nick, its activity
      sdNickActivity :: Map Day (Map Text NickActivity)
    }

instance Semigroup Activity where
    Activity a b <> Activity a' b' = Activity (a + a') (b + b')
instance Monoid Activity where
    mempty = Activity 0 0

instance Semigroup NickActivity where
    NickActivity a b c <> NickActivity a' b' c' =
        NickActivity (Map.unionWith (<>) a a') (Map.unionWith (<>) b b') (c ++ c')
instance Monoid NickActivity where
    mempty = NickActivity mempty mempty []

instance ProviderStats StatData where
    statsEmpty = StatData
        { sdPerChan = mempty
        , sdNickActivity = mempty
        }

    -- TODO: optimise for batch indexing
    statsAddBatch l s = foldl' (flip statsAdd) s l

    statsAdd e st =
        st { sdPerChan = upsert (eventChannel e) statDataPerEmpty (statsPerAdd e) (sdPerChan st)
           , sdNickActivity =
                let utc = zonedTimeToUTC (eventTimestamp e)
                    day = utctDay utc
                in case eventNick e of
                     Just nick
                       | eventType e == "talk" || eventType e == "act"
                       -> upsert day mempty (upsert nick mempty (nickActivityAdd e)) (sdNickActivity st)
                     _ -> sdNickActivity st
           }

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
                                && eventText ev `startsWith` "@remember "
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

-- | Assumes that the given event has type {talk,act}!
nickActivityAdd :: Event -> NickActivity -> NickActivity
nickActivityAdd ev na =
    na { naHourActivity =
             if isRealConvo
                 then upsert hour mempty (activityAdd (eventText ev)) (naHourActivity na)
                 else naHourActivity na
       , naYearActivity =
             if isRealConvo
                 then upsert year mempty (activityAdd (eventText ev)) (naYearActivity na)
                 else naYearActivity na
       , naQuotes = TODO
-- define a "real conversation" message as one matching:
--   NOT (text ~ /^@|^> |^:t |^:k |^% /)

-- statistics:
--   per Day:
--     per nick:
--       grouped by hour-of-day:
--         - total number of real conversation messages
--         - total number of words in real conversation messages
--       grouped by year:
--         - total number of real conversation messages
--         - total number of words in real conversation messages
--       list of {talk,act} messages matching /^@remember ([^ ]+) (.*)/ where \1 = nick; value stored is \2
--   per nick:
--     integer karma
       }
  where
    utc = zonedTimeToUTC (eventTimestamp ev)
    day = utctDay utc
    (year, _, _) = toGregorian day
    hour = floor (realToFrac (utctDayTime utc) :: Double)

    isRealConvo = not (any (eventText ev `startsWith`) ["@", "> ", ":t ", ":k ", "% "])

    activityAdd :: Text -> Activity -> Activity
    activityAdd text act =
        act { actNumMsg = actNumMsg act + 1
            , actNumWords = actNumWords act + length (T.words text) }

startsWith :: Text -> Text -> Bool
startsWith long short = T.take (T.length short) long == short

upsert :: Ord k => k -> v -> (v -> v) -> Map k v -> Map k v
upsert k emptyv vf m = Map.insert k (vf (fromMaybe emptyv (Map.lookup k m))) m

mget :: Ord k => v -> k -> Map k v -> v
mget v k = fromMaybe v . Map.lookup k

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
