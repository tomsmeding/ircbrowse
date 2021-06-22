-- | General statistics.

module Ircbrowse.Model.Stats where

import Data.IRC.Provider
import Ircbrowse.Types
import Ircbrowse.Data
import Ircbrowse.Model.Data
import Ircbrowse.Types.Import

import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
-- import Data.Time.Calendar
import Numeric
import Snap.App
import System.Random

getStats :: Channel -> Range -> Model c PState Stats
getStats channel range@(Range from to) = do
  provider <- reader (stateProvider . modelStateAnns)
  stats <- liftIO $ providerStats provider
  let stats' = mget statDataPerEmpty channel (sdPerChan stats)

  let count = sum [mget 0 day (sdpDayEvents stats') | day <- [from..pred to]]
  -- count <- single ["SELECT COUNT(*)"
  --                 ,"FROM event"
  --                 ,"WHERE channel = ? and timestamp > ? and timestamp < ?"]
  --                 (cid,from,to)

  let msgcount = sum [mget 0 day (sdpTalksPerDay stats') | day <- [from..pred to]]
  -- msgcount <- single ["SELECT COUNT(*)"
  --                    ,"FROM event"
  --                    ,"WHERE type = 'talk'"
  --                    ,"AND channel = ? and timestamp > ? and timestamp < ?"]
  --                    (cid,from,to)

  let nicks = fromIntegral $
        sum [length . filter ((> 0) . snd) . Map.toList $ mget mempty day (sdpNickTalks stats')
            | day <- [from..pred to]]
  -- nicks <- single ["SELECT COUNT(DISTINCT nick)"
  --                 ,"FROM event"
  --                 ,"WHERE channel = ? and type = 'talk'"
  --                 ,"AND timestamp > ? and timestamp < ?"]
  --                 (cid,from,to)

  let activetimes = Map.assocs $
        Map.unionsWith (+) [Map.map fromIntegral (mget mempty day (sdpHourTalks stats'))
                           | day <- [from..pred to]]
  -- activetimes <- query ["SELECT DATE_PART('HOUR',timestamp)::int,COUNT(*)"
  --                      ,"FROM EVENT"
  --                      ,"WHERE channel = ? and type = 'talk'"
  --                      ,"AND timestamp > ? AND timestamp < ?"
  --                      ,"GROUP BY DATE_PART('HOUR',timestamp)"
  --                      ,"ORDER BY 1 ASC"]
  --                      (cid,from,to)

  let dailyactivity = Map.toList $ Map.fromListWith (+)
                        [let (_, _, daynum) = toGregorian day
                         in (daynum, mget 0 day (sdpTalksPerDay stats'))
                        | day <- [from..pred to]]
  -- dailyactivity <- query ["SELECT date_part('day',date)::int,count FROM"
  --                        ," (SELECT timestamp::date as date,COUNT(*) as count"
  --                        ,"  FROM EVENT"
  --                        ,"  WHERE channel = ? and type = 'talk'"
  --                        ,"  AND timestamp > ? AND timestamp < ?"
  --                        ,"  GROUP BY timestamp::date"
  --                        ,"  ORDER BY 1 ASC) c"]
  --                        (cid,from,to)

  nickstats <- map (fmap fromIntegral) <$> getNickStats channel range

  let activitybyyear = Map.assocs (sdpYearEvents stats')
  -- activitybyyear <- query ["SELECT year,lines FROM general_activity_by_year where channel = ? order by year asc"]
  --                         (Only cid)

  let conversationbyyear = Map.assocs (sdpYearTalkActs stats')
  -- conversationbyyear <- query ["SELECT year,lines FROM conversation_by_year where channel = ? order by year asc"]
  --                             (Only cid)

  return Stats
    { stEventCount    = count
    , stMsgCount      = msgcount
    , stNickCount     = nicks
    , stActiveTimes   = activetimes
    , stDailyActivity = dailyactivity
    , stActiveNicks   = nickstats
    , stActivityByYear = activitybyyear
    , stConversationByYear = conversationbyyear
   }

  -- where cid = showChanInt channel

getNickStats :: Channel -> Range -> Model c PState [(Text,Int)]
getNickStats channel (Range from to) = do
  provider <- reader (stateProvider . modelStateAnns)
  stats <- liftIO $ providerStats provider
  let stats' = mget statDataPerEmpty channel (sdPerChan stats)
  return (Map.toList $ Map.unionsWith (+)
            [mget mempty day (sdpNickTalks stats')
            | day <- [from..pred to]])
  -- query ["SELECT nick,COUNT(*)"
  --       ,"FROM EVENT"
  --       ,"WHERE channel = ? and type = 'talk'"
  --       ,"AND timestamp > ?"
  --       ,"AND timestamp < ?"
  --       ,"GROUP BY nick"
  --       ,"ORDER BY 2 DESC"
  --       ,"LIMIT 200"]
  --       (showChanInt channel,from,to)

-- | Some test stats.
sampleStats :: Stats
sampleStats         = Stats
  { stEventCount    = 110000
  , stMsgCount      = 100000
  , stNickCount     = 1000
  , stActiveNicks   = sortBy (flip (comparing snd))
                             (zipWith (\r i -> (pack (showHex (r+i) ""),r))
                                      (randomRs (0,10000) (mkStdGen 1))
                                      [1..10])
  , stActiveTimes   = zipWith (\r hour -> (hour,r))
                              (randomRs (0,60) (mkStdGen 2))
                              [0..23]
  , stDailyActivity = zipWith (\r day -> (day,r))
                              (randomRs (0,60) (mkStdGen 3))
                              [1..31]
  , stActivityByYear = []
  , stConversationByYear = []
  }
