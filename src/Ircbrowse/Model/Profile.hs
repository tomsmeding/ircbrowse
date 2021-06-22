{-# LANGUAGE OverloadedStrings #-}

-- | Statistics for a specific nick's profiling.

module Ircbrowse.Model.Profile (activeHours, NickStats(..)) where

import Data.IRC.Provider
import Ircbrowse.Data
import Ircbrowse.Event
import Ircbrowse.Model.Data
import Ircbrowse.Model.Data.Recent
import Ircbrowse.PerfStats (wrapTimedFunc')
import Ircbrowse.Types

import Control.Monad.Reader (reader)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import Snap.App

data NickStats = NickStats
  { nickHours  :: ![(Int,Int,Int,Int)]
  , nickYears  :: ![(Integer,Int,Int,Int)]
  , nickLines  :: !Int
  , nickQuote  :: !(Maybe (Text,Text))
  , nickQuotes :: ![(EventId,UTCTime,Text)]
  , nickKarma  :: !Integer
  }

wrapTimed :: String -> Model c PState a -> Model c PState a
wrapTimed key = wrapTimedFunc' key (reader (statePerfCtx . modelStateAnns)) 

activeHours :: Text -> Bool -> Model c PState NickStats
activeHours nick recent = wrapTimed "activeHours" (activeHours' nick recent)

activeHours' :: Text -> Bool -> Model c PState NickStats
activeHours' nick recent = do
  let unRecent :: Recent a -> a
      unRecent | recent = recentRecent
               | otherwise = recentAlltime

  provider <- reader (stateProvider . modelStateAnns)
  stats <- liftIO $ providerStats provider

  let doublediv a b = (fromIntegral a :: Double) / fromIntegral b

  today <- utctDay <$> liftIO getCurrentTime

  -- grouped by hour-of-day:
  --   (hour
  --   ,average number of words per real conversation message
  --   ,total number of words in real conversation messages
  --   ,number of real conversation messages)
  let hours = [(hour, round (actNumWords act `doublediv` actNumMsg act), actNumWords act, actNumMsg act)
              | (hour, act) <- map (fmap unRecent) $ Map.toList (mget mempty nick (sdNickHourActivity stats))]
  -- hours <- wrapTimed "activeHours-q1" $
  --          query ["SELECT"
  --                ,"DATE_PART('HOUR',timestamp) :: integer AS hour,"
  --                ,"AVG(ARRAY_UPPER(STRING_TO_ARRAY(text,' '),1)) :: integer,"
  --                ,"SUM(ARRAY_UPPER(STRING_TO_ARRAY(text,' '),1)) :: integer,"
  --                ,"COUNT(*) AS lines"
  --                ,"FROM event"
  --                ,"WHERE nick = ? and type in ('talk','act') AND"
  --                ,"(? OR (timestamp > ? AND timestamp < ?)) AND"
  --                ,"NOT (text LIKE '@%' OR text LIKE '> %' OR text LIKE ':t %' OR text LIKE ':k %'"
  --                ,"OR text LIKE 'lambdabot: %' OR text ~ '^[^ :]: ')"
  --                ,"GROUP BY DATE_PART('HOUR',timestamp)"
  --                ,"ORDER BY hour;"]
  --                (nick,alltime,from,to)

  -- grouped by year:
  --   (year
  --   ,average number of words per real conversation message
  --   ,total number of words in real conversation messages
  --   ,number of real conversation messages)
  -- Only valid if 'recent' is False.
  let years
        | recent = []
        | otherwise = [(year, round (actNumWords act `doublediv` actNumMsg act), actNumWords act, actNumMsg act)
                      | (year, act) <- Map.toList (mget mempty nick (sdNickYearActivity stats))]
  -- years <- wrapTimed "activeHours-q2" $
  --          query ["SELECT"
  --                ,"DATE_PART('YEAR',timestamp) :: integer,"
  --                ,"ROUND(AVG(ARRAY_UPPER(STRING_TO_ARRAY(text,' '),1))) :: integer,"
  --                ,"SUM(ARRAY_UPPER(STRING_TO_ARRAY(text,' '),1)) :: integer,"
  --                ,"COUNT(*) :: integer"
  --                ,"FROM event"
  --                ,"WHERE nick = ? and TYPE in ('talk','act') AND"
  --                ,"(? OR (timestamp > ? AND timestamp < ?)) AND"
  --                ,"NOT (text LIKE '@%' OR text LIKE '> %' OR text LIKE ':t %' OR text LIKE ':k %'"
  --                ,"OR text LIKE 'lambdabot: %')"
  --                ,"GROUP BY DATE_PART('YEAR',timestamp)"
  --                ,"ORDER BY DATE_PART('YEAR',timestamp);"]
  --                (nick,alltime,from,to)

  let lines = sum (map (\(_,_,_,lines) -> lines) years)

  -- (type, text) for a random event in the time range with type in {talk,act}
  rquote <- wrapTimed "activeHours-q3" $
            query ["SELECT"
                  ,"type,text"
                  ,"FROM event"
                  ,"WHERE nick = ? and TYPE in ('talk','act') AND"
                  ,"(? OR (timestamp > ? AND timestamp < ?)) AND"
                  ,"NOT (text LIKE '@%' OR text LIKE '> %' OR text LIKE ':t %' OR text LIKE ':k %'"
                  ,"OR text LIKE 'lambdabot: %')"
                  ,"OFFSET random()*?"
                  ,"LIMIT 1"]
                  (nick,alltime,from,to,lines)

  -- list of quotes of this person in the time range
  let quotes =
          let isInRange ev | recent = utctDay (zonedTimeToUTC (eventTimestamp ev)) >= addDays (-31) today
                           | otherwise = True
          in [(eventId ev, zonedTimeToUTC (eventTimestamp ev), nick <> T.pack " " <> quote)
             | ev <- takeWhile isInRange (mget mempty nick (sdQuotes stats))
             , Just (nick, quote) <- [parseRemember (eventText ev)]]
  -- quotes <- wrapTimed "activeHours-q4" $
  --           query ["SELECT"
  --                 ,"index.id,timestamp,REGEXP_REPLACE(text,'^@remember ([^ ]+)','')"
  --                 ,"FROM event, event_order_index index"
  --                 ,"WHERE TYPE in ('talk','act') AND"
  --                 ,"index.idx = ? AND index.origin=event.id AND"
  --                 ,"(? OR (timestamp > ? AND timestamp < ?))"
  --                 ,"AND text LIKE '@remember %'"
  --                 ,"AND REGEXP_REPLACE(text,'^@remember ([^ ]+).*',E'\\\\1') = ?"
  --                 ,"ORDER BY timestamp DESC"]
  --                 (idxNum LcHaskell,alltime,from,to,nick)

  let karma = mget 0 nick (sdKarma stats)
  -- karmap <- wrapTimed "activeHours-q5" $
  --           single ["select count(*) from event where text like ?"]
  --                  (Only (nick <> "++%"))
  -- karmam <- wrapTimed "activeHours-q6" $
  --           single ["select count(*) from event where text like ?"]
  --                  (Only (nick <> "--%"))

  return $
    NickStats { nickHours = hours
              , nickYears = years
              , nickLines = lines
              , nickQuote = listToMaybe rquote
              , nickQuotes = quotes
              , nickKarma = karma
              }
