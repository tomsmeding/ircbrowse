-- | Model for @remember'd quotes. Does not take into account @forget.

module Ircbrowse.Model.Quotes where

import Data.IRC.Provider
import Ircbrowse.Event
import Ircbrowse.Model.Data
import Ircbrowse.Types
import Ircbrowse.Types.Import

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Text (Text)
import Data.Time
import Snap.App

-- | Get the most recent n quotes.
getRecentQuotes :: Int -> Model c PState [(EventId,UTCTime,Text)]
getRecentQuotes n = do
  provider <- reader (stateProvider . modelStateAnns)
  stats <- liftIO $ providerStats provider
  let evs = sdpRemembers (mget statDataPerEmpty LcHaskell (sdPerChan stats))
  return [(eventId ev, zonedTimeToUTC (eventTimestamp ev), eventText ev)
         | ev <- reverse (take n evs)]
  -- query ["SELECT index.id,timestamp,REGEXP_REPLACE(text,'^@remember ([^ ]+)',E'<\\\\1>')"
  --       ,"FROM event, event_order_index index"
  --       ,"WHERE"
  --       ,"channel = ? AND"
  --       ,"index.idx = ? AND index.origin=event.id AND"
  --       ,"TYPE IN ('talk','act') AND"
  --       ,"text LIKE '@remember %'"
  --       ,"ORDER BY timestamp DESC"
  --       ,"LIMIT ?"]
  --       (showChanInt LcHaskell
  --       ,idxNum LcHaskell
  --       ,n)
