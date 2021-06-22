module Ircbrowse.Event where

import Data.IRC.EventId
import Ircbrowse.Types.Import

import Data.Text (Text)
import Data.Time

data Event = Event
  { eventId        :: !EventId
  , eventTimestamp :: !ZonedTime
  , eventChannel   :: !Channel
  , eventType      :: !Text
  , eventNick      :: !(Maybe Text)
  , eventText      :: !Text
  } -- deriving (Show)

-- instance FromRow Event where
--   fromRow = Event <$> field <*> field <*> field <*> field <*> field <*> field <*> field
