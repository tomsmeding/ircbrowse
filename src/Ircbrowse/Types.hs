{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ircbrowse.Types where

import Data.IRC.EventID
import {-# SOURCE #-} Data.IRC.Provider (Provider)
import Ircbrowse.Data
import Ircbrowse.Types.Import
import Ircbrowse.Monads
import Ircbrowse.PerfStats (PerfStatsCtx)

import Data.Text
import Database.PostgreSQL.Simple (ConnectInfo)
import Database.PostgreSQL.Simple.FromRow
import Network.Mail.Mime (Address)
import Snap.App.Cache
import Snap.App.Types

-- | Site-wide configuration.
data Config = Config
  { configPostgres        :: !ConnectInfo
  , configDomain          :: !String
  , configAdmin           :: !Address
  , configSiteAddy        :: !Address
  , configCacheDir        :: !FilePath
  , configLogDirs         :: ![(String, FilePath)]
  }

configLogDirFor :: Config -> Network -> FilePath
configLogDirFor cfg netw =
    case lookup (showNetwork netw) (configLogDirs cfg) of
      Just fp -> fp
      Nothing -> error ("No irc log directory specified in config for network " ++ showNetwork netw)

instance AppConfig Config where
  getConfigDomain = configDomain

instance CacheDir Config where
  getCacheDir = configCacheDir

data PState = PState
  { statePerfCtx :: PerfStatsCtx
  , stateProvider :: Provider }

-- | Statistics.
data Stats = Stats
  { stEventCount         :: !Integer
  , stMsgCount           :: !Integer
  , stNickCount          :: !Integer
  , stActiveTimes        :: ![(Integer,Integer)]
  , stDailyActivity      :: ![(Integer,Integer)]
  , stActiveNicks        :: ![(String,Integer)]
  , stActivityByYear     :: ![(Integer,Integer)]
  , stConversationByYear :: ![(Integer,Integer)]
  } deriving Show

instance Default Stats where
  def = Stats
    { stEventCount = 0
    , stMsgCount   = 0
    , stNickCount = 0
    , stActiveNicks = []
    , stActiveTimes = []
    , stDailyActivity = []
    , stConversationByYear = []
    , stActivityByYear = []
    }

instance AppLiftModel Config PState where
  liftModel action = do
    conn <- env controllerStateConn
    anns <- env controllerState
    conf <- env controllerStateConfig
    let st = ModelState conn anns conf
    io $ runReaderT (runModel action) st

data Range = Range
  { rangeFrom :: !Day, rangeTo :: !Day }
  deriving (Eq,Show)

data CacheKey
  = StatsOverview Channel
  | Overview
  | NickCloud Channel
  | Social (Maybe Channel)
  | BrowseDay Channel Day Text
  | BrowseToday Channel Text
  | Profile Text Bool
  | AllNicks Channel Text
  | UniquePDFs Channel
  | Calendar Channel
  | Channel Channel

instance Key CacheKey where
  keyToString (Calendar channel) = norm $ "calendar-" ++ showChan channel ++ ".html"
  keyToString (BrowseToday channel mode) = norm $ "browse-today-" ++ showChan channel ++ "-" ++ unpack mode ++ ".html"
  keyToString (BrowseDay channel day mode) = norm $ "browse-day-" ++ showDay day ++ "-" ++ showChan channel ++ "-" ++ unpack mode ++ ".html"
  keyToString (UniquePDFs channel) = norm $ "unique-pdfs-" ++ showChan channel ++ ".html"
  keyToString (StatsOverview channel) = norm $ contexted "overview" (Just channel)
  keyToString Overview = norm $ "overview.html"
  keyToString (NickCloud channel) = norm $ contexted "nick-cloud" (Just channel)
  keyToString (Social channel) = norm $ contexted "social" channel
  keyToString (Profile nick recent) = norm $
   "profile-" ++ unpack nick ++ "-" ++ (if recent then "recent" else "all") ++
   ".html"
  keyToString (AllNicks channel mode) = norm $
    "nicks-" ++ showChan channel ++ "-" ++ unpack mode ++
    ".html"
  keyToString (Channel channel) = norm $ "channel-" ++ showChan channel ++ ".html"

norm :: [Char] -> [Char]
norm = go where
  go (x:xs) | isDigit x || isLetter x || x == '.' || x == '-'  = x : go xs
            | otherwise = show (fromEnum x) ++ go xs
  go [] = []

contexted :: [Char] -> Maybe Channel -> [Char]
contexted name channel =
  name ++ "-" ++ opt (fmap showChan channel) ++ ".html"
    where opt Nothing = "_"
          opt (Just x) = x

showDay :: Day -> String
showDay = formatTime defaultTimeLocale "%Y-%m-%d"

data Event = Event
  { eventId        :: !EventId
  , eventTimestamp :: !ZonedTime
  , eventNetwork   :: !Int
  , eventChannel   :: !Int
  , eventType      :: !Text
  , eventNick      :: !(Maybe Text)
  , eventText      :: !Text
  } -- deriving (Show)

-- instance FromRow Event where
--   fromRow = Event <$> field <*> field <*> field <*> field <*> field <*> field <*> field
