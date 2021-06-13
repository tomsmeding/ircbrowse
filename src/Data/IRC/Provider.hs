{-# LANGUAGE ViewPatterns #-}
module Data.IRC.Provider (
    module Data.IRC.EventId,
    Provider,
    makeProvider,
    eventsOnDay,
    eventsWithIds,
) where

import qualified Data.IRC.Event as Clog
import Data.IRC.EventId
import qualified Data.IRC.Znc.Parse as P
import Ircbrowse.Types
import Ircbrowse.Types.Import

import Control.Monad (forM)
import Data.Char (toLower)
import Data.List (find, sortBy, groupBy)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, isNothing, maybeToList)
import Data.Ord (comparing)
import Data.Function (on)
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Time.LocalTime.TimeZone.Olson as Zone
import qualified Data.Time.LocalTime.TimeZone.Series as Zone
import System.Directory (doesFileExist, listDirectory)
import qualified System.Environment as Env
import System.FilePath ((</>), takeFileName, splitExtension)


data ProviderInfo = ProviderInfo
    { priTZSeries :: Zone.TimeZoneSeries
    -- , priLogToUTC :: Time.LocalTime -> Time.UTCTime
    -- , priUtcToLog :: Time.UTCTime -> Time.LocalTime
    , priConfig :: Config
    }

data Provider = Provider
    { prInfo :: ProviderInfo
    , prOrderIndex :: Map Channel [(EventId, Int)]  -- (eventid, rank) where rank is one-based
    }

makeProvider :: Config -> IO Provider
makeProvider config = do
    let P.Config {P.timeZone = timeZone, P.zoneInfo = zoneInfo} = P.ircbrowseConfig
    tzdir <- fromMaybe zoneInfo <$> Env.lookupEnv "TZDIR"
    series <- Zone.getTimeZoneSeriesFromOlsonFile (tzdir </> timeZone)
    let prc = ProviderInfo
                  { priTZSeries = series
                  -- , priLogToUTC = Zone.localTimeToUTC' series
                  -- , priUtcToLog = Zone.utcToLocalTime' series
                  , priConfig = config }
    orderIndex <- initScanLogs prc
    return $ Provider
        { prInfo = prc
        , prOrderIndex = orderIndex }

initScanLogs :: ProviderInfo -> IO (Map Channel [(EventId, Int)])
initScanLogs info = initScanLogs' (configLogDirs (priConfig info))

initScanLogs' :: [(String, FilePath)] -> IO (Map Channel [(EventId, Int)])
initScanLogs' logdirs = do
    fmap (Map.fromList . concat) . forM logdirs $ \(networkName, networkLogdir) -> do
        subdirs <- listDirectory networkLogdir
        fmap concat . forM subdirs $ \channelName -> do
            let chanPred chan = prettyChan chan == channelName && showNetwork (chanNetwork chan) == networkName
            forM (maybeToList (find chanPred [toEnum 0 ..])) $ \channel -> do
                fileNames <- listDirectory (networkLogdir </> channelName)
                let files = case mapM parseLogName fileNames of
                              Just files -> files
                              Nothing -> error ("Cannot parse log file name: " ++
                                                  show (find (isNothing . parseLogName) fileNames))
                print files
                return (channel, [])

parseLogName :: FilePath -> Maybe Time.Day
parseLogName path =
    case splitExtension (takeFileName path) of
      (name, "log") -> Time.parseTimeM False Time.defaultTimeLocale "%Y-%m-%d" name
      _ -> Nothing

logFileName :: Config -> Channel -> Time.Day -> FilePath
logFileName config chan day =
    configLogDirFor config (chanNetwork chan) </> prettyChan chan </> dayToYMD day ++ ".log"

dayToYMD :: Time.Day -> String
dayToYMD = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d"

-- Get all events on a given day in the given channel. The function argument
-- decides whether a particular event needs to be included in the result.
eventsOnDay :: Provider -> (Time.UTCTime -> Event -> Bool) -> Channel -> Time.Day -> IO [Event]
eventsOnDay (prInfo -> info) predicate channel day = do
    let filename = logFileName (priConfig info) channel day
    exists <- doesFileExist filename
    events <- if exists
                  then P.parseLog P.ircbrowseConfig filename
                  else return []

    let toGeneric (Clog.EventAt utc ev) = (utc, Clog.decompose ev)
        toGeneric (Clog.NoParse text) =
            error ("Cannot parse Znc log line: " ++ T.unpack text)

    let generics = map toGeneric events

    return [event
           | (linenum, (utc, Clog.GenericEvent typ mnick texts))
                <- zip [0..] generics
           , let event = Event
                   { eventId = packEventId channel (toDayCode day) linenum
                   , eventTimestamp =
                       Time.utcToZonedTime
                            (Zone.timeZoneFromSeries (priTZSeries info) utc)
                            utc
                   , eventNetwork = 1
                   , eventChannel = showChanInt channel
                   , eventType = T.pack (map toLower (show typ))
                   , eventNick = (\(Clog.Nick n) -> n) <$> mnick
                   , eventText = T.concat texts }
           , predicate utc event]

-- Look up the events in the given list. Invalid or nonexistent event ids will
-- produce Nothing in the result.
eventsWithIds :: Provider -> [EventId] -> IO [Maybe Event]
eventsWithIds provider ids = do
    let groups :: [((Channel, DayCode), [(Int, EventId)])]
        groups = map ((,) <$> fst . head <*> map snd)
                 . groupBy ((==) `on` fst)
                 . sortBy (comparing (\((channel, daycode), _) -> (fromEnum channel, daycode)))
                 $ [((channel, daycode), (index, eid))
                   | (index, eid) <- zip [0..] ids
                   , Just (channel, daycode, _) <- [unpackEventId eid]]

    results <- concat <$> sequence
        [do events <- eventsOnDay provider (\_ ev -> eventId ev `elem` map snd idsOnDay) channel (fromDayCode daycode)
            return [(index, find ((== eid) . eventId) events)
                   | (index, eid) <- idsOnDay]
        | ((channel, daycode), idsOnDay) <- groups]

    return (map snd (sortBy (comparing fst) results))
