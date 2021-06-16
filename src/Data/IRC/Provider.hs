{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Data.IRC.Provider (
    module Data.IRC.EventId,
    Provider,
    makeProvider,
    eventIdAtOffset,
    eventsOnDay,
    eventsWithIds,
) where

import qualified Data.IRC.Event as Clog
import Data.IRC.EventId
import qualified Data.IRC.Znc.Parse as P
import Ircbrowse.Types
import Ircbrowse.Types.Import

import Control.DeepSeq (force)
import Control.Monad (forM)
import Data.Char (toLower)
import Data.List (find, sortBy, groupBy)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, fromMaybe, isNothing, maybeToList)
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
    , prOrderIndex :: Map Channel [(EventId, Int)]  -- (eventid, offset) where offset is one-based
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
initScanLogs info = do
    -- For all networks...
    fmap (Map.fromList . concat) . forM (configLogDirs (priConfig info)) $ \(networkName, networkLogdir) -> do
        -- For each channel on that network...
        subdirs <- listDirectory networkLogdir
        fmap concat . forM subdirs $ \channelName -> do
            let chanPred chan = prettyChan chan == channelName && showNetwork (chanNetwork chan) == networkName
            forM (maybeToList (find chanPred [toEnum 0 ..])) $ \channel -> do
                putStrLn $ "Indexing logs for " ++ prettyChanWithNetwork channel ++ "..."
                -- Get the list of log file names
                fileNames <- listDirectory (networkLogdir </> channelName)
                -- Build the list of full file paths with the day they represent
                let files :: [(FilePath, Time.Day)]
                    files = sortBy (comparing snd) $
                                case mapM (\f -> (networkLogdir </> channelName </> f,) <$> parseLogName f) fileNames of
                                  Just files -> files
                                  Nothing -> error ("Cannot parse log file name: " ++
                                                      show (find (isNothing . parseLogName) fileNames))
                -- Collect the first event id on each day (if any) together with the number of events on that day
                days <- sequence
                    [do events <- readLogFile info fname (\_ _ -> True) channel day
                        return (eventId <$> safeHead events, length events)
                    | (fname, day) <- sortBy (comparing snd) files]
                -- Compute the offsets of each of those first events
                let offsets = init (scanl (\off (_, num) -> off + num) 1 days)
                -- Zip those together with the event ids themselves to get the final index entries
                let index = catMaybes (zipWith (\(meid, _) off -> (,off) <$> meid) days offsets)
                force index `seq` return (channel, index)

parseLogName :: FilePath -> Maybe Time.Day
parseLogName path =
    case splitExtension (takeFileName path) of
      (name, ".log") -> Time.parseTimeM False Time.defaultTimeLocale "%Y-%m-%d" name
      _ -> Nothing

logFileName :: Config -> Channel -> Time.Day -> FilePath
logFileName config chan day =
    configLogDirFor config (chanNetwork chan) </> prettyChan chan </> dayToYMD day ++ ".log"

dayToYMD :: Time.Day -> String
dayToYMD = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d"

-- The first element in the log has index 1.
eventIdAtOffset :: Provider -> Channel -> Integer -> IO (Maybe EventId)
eventIdAtOffset provider channel offset = ()

-- Get all events on a given day in the given channel. The function argument
-- decides whether a particular event needs to be included in the result.
eventsOnDay :: Provider -> (Time.UTCTime -> Event -> Bool) -> Channel -> Time.Day -> IO [Event]
eventsOnDay (prInfo -> info) predicate channel day = do
    let filename = logFileName (priConfig info) channel day
    exists <- doesFileExist filename
    if exists
        then readLogFile info filename predicate channel day
        else return []

-- Assumes the file exists and contains events for the given channel on the given day.
readLogFile :: ProviderInfo -> FilePath -> (Time.UTCTime -> Event -> Bool) -> Channel -> Time.Day -> IO [Event]
readLogFile info filename predicate channel day = do
    events <- P.parseLog P.ircbrowseConfig filename

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

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x
