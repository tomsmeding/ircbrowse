{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Data.IRC.Provider (
    module Data.IRC.EventId,
    Provider,
    makeProvider,
    eventAtOffset,
    eventsFromOffset,
    eventsOnDay,
    eventsWithIds,
    numChannelEvents,
) where

import qualified Data.IRC.Event as Clog
import Data.IRC.EventId
import qualified Data.IRC.Znc.Parse as P
import Ircbrowse.Types
import Ircbrowse.Types.Import

import Control.Arrow ((***))
import Control.DeepSeq (force)
import Control.Monad (forM)
import Data.Char (toLower)
import Data.List (find, sortBy, groupBy)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, fromMaybe, isNothing)
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

data ProviderStatsPer = ProviderStatsPer
    { spTotalEvents :: Integer
    }

data ProviderStats = ProviderStats
    { sPerChan :: Map Channel ProviderStatsPer
    }

data Provider = Provider
    { prInfo :: ProviderInfo
    , prOrderIndex :: Map Channel [(EventId, Integer)]  -- (eventid, offset) where offset is one-based
    , prStats :: ProviderStats
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
    scanResult <- initScanLogs prc
    return $ Provider
        { prInfo = prc
        , prOrderIndex = Map.map fst scanResult
        , prStats = ProviderStats { sPerChan = Map.map snd scanResult } }

initScanLogs :: ProviderInfo -> IO (Map Channel ([(EventId, Integer)], ProviderStatsPer))
initScanLogs info = do
    fmap Map.fromList . forM [toEnum 0 ..] $ \channel -> do
        putStrLn $ "Indexing logs for " ++ prettyChanWithNetwork channel ++ "..."

        files <- logFilesForChannel (priConfig info) channel
        -- Collect the first event id on each day (if any) together with the number of events on that day
        days <- sequence
            [do events <- readLogFile info fname (\_ _ -> True) channel day
                return (eventId <$> safeHead events, fromIntegral (length events) :: Integer)
            | (fname, day) <- sortBy (comparing snd) files]
        -- Compute the offsets of each of those first events
        let offsets = init (scanl (\off (_, num) -> off + num) 1 days)
        -- Zip those together with the event ids themselves to get the final index entries
        let index = catMaybes (zipWith (\(meid, _) off -> (,off) <$> meid) days offsets)

        let stats = ProviderStatsPer { spTotalEvents = sum (map snd days) }

        force index `seq` return (channel, (index, stats))

-- Returns full file path for each log file, together with the day it represents. List is sorted on the day.
logFilesForChannel :: Config -> Channel -> IO [(FilePath, Time.Day)]
logFilesForChannel config channel = do
    let networkLogdir = configLogDirFor config (chanNetwork channel)
    fileNames <- listDirectory (networkLogdir </> prettyChan channel)
    return $ sortBy (comparing snd) $
        case mapM (\f -> (networkLogdir </> prettyChan channel </> f,) <$> parseLogName f) fileNames of
          Just files -> files
          Nothing -> error ("Cannot parse log file name: " ++
                              show (find (isNothing . parseLogName) fileNames))

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
eventAtOffset :: Provider -> Channel -> Integer -> IO (Maybe Event)
eventAtOffset provider channel offset =
    fmap (head . snd) <$> eventIdsOnDayFromOffset provider channel offset

-- If there are less than @count@ events in the log starting at the given
-- offset, a smaller list will be returned containing only the events that do
-- exist. If the offset is out of range, 'Nothing' is returned.
-- The first element in the log has index 1.
eventsFromOffset :: Provider -> Channel -> Integer -> Integer -> IO (Maybe [Event])
eventsFromOffset provider channel offset count =
    eventIdsOnDayFromOffset provider channel offset >>= \case
      Nothing -> return Nothing
      Just (_, remaining) -> do
        let num = fromIntegral (length remaining) :: Integer
        if num >= count
            then return (Just (take (fromIntegral count) remaining))
            else (Just . (remaining ++) . fromMaybe []) <$>
                     eventsFromOffset provider channel (offset + num) (count - num)

-- Finds the day containing the event with the given offset, if it exists.
-- Returns the corresponding day and the remaining list of events on that day,
-- starting at and including the one indicated.
-- The first element in the log has index 1.
eventIdsOnDayFromOffset :: Provider -> Channel -> Integer -> IO (Maybe (Time.Day, [Event]))
eventIdsOnDayFromOffset provider channel offset = do
    let offsetIndex = fromMaybe [] (Map.lookup channel (prOrderIndex provider))

    case (safeLast *** safeHead) $ break ((> offset) . snd) offsetIndex of
      (Just (eid1, off1), Just (eid2, _)) ->
          if eidDay eid2 `Time.diffDays` eidDay eid1 < 10
              then
                  -- We have a lower and upper bound on the day, and they're fairly close together.
                  -- Loop over the days and retrieve the corresponding logs from the file system.
                  findWithDays (eidDay eid1) (eidDay eid2) off1
              else do
                  -- The upper and lower bounds are very far apart; presumably there are few log files in between.
                  -- Get a directory listing and check the logs that fall in the interval.
                  files <- filter (\(_, day) -> eidDay eid1 <= day && day < eidDay eid2) <$>
                               logFilesForChannel (priConfig (prInfo provider)) channel
                  findWithList files off1

      (Just (eid1, off1), Nothing) -> do
          -- Only a lower bound.
          -- Check that day, and if the event isn't in there, get a directory listing and check all later days.
          events1 <- eventsOnDay provider (\_ _ -> True) channel (eidDay eid1)
          let num1 = length events1
          if offset - off1 < fromIntegral num1
              then return (Just (eidDay eid1, drop (fromIntegral (offset - off1)) events1))
              else do
                  files <- filter (\(_, day) -> eidDay eid1 < day) <$>
                               logFilesForChannel (priConfig (prInfo provider)) channel
                  findWithList files (off1 + fromIntegral num1)

      (Nothing, Just (_, 1)) ->
          -- Apparently we got offset < 1, which doesn't exist.
          return Nothing

      (Nothing, _) -> do
          -- The offset index is empty or we are before its first entry.
          -- Must get a directory listing and do a linear search.
          files <- logFilesForChannel (priConfig (prInfo provider)) channel
          findWithList files 1
  where
    eidDay :: EventId -> Time.Day
    eidDay eid = case unpackEventId eid of
                   Just (_, day, _) -> fromDayCode day
                   Nothing -> error "Invalid event id in offset index"

    findWithDays :: Time.Day -> Time.Day -> Integer -> IO (Maybe (Time.Day, [Event]))
    findWithDays day untilDay dayoffset
      | day >= untilDay = return Nothing
      | otherwise = do
          events <- eventsOnDay provider (\_ _ -> True) channel day
          let num = fromIntegral (length events) :: Integer
          if offset - dayoffset < num
              then return (Just (day, drop (fromIntegral (offset - dayoffset)) events))
              else findWithDays (succ day) untilDay (dayoffset + num)

    findWithList :: [(FilePath, Time.Day)] -> Integer -> IO (Maybe (Time.Day, [Event]))
    findWithList [] _ = return Nothing
    findWithList ((fpath, day) : files) dayoffset = do
        events <- readLogFile (prInfo provider) fpath (\_ _ -> True) channel day
        let num = fromIntegral (length events) :: Integer
        if offset - dayoffset < num
            then return (Just (day, drop (fromIntegral (offset - dayoffset)) events))
            else findWithList files (dayoffset + num)

-- Get all events on a given day in the given channel. The function argument
-- decides whether a particular event needs to be included in the result.
eventsOnDay :: Provider -> (Time.UTCTime -> Event -> Bool) -> Channel -> Time.Day -> IO [Event]
eventsOnDay (prInfo -> info) predicate channel day = do
    let filename = logFileName (priConfig info) channel day
    exists <- doesFileExist filename
    if exists
        then readLogFile info filename predicate channel day
        else return []

numChannelEvents :: Provider -> Channel -> IO Integer
numChannelEvents provider channel =
    return (maybe 0 spTotalEvents (Map.lookup channel (sPerChan (prStats provider))))

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

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_:l) = safeLast l
