module Data.IRC.Provider where

import qualified Data.IRC.Event as Clog
import Data.IRC.EventID
import qualified Data.IRC.Znc.Parse as P
import Ircbrowse.Types
import Ircbrowse.Types.Import
import qualified Ircbrowse.Import as Import

import Data.Char (toLower)
import Data.List (find, sortBy, groupBy)
import Data.Ord (comparing)
import Data.Function (on)
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Time.LocalTime.TimeZone.Olson as Zone
import qualified Data.Time.LocalTime.TimeZone.Series as Zone
import System.Directory (doesFileExist)


data Provider = Provider
    { prTZSeries :: Zone.TimeZoneSeries
    , prLogToUTC :: Time.LocalTime -> Time.UTCTime
    , prUtcToLog :: Time.UTCTime -> Time.LocalTime
    , prConfig :: Config
    }

makeProvider :: Config -> IO Provider
makeProvider config = do
    series <- Zone.getTimeZoneSeriesFromOlsonFile (P.zoneInfo P.ircbrowseConfig)
    return $ Provider
        { prTZSeries = series
        , prLogToUTC = Zone.localTimeToUTC' series
        , prUtcToLog = Zone.utcToLocalTime' series
        , prConfig = config }

-- Get all events on a given day in the given channel. The function argument
-- decides whether a particular event needs to be includede in the result.
eventsOnDay :: Provider -> (Time.UTCTime -> Event -> Bool) -> Channel -> Time.Day -> IO [Event]
eventsOnDay provider predicate channel day = do
    let filename = Import.logFileName (prConfig provider) channel day
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
                            (Zone.timeZoneFromSeries (prTZSeries provider) utc)
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
