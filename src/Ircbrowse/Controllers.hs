{-# LANGUAGE OverloadedStrings #-}

module Ircbrowse.Controllers where

import           Ircbrowse.Data
import           Ircbrowse.Model.Events
import           Ircbrowse.Model.Nicks
import           Ircbrowse.Model.Profile
import           Ircbrowse.Model.Quotes
-- import           Ircbrowse.Model.Social
import           Ircbrowse.Model.Stats
import           Ircbrowse.Monads
import           Ircbrowse.PerfStats (currentPerfStats)
import           Ircbrowse.Types
import           Ircbrowse.Types.Import
import           Ircbrowse.View.Browse as V
import           Ircbrowse.View.Calendar as V
import           Ircbrowse.View.NickCloud as V
import           Ircbrowse.View.Nicks as V
import           Ircbrowse.View.Overview as V
import           Ircbrowse.View.PerfStats as V
import           Ircbrowse.View.Profile as V
-- import           Ircbrowse.View.Social as V

import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Safe
import           Snap.App
import           Snap.App.Cache
import           Snap.App.RSS
import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Pagination

--------------------------------------------------------------------------------
-- Controllers

-- export :: Controller Config PState ()
-- export = do
--   return ()

overview :: Controller Config PState ()
overview = do
  viewCached Overview $ do
    return $ V.overview

stats :: Controller Config PState ()
stats = do
  range <- getRange
  channel <- getChannel
  viewCached (StatsOverview channel) $ do
    stats <- model $ getStats channel range
    return $ V.statsOverview channel range stats

-- channel :: Controller Config PState ()
-- channel = do
--   channel <- getChannel
--   viewCached (Channel channel) $ do
--     return $ V.channel channel

calendar :: Controller Config PState ()
calendar = do
  channel <- getChannel
  viewCached (Calendar channel) $ do
    day <- model $ getFirstEventDate channel
    today <- fmap utctDay (io getCurrentTime)
    return $ V.calendar day today channel

nickProfile :: Controller Config PState ()
nickProfile = do
  nick <- getText "nick"
  recent <- getBoolean "recent" True
  range <- getRange
  viewCached (Profile nick recent) $ do
    hours <- model $ activeHours nick recent range
    return $ V.nickProfile nick recent hours

allNicks :: Controller Config PState ()
allNicks = do
  mode <- fmap (maybe "recent" (\x -> if x == "all" then "all" else "recent")) (getTextMaybe "mode")
  range <- getRange
  channel <- getChannel
  viewCached (AllNicks channel mode) $ do
    nicks <- model $ getNicks channel 100 (mode == "recent") range
    count <- model $ getNickCount channel (mode == "recent") range
    return $ V.nicks channel count nicks mode

-- socialGraph :: Controller Config PState ()
-- socialGraph = do
--   range <- getRange
--   channel <- getChannelMaybe
--   viewCached (Social channel) $ do
--     graph <- model $ getSocialGraph channel range
--     return $ V.socialGraph graph

nickCloud :: Controller Config PState ()
nickCloud = do
  range <- getRange
  channel <- getChannel
  viewCached (NickCloud channel) $ do
    nicks <- model $ getNickStats channel range
    return $ V.nickCloud nicks

browseDay :: Bool -> Controller Config PState ()
browseDay today =
  if today
    then do today <- io (fmap utctDay getCurrentTime)
            browseSomeDay True today (T.pack (formatTime defaultTimeLocale "%Y/%m/%d" today))
    else browseGivenDate

browseGivenDate :: Controller Config PState ()
browseGivenDate = do
  year <- getText "year"
  month <- getText "month"
  day <- getText "day"
  let datetext = year <> "/" <> month <> "/" <> day
  case parseTimeM True defaultTimeLocale "%Y/%m/%d" (T.unpack datetext) of
    Nothing -> return ()
    Just day -> browseSomeDay False day datetext

browseSpecified :: Controller Config PState ()
browseSpecified =
  do channel <- getChannel
     events <- getText "events"
     title <- getText "title"
     let eids = mapMaybe (readMay . T.unpack) (T.split (==',') events)
     events <- model (getEventsByOrderIds channel eids)
     uri <- getMyURI
     outputText (renderHtml (V.selection channel title events uri))

browseSomeDay :: Bool -> Day -> Text -> Controller Config PState ()
browseSomeDay today day datetext = do
  channel <- getChannel
  mode <- fmap (fromMaybe "everything") (getTextMaybe "mode")
  out <- cache (if today then BrowseToday channel mode else BrowseDay channel day mode) $ do
    events <- model $ getEventsByDay channel day (mode == "everything")
    uri <- getMyURI
    return $ Just $ V.browseDay channel mode datetext events uri
  maybe (return ()) outputText out

browse :: Controller Config PState ()
browse = do
  evid <- getIntegerMaybe "id"
  channel <- getChannel
  q <- getSearchText "q"
  pn <- getPagination "events"
  let pn' = pn { pnResultsPerPage = Just [25,35,50,100] }
  (pagination,logs) <- model $ getEvents channel evid pn' q
  uri <- getMyURI
  outputText $ renderHtml $ V.browse channel uri logs pn' { pnPn = pagination } q

pdfs :: Controller Config PState ()
pdfs = do
  unique <- fmap isJust (getTextMaybe "unique")
  if unique
     then uniquePdfs
     else paginatedPdfs

uniquePdfs :: Controller Config PState ()
uniquePdfs = do
  channel <- getChannel
  uri <- getMyURI
  viewCached (UniquePDFs channel) $ do
    pdfs <- model $ getAllPdfs channel
    return $ V.allPdfs uri channel pdfs

paginatedPdfs :: Controller Config PState ()
paginatedPdfs = do
  channel <- getChannel
  pn <- getPagination "events"
  let pn' = pn { pnResultsPerPage = Just [25,35,50,100] }
  (pagination,logs) <- model $ getPaginatedPdfs channel pn'
  uri <- getMyURI
  outputText $ renderHtml $ V.pdfs channel uri logs pn' { pnPn = pagination } Nothing

quotes :: Controller Config PState ()
quotes = do
  return ()
  qs <- model $ getRecentQuotes 30
  outputRSS "IRCBrowse Quotes"
            "https://ircbrowse.tomsmeding.com/quotes.rss"
            (map (\(eid,date,title) -> (date,title,"",T.pack (makeLink eid date)))
                 qs)

  where makeLink eid t =
          concat ["https://ircbrowse.tomsmeding.com/browse/haskell?id="
                 ,show eid
                 ,"&timestamp="
                 ,secs
                 ,"#t"
                 ,secs]
         where secs = formatTime defaultTimeLocale "%s" t

perfStats :: Controller Config PState ()
perfStats = do
  perfCtx <- reader (statePerfCtx . controllerState)
  current <- currentPerfStats perfCtx
  outputText $ renderHtml $ V.perfStats current

--------------------------------------------------------------------------------
-- Utilities

getRange :: Controller c s Range
getRange = do
  now <- io getCurrentTime
  let range = Range (addDays (-31) (utctDay now)) (utctDay now)
  return range

getChannel :: Controller c s Channel
getChannel = do
    chan <- getChannelMaybe
    case chan of
      Just c -> return c
      Nothing -> do
          modifyResponse $ setResponseStatus 404 "Not Found"
          writeBS "Channel not found"
          getResponse >>= finishWith

getChannelMaybe :: Controller c s (Maybe Channel)
getChannelMaybe = do
  chan <- getStringMaybe "channel"
  return $ chan >>= parseChan

getTimestamp :: Controller c s (Maybe UTCTime)
getTimestamp = do
  string <- getStringMaybe "timestamp"
  return $ string >>= parseTimeM True defaultTimeLocale "%s"

getSearchText :: ByteString -> Controller c s (Maybe Text)
getSearchText key = do
  v <- getTextMaybe key
  case fmap (T.filter (not.isSpace)) v of
    Nothing -> return Nothing
    Just e | T.null e -> return Nothing
           | otherwise -> return v

-- | Get text (maybe).
getTextMaybe :: ByteString -> Controller c s (Maybe Text)
getTextMaybe name = do
  pid <- fmap (fmap T.decodeUtf8) (getParam name)
  return pid

-- | Get text (maybe).
getText :: ByteString -> Controller c s Text
getText name = do
  getTextMaybe name >>= maybe (error ("expected param: " ++ show name)) return

-- | Get integer parmater.
getIntegerMaybe :: ByteString -> Controller c s (Maybe Integer)
getIntegerMaybe name = do
  pid <- fmap (>>= readMay) (getStringMaybe name)
  return pid

-- | Get a boolean value.
getBoolean :: ByteString -> Bool -> Controller c s Bool
getBoolean key def = fmap (maybe def (=="true")) (getTextMaybe key)
