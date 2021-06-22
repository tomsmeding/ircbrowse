module Ircbrowse.Model.Events where

import Data.IRC.Provider
-- import Database.PostgreSQL.Simple.FromRow
import Ircbrowse.Data
import Ircbrowse.Event
import Ircbrowse.Monads
import Ircbrowse.Types
import Ircbrowse.Types.Import

import Data.Text (Text)
import Snap.App
-- import Sphinx
import Text.Blaze.Pagination

getEvents :: Channel -> Maybe Integer -> PN -> Maybe Text
          -> Model c PState (Pagination,[Event])
getEvents channel tid (PN _ pagination _) _q = do
  case Nothing {-q-} of
    -- Just q -> do
    --   result <- io $ search def
    --     { sPath = "/opt/sphinx/bin/search"
    --     , sConfig = "sphinx.conf"
    --     , sQuery = escapeText q
    --     , sOffset = fromIntegral ((pnCurrentPage pagination - 1) * pnPerPage pagination)
    --     , sLimit = fromIntegral (pnPerPage pagination)
    --     , sFilters = [("channel",showChanInt channel)]
    --     }
    --   case result of
    --     Left{} -> return (pagination { pnTotal = 0 },[])
    --     Right result -> do
    --       results <- getEventsByIds channel (map fst (rResults result))
    --       return (pagination { pnTotal = fromIntegral (rTotal result) }
    --              ,results)
    Nothing -> do
      case tid of
        Nothing -> getPaginatedEvents channel pagination
        Just t -> getTimestampedEvents channel t pagination

getFirstEventDate :: Channel -> Model c s Day
getFirstEventDate channel = do
  today <- fmap utctDay (io getCurrentTime)
  fmap (maybe today utctDay)
       (single ["SELECT timestamp"
               ,"FROM event"
               ,"WHERE channel = ?"
               ,"ORDER BY timestamp"
               ,"ASC LIMIT 1"]
               (Only (showChanInt channel)))

getEventsByOrderIds :: [EventId] -> Model c PState [Event]
getEventsByOrderIds eids = do
  provider <- reader (stateProvider . modelStateAnns)
  liftIO $ catMaybes <$> eventsWithIds provider (sort eids)
  -- query ["SELECT id,timestamp,network,channel,type,nick,text"
  --       ,"FROM event"
  --       ,"WHERE id in (SELECT origin FROM event_order_index WHERE idx = ? AND id IN ("
  --       ,intercalate ", " (map show eids)
  --       ,")) ORDER BY id ASC"]
  --       (Only (idxNum channel))

-- getEventsByIds :: Channel -> [Int] -> Model c s [Event]
-- getEventsByIds channel eids = do
--   query ["SELECT (SELECT id FROM event_order_index WHERE origin = event.id AND idx = ? limit 1),"
--         ,"timestamp,network,channel,type,nick,text"
--         ,"FROM event"
--         ,"WHERE id IN ("
--         ,intercalate ", " (map show eids)
--         ,") ORDER BY id DESC"]
--         (Only (idxNum channel))

getEventsByDay :: Channel -> Day -> Bool -> Model c PState [Event]
getEventsByDay channel day everything =
  if everything
     then getAllEventsByDay channel day
     else getRecentEventsByDay channel day

getRecentEventsByDay :: Channel -> Day -> Model c PState [Event]
getRecentEventsByDay channel day = do
  provider <- reader (stateProvider . modelStateAnns)
  events <- liftIO $ eventsOnDay provider (\_ _ -> True) channel day
  return (reverse (take 50 (reverse events)))
  -- count <- single ["SELECT count FROM event_count where channel = ?"] (Only (showChanInt channel))
  -- let offset = fromMaybe 0 count - limit
  -- query ["SELECT idx.id,e.timestamp,e.network,e.channel,e.type,e.nick,e.text FROM event e,"
  --        ,"event_order_index idx"
  --        ,"WHERE e.id = idx.origin and idx.idx = ? and idx.id >= ?"
  --        ,"ORDER BY e.id DESC"
  --        ,"LIMIT ?"]
  --        (idxNum channel
  --        ,offset
  --        ,limit)
  -- where limit = 50 :: Int

getAllEventsByDay :: Channel -> Day -> Model c PState [Event]
getAllEventsByDay channel day = do
  provider <- reader (stateProvider . modelStateAnns)
  liftIO $ eventsOnDay provider (\_ _ -> True) channel day
  -- query ["SELECT (SELECT id FROM event_order_index WHERE origin = event.id AND idx = ? limit 1) as id,"
  --       ,"timestamp,network,channel,type,nick,text"
  --       ,"FROM event"
  --       ,"WHERE channel = ?"
  --       ,"AND timestamp >= ?"
  --       ,"AND timestamp < (?::timestamp) + interval '1 day'"
  --       ,"ORDER BY id ASC"
  --       ]
  --      (idxNum channel
  --      ,showChanInt channel
  --      ,day
  --      ,day)

getTimestampedEvents :: Channel
                     -> Integer
                     -> Pagination
                     -> Model c PState (Pagination,[Event])
getTimestampedEvents channel tid pagination = do
  getPaginatedEvents channel pagination
    { pnCurrentPage = tid `ceilDiv` pnPerPage pagination }
  where ceilDiv n d = (n + d - 1) `div` d

getPaginatedEvents :: Channel -> Pagination -> Model c PState (Pagination,[Event])
getPaginatedEvents channel pagination = do
  let offset = 1 + max 0 (pnCurrentPage pagination - 1) * pnPerPage pagination
      limit = pnPerPage pagination
  provider <- reader (stateProvider . modelStateAnns)
  count <- liftIO $ numChannelEvents provider channel
  events <- liftIO $ fromMaybe [] <$> eventsFromOffset provider channel offset limit
  -- count <- single ["SELECT count FROM event_count where channel = ?"] (Only (showChanInt channel))
  -- events <- query ["SELECT idx.id,e.timestamp,e.network,e.channel,e.type,e.nick,e.text FROM event e,"
  --                 ,"event_order_index idx"
  --                 ,"WHERE e.id = idx.origin and idx.idx = ? and idx.id >= ? and idx.id < ?"
  --                 ,"ORDER BY e.id asc"
  --                 ,"LIMIT ?"]
  --                 (idxNum channel
  --                 ,offset
  --                 ,offset + limit
  --                 ,limit)
  return (pagination { pnTotal = count }, events)

getPaginatedPdfs :: Channel -> PN -> Model c PState (Pagination,[Event])
getPaginatedPdfs _channel (PN _ pagination _) = do
  TODO: Support PDFs
  liftIO $ putStrLn "ERROR: PDFS NOT SUPPORTED YET"
  return (pagination { pnTotal = 0 }, [])
  -- count <- single ["SELECT COUNT(*) FROM event_order_index WHERE idx = ?"]
  --                 (Only (idxNum channel + 1))
  -- events <- query ["SELECT idx.id,e.timestamp,e.network,e.channel,e.type,e.nick,e.text FROM event e,"
  --                 ,"event_order_index idx"
  --                 ,"WHERE e.id = idx.origin and idx.idx = ? and idx.id >= ? and idx.id < ?"
  --                 ,"ORDER BY idx.id asc"
  --                 ,"LIMIT ?"]
  --                 (idxNum channel + 1
  --                 ,offset
  --                 ,offset + limit
  --                 ,limit)
  -- return (pagination { pnTotal = fromMaybe 0 count }
  --        ,events)

  -- where offset = 1 + (max 0 (pnCurrentPage pagination - 1) * pnPerPage pagination)
  --       limit = pnPerPage pagination

getAllPdfs :: Channel -> Model c s [(Int,ZonedTime,Text)]
getAllPdfs channel = do
  events <- query ["SELECT idx2.id,e.timestamp,e.text FROM event e,"
                  ,"event_order_index idx,"
                  ,"event_order_index idx2"
                  ,"WHERE e.id = idx.origin AND idx.idx = ?"
                  ,"AND e.id = idx2.origin AND idx2.idx = ?"
                  ,"ORDER BY idx.id asc"]
                  (idxNum channel + 1, idxNum channel)
  return events
