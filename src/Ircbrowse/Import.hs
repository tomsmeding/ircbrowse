{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Importing from logs.

module Ircbrowse.Import where

import           Ircbrowse.Data
import           Ircbrowse.Model.Data
import           Ircbrowse.Model.Migrations
import           Ircbrowse.Monads
import           Ircbrowse.System
import           Ircbrowse.Types
import           Ircbrowse.Types.Import

import           Control.Exception (try,IOException,catchJust)
import           Data.IORef
import           Data.IRC.CLog.Parse hiding (Config,parseLog)
import           Data.IRC.Znc.Parse hiding (Config)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Snap.App
import           Snap.App.Cache
import           Snap.App.Migrate
import           System.IO.Error (isDoesNotExistError)

-- -- | Do a batch import of files in the current directory.
-- batchImport :: Config -> Channel -> Pool -> IO ()
-- batchImport config channel pool = do
--   files <- fmap (sort . filter (not . all (=='.'))) (getDirectoryContents ".")
--   hSetBuffering stdout NoBuffering
--   forM_ files $ \file -> do
--     putStr $ "Importing " ++ file ++ " ... "
--     runDB config () pool $ importFile channel config file
--     putStrLn $ "Done."
--     removeFile file

-- | Import most recent logs for all available channels.
importRecent :: Bool -> Config -> Pool -> Maybe String -> IO ()
importRecent quick config pool mThisChan = do
  channelsToImport <-
      case mThisChan of
        Just thisChan
          | [chan] <- filter ((== thisChan) . showChan) [toEnum 0 ..]
          -> return [chan]
          | otherwise
          -> do putStrLn ("Channel not found: " ++ thisChan)
                return []
        Nothing -> return $ filter (networkIsActive . chanNetwork) [toEnum 0 ..]
  forM_ channelsToImport $ \channel -> do
    let network = chanNetwork channel
    putStrLn $ "Importing channel: " ++ showChan channel
    v <- newIORef []
    runDB config () pool $ do
      row <- query ["select timestamp"
                   ,"from event"
                   ,"where channel = ?"
                   ,"order by id"
                   ,"desc limit 1"]
                   (Only (showChanInt channel))
      io $ writeIORef v row
    last <- readIORef v
    now <- getZonedTime
    let today = localDay (zonedTimeToLocalTime now)
    case listToMaybe last of
      Just (Only (lastdate::UTCTime)) -> do
        putStrLn $ "Last date: " ++ show lastdate
        putStrLn $ "Importing from day: " ++ show (utctDay lastdate)
        when (utctDay lastdate /= today) $
          putStrLn $ "And also day: " ++ show (addDays 1 (utctDay lastdate))
        importChannel lastdate config pool (utctDay lastdate) channel False
        when (utctDay lastdate /= today) $
          importChannel lastdate config pool (addDays 1 (utctDay lastdate)) channel False
        putStrLn $ "Imported channel " ++ showChan channel
      _ -> do
        logs <- sort <$>
                    catchJust (\e -> if isDoesNotExistError e then Just () else Nothing)
                              (getDirectoryContents (configLogDirFor config network </> prettyChan channel))
                              (const (return []))
        case find (isInfixOf "-") logs of
          Nothing -> putStrLn $ "Unable to get last import time, or find the first log of this channel: " ++ showChan channel
          Just fp ->
            case parseFileTime fp of
              Nothing -> error $ "Found log of this channel in the log dir, but couldn't parse date from the filename: " ++ fp
              Just lastdate -> do
                putStrLn $ "This channel has never been imported: " ++ showChan channel
                putStrLn $ "Going to import from: " ++ show (lastdate :: UTCTime)
                importChannel lastdate config pool (utctDay lastdate) channel True
                return ()
    putStrLn "Clearing caches ..."
    runDB PState config pool $ do
       resetCacheModel (BrowseToday channel "recent")
       resetCacheModel (BrowseToday channel "everything")

  unless quick $ do
    putStrLn "Running ANALYZE ..."
    runDB config () pool $ void $ exec ["ANALYZE event_order_index"] ()
    putStrLn "Running data regeneration ..."
    runDB config () pool $ generateData

parseFileTime :: ParseTime t => [Char] -> Maybe t
parseFileTime = parseTimeM False defaultTimeLocale "%Y-%m-%d.log"

-- | Import the channel into the database of the given date.
importChannel :: UTCTime -> Config -> Pool -> Day -> Channel -> Bool -> IO ()
importChannel last config pool day channel frst = do
  mtmp <- copyLog channel day
  case mtmp of
    Nothing -> putStrLn ("Nothing to import right now for " ++ prettyChanWithNetwork channel)
    Just tmp -> do let db = runDB config () pool
                   db $ migrate False versions
                   db $ importFile last channel config tmp frst
                   void (try (removeFile tmp) :: IO (Either IOException ()))

  where copyLog chan day = do
          let fp = prettyChan chan ++ "/" ++ unmakeDay day ++ ".log"
          tmp <- getTemporaryDirectory

          let tmpfile = tmp </> prettyChan chan ++ "_" ++ unmakeDay day ++ ".log"
              target = configLogDirFor config (chanNetwork chan) ++ fp
          putStrLn $ "Importing from file " ++ target
          exists <- doesFileExist target
          if exists
             then do copyFile target
                              tmpfile
                     return (Just tmpfile)
             else return Nothing

unmakeDay :: FormatTime t => t -> String
unmakeDay = formatTime defaultTimeLocale "%Y-%m-%d"

-- | Update the event order index for the given channel.
updateChannelIndex :: Config -> Channel -> Model c s ()
updateChannelIndex _ channel = do
  io $ putStrLn $ "Updating order indexes for " ++ showChan channel ++ " ..."
  result <- query ["SELECT id,origin"
                  ,"FROM event_order_index"
                  ,"WHERE idx = ?"
                  ,"ORDER BY id DESC"
                  ,"LIMIT 1"]
                  (Only (idxNum channel))
  case result of
    -- [] -> error $ "Unable to get information for updating the channel index: " ++ showChan channel
    [] -> void $ exec ["INSERT INTO event_order_index"
                     ,"SELECT ? + RANK() OVER(ORDER BY id ASC) AS id,"
                     ,"id AS ORIGIN,"
                     ,"? AS idx"
                     ,"FROM event"
                     ,"WHERE channel = ?"
                     ,"AND id > ?"
                     ,"ORDER BY id ASC;"]
                     (0::Int
                     ,idxNum channel
                     ,showChanInt channel
                     ,0::Int)
    ((lastId::Int,lastOrigin::Int):_) -> void $
      exec ["INSERT INTO event_order_index"
           ,"SELECT ? + RANK() OVER(ORDER BY id ASC) AS id,"
           ,"id AS ORIGIN,"
           ,"? AS idx"
           ,"FROM event"
           ,"WHERE channel = ?"
           ,"AND id > ?"
           ,"ORDER BY id ASC;"]
           (lastId
           ,idxNum channel
           ,showChanInt channel
           ,lastOrigin)
  io $ putStrLn $  "Updating event count for " ++ showChan channel ++ " ..."
  void $ exec ["delete from event_count where channel = ?;"] (Only (showChanInt channel))
  void $ exec ["insert into event_count (count, channel) values ((select count(*) from event where channel = ?), ?);"]
             (showChanInt channel
             ,showChanInt channel)

-- | Import from the given file.
importFile :: UTCTime -> Channel -> Config -> FilePath -> Bool -> Model c s ()
importFile last channel config path frst = do
   events'' <- liftIO $ parseLog ircbrowseConfig path
   events' <- flip filterM events'' $ \case
       EventAt{} -> return True
       NoParse err -> do io $ putStrLn ("Failed to parse line: " ++ T.unpack err)
                         return False
   io $ putStrLn "Importing the following events:"
   let events | frst = events'
              | otherwise = dropWhile (not.(\case (EventAt t _) -> t > last
                                                  e -> error (show e))) events'
   io $ forM_ events $ \event ->
     print event
   {-let events_ = if null events
                    then [EventAt (let UTCTime d i = last
                                   in UTCTime (addDays 1 d) i) (Log "IRCBrowse was down during this period.")]
                    else events-}
   unless (null events)
     (do importEvents channel events
         updateChannelIndex config channel
         )

  -- This code is no longer applicable for ZNC. It was for tunes.org logs.
  --
  -- case reverse events of
  --   (EventAt _ (decompose -> GenericEvent typ _ (T.concat -> text)):_)
  --     | show typ == "Log" && T.isPrefixOf "ended" text -> importEvents channel events
  --   _ -> do liftIO $ removeFile path
  --                error $ "Log is incomplete (no 'ended' entry). (Removed file cache.)"

-- | Import an event.
importEvents :: Channel -> [EventAt] -> Model c s ()
importEvents channel events = do
   forM_ (nub events) $ \event ->
    case event of
      EventAt time (decompose -> GenericEvent typ mnick texts) -> do
        let text = T.concat texts
        exists <- fmap (==[Only True])
                       (query ["select true from event"
                              ,"where timestamp = ?"
                              ,"and network = ?"
                              ,"and channel = ?"
                              ,"and type = ?"
                              ,"and nick = ?"
                              ,"and text = ?"]
                              (time
                              ,1::Int
                              ,showChanInt channel
                              ,map toLower (show typ)
                              ,fmap unNick mnick
                              ,text))
        if (exists || T.null (T.strip text))
           then return False
           else do (void (exec ["INSERT INTO event (timestamp,network,channel,type,nick,text) VALUES"
                               ,"(?,?,?,?,?,?)"]
                               (time
                               ,1::Int
                               ,showChanInt channel
                               ,map toLower (show typ)
                               ,fmap unNick mnick
                               ,text)))
                   return True
      NoParse text -> do liftIO $ T.putStrLn $ mappend "Unable to import line: " text
                         return False
  where unNick (Nick t) = t
