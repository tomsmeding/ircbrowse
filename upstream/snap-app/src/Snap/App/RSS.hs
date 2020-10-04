-- | Output RSS feeds.

module Snap.App.RSS where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Snap.App
import           Text.Feed.Export
import           Text.Feed.Types
import           Text.RSS.Syntax

-- | Output the given XML element.
outputRSS :: Text -> Text -> [(UTCTime,Text,Text,Text)] -> Controller c s ()
outputRSS title link items =
    case textFeed (makeFeed title link items) of
        Just res -> writeLazyText res
        Nothing -> do modifyResponse (setResponseCode 500)
                      writeText (T.pack "Error building RSS feed")

-- | Make a simple RSS feed.
makeFeed :: Text -> Text -> [(UTCTime,Text,Text,Text)] -> Feed
makeFeed title link = RSSFeed . makeRSS where
  makeRSS qs = (nullRSS title link)
               { rssChannel = makeChannel qs }
  makeChannel qs = (nullChannel title link)
                   { rssItems = map makeItem qs }
  makeItem (time,itemtitle,desc,itemlink) =
    (nullItem itemtitle)
    { rssItemPubDate = return (T.pack (toPubDate time))
    , rssItemDescription = return desc
    , rssItemLink = return itemlink
    }
  toPubDate = formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S UT"
