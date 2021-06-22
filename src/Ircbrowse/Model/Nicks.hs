-- | Statistics for nicks in aggregate.

module Ircbrowse.Model.Nicks where

import Data.IRC.Provider
import Ircbrowse.Model.Data
import Ircbrowse.Model.Data.Recent
import Ircbrowse.Types
import Ircbrowse.Types.Import

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (reader)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Snap.App

getNicks :: Channel -> Int -> Bool -> Model c PState [(Text,Int)]
getNicks channel n recent = do
  let unRecent :: Monoid a => Recent a -> a
      unRecent | recent = recentRecent
               | otherwise = recentAlltime

  provider <- reader (stateProvider . modelStateAnns)
  stats <- liftIO $ providerStats provider

  let pairs = Map.assocs (Map.map (actNumMsg . unRecent) (mget mempty channel (sdChannelNickActivity stats)))
  return $ take n (sortBy (comparing snd) pairs)

  -- query ["SELECT nick,COUNT(*)"
  --       ,"FROM event"
  --       ,"WHERE channel = ? and TYPE in ('talk','act') AND"
  --       ,"(NOT ? OR (timestamp > ? AND timestamp < ?)) AND"
  --       ,"NOT (text LIKE '@%' OR text LIKE '> %' OR text LIKE ':t %' OR text LIKE ':k %'"
  --       ,"OR text LIKE 'lambdabot: %')"
  --       ,"GROUP BY nick"
  --       ,"ORDER BY 2 desc"
  --       ,"LIMIT ?;"]
  --       (showChanInt channel,recent,from,to,n)

getNickCount :: Channel -> Bool -> Model c PState Int
getNickCount channel recent = do
  let unRecent :: Monoid a => Recent a -> a
      unRecent | recent = recentRecent
               | otherwise = recentAlltime

  provider <- reader (stateProvider . modelStateAnns)
  stats <- liftIO $ providerStats provider

  return $ length . map fst . filter ((> 0) . snd) $
            Map.assocs (Map.map (actNumMsg . unRecent) (mget mempty channel (sdChannelNickActivity stats)))

  -- fmap (sum . map (\(Only x) -> x))
  --      (query ["SELECT DISTINCT COUNT(*)"
  --             ,"FROM (SELECT nick FROM event"
  --             ,"      WHERE channel= ? and (NOT ? OR (timestamp > ? AND timestamp < ?))"
  --             ,"      GROUP BY nick) c"]
  --             (showChanInt channel,recent,from,to))
