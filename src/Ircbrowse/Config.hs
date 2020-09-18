{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing #-}

-- | Load the configuration file.

module Ircbrowse.Config (getConfig) where

import Ircbrowse.Types

import Data.ConfigFile
import Database.PostgreSQL.Simple (ConnectInfo(..))
import qualified Data.Text as T
import Network.Mail.Mime

getConfig :: FilePath -> IO Config
getConfig conf = do
  contents <- readFile conf
  let config = do
        c <- readstring emptyCP contents
        pghost <- get c "POSTGRESQL" "host"
        pgport <- get c "POSTGRESQL" "port"
        pguser <- get c "POSTGRESQL" "user"
        pgpass <- get c "POSTGRESQL" "pass"
        pgdb   <- get c "POSTGRESQL" "db"
        domain <- get c "WEB" "domain"
        cache  <- get c "WEB" "cache"
        admin    <- get c "ADDRESSES" "admin"
        siteaddy <- get c "ADDRESSES" "site_addy"
        ldir <- get c "IMPORT" "log_dir"

        return Config {
           configPostgres = ConnectInfo pghost (read pgport) pguser pgpass pgdb
         , configDomain = domain
         , configAdmin = Address Nothing (T.pack admin)
         , configSiteAddy = Address Nothing (T.pack siteaddy)
         , configCacheDir = cache
         , configLogDir = ldir
         }
  case config of
    Left cperr -> error $ show cperr
    Right config -> return config
