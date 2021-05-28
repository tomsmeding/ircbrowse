-- | Load the configuration file.

module Ircbrowse.Config (getConfig) where

import Ircbrowse.Types
import Ircbrowse.Types.Import (showNetwork)

import Control.Monad (when)
import Data.ConfigFile
import Data.List (intercalate, sort, (\\))
import Database.PostgreSQL.Simple (ConnectInfo(..))
import qualified Data.Text as T
import Network.Mail.Mime
import System.Exit (die)

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
        logdirs <- options' c "LOGDIRS"

        return Config {
           configPostgres = ConnectInfo pghost (read pgport) pguser pgpass pgdb
         , configDomain = domain
         , configAdmin = Address Nothing (T.pack admin)
         , configSiteAddy = Address Nothing (T.pack siteaddy)
         , configCacheDir = cache
         , configLogDirs = logdirs
         }
  case config of
    Left cperr -> error $ show cperr
    Right config -> do
      let providedNetworks = sort (map fst (configLogDirs config))
          knownNetworks = sort (map showNetwork [toEnum 0 ..])
      when (not . null $ knownNetworks \\ providedNetworks) $
        die ("Configuration file does not contain a log directory for all networks (" ++
                intercalate ", " (map showNetwork [toEnum 0 ..]) ++ ")!")
      return config
  where
    options' :: Get_C a
             => ConfigParser -> SectionSpec -> Either CPError [(OptionSpec, a)]
    options' cp sec = do
        keys <- options cp sec
        values <- mapM (get cp sec) keys
        return (zip keys values)
