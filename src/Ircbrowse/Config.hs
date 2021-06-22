-- | Load the configuration file.

module Ircbrowse.Config (
  Config(..),
  configLogDirFor,
  getConfig,
) where

import Ircbrowse.Types.Import (Network, showNetwork)

import Control.Monad (when)
import Data.ConfigFile
import Data.List (intercalate, sort, (\\))
import qualified Data.Text as T
import Network.Mail.Mime
import Snap.App.Cache (CacheDir(..))
import Snap.App.Types (AppConfig(..))
import System.Exit (die)


-- | Site-wide configuration.
data Config = Config
  { configDomain          :: !String
  , configAdmin           :: !Address
  , configSiteAddy        :: !Address
  , configCacheDir        :: !FilePath
  , configLogDirs         :: ![(String, FilePath)]
  }

configLogDirFor :: Config -> Network -> FilePath
configLogDirFor cfg netw =
    case lookup (showNetwork netw) (configLogDirs cfg) of
      Just fp -> fp
      Nothing -> error ("No irc log directory specified in config for network " ++ showNetwork netw)

instance AppConfig Config where
  getConfigDomain = configDomain

instance CacheDir Config where
  getCacheDir = configCacheDir

getConfig :: FilePath -> IO Config
getConfig conf = do
  contents <- readFile conf
  let econfig = do
        c <- readstring emptyCP contents
        domain <- get c "WEB" "domain"
        cache  <- get c "WEB" "cache"
        admin    <- get c "ADDRESSES" "admin"
        siteaddy <- get c "ADDRESSES" "site_addy"
        logdirs <- options' c "LOGDIRS"

        return Config {
           configDomain = domain
         , configAdmin = Address Nothing (T.pack admin)
         , configSiteAddy = Address Nothing (T.pack siteaddy)
         , configCacheDir = cache
         , configLogDirs = logdirs
         }
  case econfig of
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
