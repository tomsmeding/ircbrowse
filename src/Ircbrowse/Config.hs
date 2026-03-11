-- | Load the configuration file.

module Ircbrowse.Config (getConfig) where

import Ircbrowse.Types
import Ircbrowse.Types.Import (showNetwork)

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.State (StateT, modify, execStateT)
import Data.Bifunctor (first)
import Data.List (intercalate, sort, (\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Database.PostgreSQL.Simple (ConnectInfo(..))
import qualified Data.Text as T
import Network.Mail.Mime
import System.Exit (die)

getConfig :: FilePath -> IO Config
getConfig conf = do
  contents <- readFile conf
  let config = do
        c <- parseConfig contents
        pghost <- get c "POSTGRESQL" "host"
        pgport <- get c "POSTGRESQL" "port"
        pguser <- get c "POSTGRESQL" "user"
        pgpass <- get c "POSTGRESQL" "pass"
        pgdb   <- get c "POSTGRESQL" "db"
        domain <- get c "WEB" "domain"
        cache  <- get c "WEB" "cache"
        admin    <- get c "ADDRESSES" "admin"
        siteaddy <- get c "ADDRESSES" "site_addy"
        logdirs <- getSection c "LOGDIRS"

        return Config {
           configPostgres = ConnectInfo pghost (read pgport) pguser pgpass pgdb
         , configDomain = domain
         , configAdmin = Address Nothing (T.pack admin)
         , configSiteAddy = Address Nothing (T.pack siteaddy)
         , configCacheDir = cache
         , configLogDirs = logdirs
         }
  case config of
    Left cperr -> error cperr
    Right config -> do
      let providedNetworks = sort (map fst (configLogDirs config))
          knownNetworks = sort (map showNetwork [toEnum 0 ..])
      when (not . null $ knownNetworks \\ providedNetworks) $
        die ("Configuration file does not contain a log directory for all networks (" ++
                intercalate ", " (map showNetwork [toEnum 0 ..]) ++ ")!")
      return config

-- A reimplementation of a part of the ConfigFile library.
newtype ParsedConfig = ParsedConfig (Map String (Map String String))
  deriving (Show)

get :: ParsedConfig -> String -> String -> Either String String
get (ParsedConfig mp) = \sec opt ->
  case get' sec opt of
    Right res -> Right res
    Left err ->
      case get' "DEFAULT" opt of
        Right res -> Right res
        Left _ -> Left err
  where
    get' :: String -> String -> Either String String
    get' sec opt =
      case Map.lookup sec mp of
        Nothing -> Left $ "Section '" ++ sec ++ "' not found in config file"
        Just mp' ->
          case Map.lookup opt mp' of
            Nothing -> Left $ "Option '" ++ opt ++ "' not found in section '" ++
                              sec ++ "' in config file"
            Just res -> Right res

getSection :: ParsedConfig -> String -> Either String [(String, String)]
getSection (ParsedConfig mp) sec =
  case Map.lookup sec mp of
    Just mp' -> Right (Map.assocs mp')
    Nothing -> Left $ "Section '" ++ sec ++ "' not found in config file"

parseConfig :: String -> Either String ParsedConfig
parseConfig = \input -> runExcept (execStateT (go "DEFAULT" (lines input)) (ParsedConfig mempty))
  where
    go :: String -> [String] -> StateT ParsedConfig (Except String) ()
    go _ [] = return ()
    go sec (line : lns) = case line of
      '#' : _ -> go sec lns
      ';' : _ -> go sec lns
      _ | all (`elem` whitespace) line -> go sec lns

      '[' : ln ->
        case break (== ']') ln of
          (sec', "]") -> go sec' lns
          (_, ']' : _) -> throwError "Invalid data after section heading in config file"
          _ -> throwError "Missing ']' in section heading in config file"

      _ ->
        case break (`elem` ":=\r\n") line of
          (optname@(_:_), r1 : rest) | r1 `elem` ":=" -> do
            let (conts, restlns) = scanContLines lns
                value = intercalate "\n" (map trim (rest : conts))
            add sec (trim optname) value
            go sec restlns
          _ -> throwError "Missing '=' or ':' in option line"

    scanContLines :: [String] -> ([String], [String])
    scanContLines [] = ([], [])
    scanContLines ((c:cs) : lns)
      | c `elem` whitespace = first (cs:) (scanContLines lns)
    scanContLines lns = ([], lns)

    add :: String -> String -> String -> StateT ParsedConfig (Except String) ()
    add sec opt val = modify $ \(ParsedConfig mp) -> ParsedConfig $
      -- insertWith passes newValue as the first argument to 'union', and 'union' prefers left
      Map.insertWith Map.union sec (Map.singleton opt val) mp

    whitespace :: String
    whitespace = " \t"

    trim :: String -> String
    trim = reverse . dropWhile (`elem` whitespace) . reverse . dropWhile (`elem` whitespace)
