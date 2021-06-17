-- | Main entry point.

module Main where

import Ircbrowse.Config (getConfig)
import Ircbrowse.Server (runServer)

import System.Environment (getArgs)
import System.Exit (die)

-- | Main entry point.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [confpath] -> do
        config <- getConfig confpath
        runServer config
    _ ->
        die $ "Usage: ircbrowse <ircbrowse.conf>"
