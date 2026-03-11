{-# LANGUAGE OverloadedStrings #-}
module Ircbrowse.View.Help where

import           Ircbrowse.View
import           Ircbrowse.View.Template


helpDataDump :: Html
helpDataDump =
  template "help" "IRC Browse: data dumps" mempty $ do
    container $ row $ do
      h2 $ "Data dump"
      p $ do
        "There is no automatic way to extract a data dump from the system, but if you ask me "
        "nicely I can send you a dump of all events in the database."
      p $ do
        "There is some contact information on "
        a ! href "https://tomsmeding.com" $ "my website"
        "."
    footer
