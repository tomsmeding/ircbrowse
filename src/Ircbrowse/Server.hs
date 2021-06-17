{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | The web server.

module Ircbrowse.Server (runServer) where

import           Data.IRC.Provider (makeProvider)
import           Ircbrowse.Types
import qualified Ircbrowse.Controllers as C
import           Ircbrowse.PerfStats (newPerfContext, wrapTimedRoute)

import           Snap.App
import           Snap.Http.Server           hiding (Config)
import           Snap.Util.FileServe

-- | Run the server.
runServer :: Config -> Pool -> IO ()
runServer config pool = do
  setUnicodeLocale "en_US"
  perfctx <- newPerfContext
  provider <- makeProvider config
  let state = PState { statePerfCtx = perfctx
                     , stateProvider = provider }
      rqPath rq = rqContextPath rq <> rqPathInfo rq  -- The URI, but then without the query
  httpServe server (wrapTimedRoute perfctx (rqPath <$> getRequest) (serve config state pool))

  where server = setPort 10009 defaultConfig

-- | Serve the controllers.
serve :: Config -> PState -> Pool -> Snap ()
serve config state pool = route routes where
  routes = [("/js/",serveDirectory "static/js")
           ,("/css/",serveDirectory "static/css")
           ,("/browse/:channel",run C.browse)
           ,("/nick-cloud/:channel",run C.nickCloud)
           -- ,("/social",run C.socialGraph)
           ,("/day/:channel/:year/:month/:day",run (C.browseDay False))
           ,("/day/:channel/today/:mode",run (C.browseDay True))
           ,("/day/:channel/today",run (C.browseDay True))
           ,("/nick/:nick",run C.nickProfile)
           ,("/nicks/:channel/:mode",run C.allNicks)
           ,("/nicks/:channel",run C.allNicks)
           ,("/quotes.rss",run C.quotes)
           ,("/robots.txt",serveFileAs "text/plain" "static/robots.txt")
           ,("/pdfs/:channel/:unique",run C.pdfs)
           ,("/pdfs/:channel",run C.pdfs)
           ,("/stats/:channel",run C.stats)
           ,("/calendar/:channel",run C.calendar)
           ,("/:channel",run C.rootfile)
           ,("/selection/:channel",run C.browseSpecified)
           -- ,("/export/:filename",run C.export)
           ,("/perfstats",run C.perfStats)
           ,("/",run C.overview)
           ]
  run = runHandler state config pool
