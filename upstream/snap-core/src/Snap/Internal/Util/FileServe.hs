{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Contains web handlers to serve files from a directory.
module Snap.Internal.Util.FileServe
  ( -- * Helper functions
    getSafePath
    -- * Configuration for directory serving
  , MimeMap
  , HandlerMap
  , DirectoryConfig(..)
  , simpleDirectoryConfig
  , defaultDirectoryConfig
  , fancyDirectoryConfig
  , defaultIndexGenerator
  , defaultMimeTypes
  , fileType
    -- * File servers
  , serveDirectory
  , serveDirectoryWith
  , serveFile
  , serveFileAs
    -- * Internal functions
  , decodeFilePath
  ) where

------------------------------------------------------------------------------
import           Control.Applicative              (Alternative ((<|>)), Applicative ((*>), (<*)), (<$>))
import           Control.Exception.Lifted         (SomeException, catch, evaluate)
import           Control.Monad                    (Monad ((>>), (>>=), return), filterM, forM_, liftM, unless, void, when, (=<<))
import           Control.Monad.IO.Class           (MonadIO (..))
import           Data.Attoparsec.ByteString.Char8 (Parser, char, endOfInput, option, string)
import           Data.ByteString.Builder          (Builder, byteString, char8, stringUtf8, toLazyByteString)
import           Data.ByteString.Char8            (ByteString)
import qualified Data.ByteString.Char8            as S (append, concat, intercalate, isSuffixOf, null, pack, takeWhile)
import qualified Data.ByteString.Lazy.Char8       as L
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as Map (empty, fromList, lookup)
import           Data.List                        (drop, dropWhile, elem, filter, foldl', null, sort, (++))
import           Data.Maybe                       (fromMaybe, isNothing)
import           Data.Monoid                      (Monoid (mappend, mconcat))
import qualified Data.Text                        as T (Text, pack, unpack)
import qualified Data.Text.Encoding               as T (decodeUtf8, encodeUtf8)
import           Data.Word                        (Word64)
import           Prelude                          (Bool (..), Eq (..), FilePath, IO, Maybe (Just, Nothing), Num (..), Ord (..), Show (show), String, const, either, flip, fromIntegral, id, maybe, not, ($), ($!), (.), (||))
import qualified Prelude
import           Snap.Core                        (MonadSnap (..), Request (rqPathInfo, rqQueryString, rqURI), deleteHeader, emptyResponse, finishWith, formatHttpTime, getHeader, getRequest, modifyResponse, parseHttpTime, pass, redirect, sendFile, sendFilePartial, setContentLength, setContentType, setHeader, setResponseBody, setResponseCode, urlDecode, writeBS)
import           Snap.Internal.Debug              (debug)
import           Snap.Internal.Parsing            (fullyParse, parseNum)
import           System.Directory                 (doesDirectoryExist, doesFileExist, getDirectoryContents)
import           System.FilePath                  (isRelative, joinPath, splitDirectories, takeExtensions, takeFileName, (</>))
import           System.PosixCompat.Files         (fileSize, getFileStatus, modificationTime)


------------------------------------------------------------------------------
-- | Gets a path from the 'Request' using 'rqPathInfo' and makes sure it is
-- safe to use for opening files.  A path is safe if it is a relative path
-- and has no ".." elements to escape the intended directory structure.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Snap.Test" as T
-- ghci> import qualified "Data.ByteString.Char8" as B8
-- ghci> T.runHandler (T.get \"\/foo\/bar\" M.empty) ('getSafePath' >>= 'writeBS' . B8.pack)
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Fri, 08 Aug 2014 16:13:20 GMT
--
-- foo\/bar
-- ghci> T.runHandler (T.get \"\/foo\/..\/bar\" M.empty) ('getSafePath' >>= 'writeBS' . B8.pack)
-- HTTP\/1.1 404 Not Found
-- ...
-- @
getSafePath :: MonadSnap m => m FilePath
getSafePath = do
    req <- getRequest
    let mp = urlDecode $ rqPathInfo req

    p <- maybe pass (return . T.unpack . T.decodeUtf8) mp

    -- relative paths only!
    when (not $ isRelative p) pass

    -- check that we don't have any sneaky .. paths
    let dirs = splitDirectories p
    when (elem ".." dirs) pass

    return $! joinPath dirs


------------------------------------------------------------------------------
-- | A type alias for dynamic handlers
type HandlerMap m = HashMap FilePath (FilePath -> m ())


------------------------------------------------------------------------------
-- | A type alias for MIME type
type MimeMap = HashMap FilePath ByteString


------------------------------------------------------------------------------
-- | The default set of mime type mappings we use when serving files. Its
-- value:
--
-- > Map.fromList [
-- >   ( ".asc"     , "text/plain"                                             ),
-- >   ( ".asf"     , "video/x-ms-asf"                                         ),
-- >   ( ".asx"     , "video/x-ms-asf"                                         ),
-- >   ( ".au"      , "audio/basic"                                            ),
-- >   ( ".avi"     , "video/x-msvideo"                                        ),
-- >   ( ".bmp"     , "image/bmp"                                              ),
-- >   ( ".bz2"     , "application/x-bzip"                                     ),
-- >   ( ".c"       , "text/plain"                                             ),
-- >   ( ".class"   , "application/octet-stream"                               ),
-- >   ( ".conf"    , "text/plain"                                             ),
-- >   ( ".cpp"     , "text/plain"                                             ),
-- >   ( ".css"     , "text/css"                                               ),
-- >   ( ".csv"     , "text/csv"                                               ),
-- >   ( ".cxx"     , "text/plain"                                             ),
-- >   ( ".doc"     , "application/msword"                                     ),
-- >   ( ".docx"    , S.append "application/vnd.openxmlformats-officedocument"
-- >                           ".wordprocessingml.document"                    ),
-- >   ( ".dotx"    , S.append "application/vnd.openxmlformats-officedocument"
-- >                           ".wordprocessingml.template"                    ),
-- >   ( ".dtd"     , "application/xml-dtd"                                    ),
-- >   ( ".dvi"     , "application/x-dvi"                                      ),
-- >   ( ".exe"     , "application/octet-stream"                               ),
-- >   ( ".flv"     , "video/x-flv"                                            ),
-- >   ( ".gif"     , "image/gif"                                              ),
-- >   ( ".gz"      , "application/x-gzip"                                     ),
-- >   ( ".hs"      , "text/plain"                                             ),
-- >   ( ".htm"     , "text/html"                                              ),
-- >   ( ".html"    , "text/html"                                              ),
-- >   ( ".ico"     , "image/x-icon"                                           ),
-- >   ( ".jar"     , "application/x-java-archive"                             ),
-- >   ( ".jpeg"    , "image/jpeg"                                             ),
-- >   ( ".jpg"     , "image/jpeg"                                             ),
-- >   ( ".js"      , "text/javascript"                                        ),
-- >   ( ".json"    , "application/json"                                       ),
-- >   ( ".log"     , "text/plain"                                             ),
-- >   ( ".m3u"     , "audio/x-mpegurl"                                        ),
-- >   ( ".m3u8"    , "application/x-mpegURL"                                  ),
-- >   ( ".mka"     , "audio/x-matroska"                                       ),
-- >   ( ".mk3d"    , "video/x-matroska"                                       ),
-- >   ( ".mkv"     , "video/x-matroska"                                       ),
-- >   ( ".mov"     , "video/quicktime"                                        ),
-- >   ( ".mp3"     , "audio/mpeg"                                             ),
-- >   ( ".mp4"     , "video/mp4"                                              ),
-- >   ( ".mpeg"    , "video/mpeg"                                             ),
-- >   ( ".mpg"     , "video/mpeg"                                             ),
-- >   ( ".ogg"     , "application/ogg"                                        ),
-- >   ( ".pac"     , "application/x-ns-proxy-autoconfig"                      ),
-- >   ( ".pdf"     , "application/pdf"                                        ),
-- >   ( ".png"     , "image/png"                                              ),
-- >   ( ".potx"    , S.append "application/vnd.openxmlformats-officedocument"
-- >                           ".presentationml.template"                      ),
-- >   ( ".ppsx"    , S.append "application/vnd.openxmlformats-officedocument"
-- >                           ".presentationml.slideshow"                     ),
-- >   ( ".ppt"     , "application/vnd.ms-powerpoint"                          ),
-- >   ( ".pptx"    , S.append "application/vnd.openxmlformats-officedocument"
-- >                           ".presentationml.presentation"                  ),
-- >   ( ".ps"      , "application/postscript"                                 ),
-- >   ( ".qt"      , "video/quicktime"                                        ),
-- >   ( ".rtf"     , "text/rtf"                                               ),
-- >   ( ".sig"     , "application/pgp-signature"                              ),
-- >   ( ".sldx"    , S.append "application/vnd.openxmlformats-officedocument"
-- >                           ".presentationml.slide"                         ),
-- >   ( ".spl"     , "application/futuresplash"                               ),
-- >   ( ".svg"     , "image/svg+xml"                                          ),
-- >   ( ".swf"     , "application/x-shockwave-flash"                          ),
-- >   ( ".tar"     , "application/x-tar"                                      ),
-- >   ( ".tar.bz2" , "application/x-bzip-compressed-tar"                      ),
-- >   ( ".tar.gz"  , "application/x-tgz"                                      ),
-- >   ( ".tbz"     , "application/x-bzip-compressed-tar"                      ),
-- >   ( ".text"    , "text/plain"                                             ),
-- >   ( ".tgz"     , "application/x-tgz"                                      ),
-- >   ( ".tif"     , "image/tiff"                                             ),
-- >   ( ".tiff"    , "image/tiff"                                             ),
-- >   ( ".torrent" , "application/x-bittorrent"                               ),
-- >   ( ".ts"      , "video/mp2t"                                             ),
-- >   ( ".txt"     , "text/plain"                                             ),
-- >   ( ".wav"     , "audio/x-wav"                                            ),
-- >   ( ".wax"     , "audio/x-ms-wax"                                         ),
-- >   ( ".webm"    , "video/webm"                                             ),
-- >   ( ".wma"     , "audio/x-ms-wma"                                         ),
-- >   ( ".wmv"     , "video/x-ms-wmv"                                         ),
-- >   ( ".xbm"     , "image/x-xbitmap"                                        ),
-- >   ( ".xlam"    , "application/vnd.ms-excel.addin.macroEnabled.12"         ),
-- >   ( ".xls"     , "application/vnd.ms-excel"                               ),
-- >   ( ".xlsb"    , "application/vnd.ms-excel.sheet.binary.macroEnabled.12"  ),
-- >   ( ".xlsx"    , S.append "application/vnd.openxmlformats-officedocument."
-- >                           "spreadsheetml.sheet"                           ),
-- >   ( ".xltx"    , S.append "application/vnd.openxmlformats-officedocument."
-- >                           "spreadsheetml.template"                        ),
-- >   ( ".xml"     , "text/xml"                                               ),
-- >   ( ".xpm"     , "image/x-xpixmap"                                        ),
-- >   ( ".xwd"     , "image/x-xwindowdump"                                    ),
-- >   ( ".zip"     , "application/zip"                                        ) ]

defaultMimeTypes :: MimeMap
defaultMimeTypes =
  Map.fromList [
    ( ".asc"     , "text/plain"                                             ),
    ( ".asf"     , "video/x-ms-asf"                                         ),
    ( ".asx"     , "video/x-ms-asf"                                         ),
    ( ".au"      , "audio/basic"                                            ),
    ( ".avi"     , "video/x-msvideo"                                        ),
    ( ".bmp"     , "image/bmp"                                              ),
    ( ".bz2"     , "application/x-bzip"                                     ),
    ( ".c"       , "text/plain"                                             ),
    ( ".class"   , "application/octet-stream"                               ),
    ( ".conf"    , "text/plain"                                             ),
    ( ".cpp"     , "text/plain"                                             ),
    ( ".css"     , "text/css"                                               ),
    ( ".csv"     , "text/csv"                                               ),
    ( ".cxx"     , "text/plain"                                             ),
    ( ".doc"     , "application/msword"                                     ),
    ( ".docx"    , S.append "application/vnd.openxmlformats-officedocument"
                            ".wordprocessingml.document"                    ),
    ( ".dotx"    , S.append "application/vnd.openxmlformats-officedocument"
                            ".wordprocessingml.template"                    ),
    ( ".dtd"     , "application/xml-dtd"                                    ),
    ( ".dvi"     , "application/x-dvi"                                      ),
    ( ".exe"     , "application/octet-stream"                               ),
    ( ".flv"     , "video/x-flv"                                            ),
    ( ".gif"     , "image/gif"                                              ),
    ( ".gz"      , "application/x-gzip"                                     ),
    ( ".hs"      , "text/plain"                                             ),
    ( ".htm"     , "text/html"                                              ),
    ( ".html"    , "text/html"                                              ),
    ( ".ico"     , "image/x-icon"                                           ),
    ( ".jar"     , "application/x-java-archive"                             ),
    ( ".jpeg"    , "image/jpeg"                                             ),
    ( ".jpg"     , "image/jpeg"                                             ),
    ( ".js"      , "text/javascript"                                        ),
    ( ".json"    , "application/json"                                       ),
    ( ".log"     , "text/plain"                                             ),
    ( ".m3u"     , "audio/x-mpegurl"                                        ),
    ( ".m3u8"    , "application/x-mpegURL"                                  ),
    ( ".mka"     , "audio/x-matroska"                                       ),
    ( ".mk3d"    , "video/x-matroska"                                       ),
    ( ".mkv"     , "video/x-matroska"                                       ),
    ( ".mov"     , "video/quicktime"                                        ),
    ( ".mp3"     , "audio/mpeg"                                             ),
    ( ".mp4"     , "video/mp4"                                              ),
    ( ".mpeg"    , "video/mpeg"                                             ),
    ( ".mpg"     , "video/mpeg"                                             ),
    ( ".ogg"     , "application/ogg"                                        ),
    ( ".pac"     , "application/x-ns-proxy-autoconfig"                      ),
    ( ".pdf"     , "application/pdf"                                        ),
    ( ".png"     , "image/png"                                              ),
    ( ".potx"    , S.append "application/vnd.openxmlformats-officedocument"
                            ".presentationml.template"                      ),
    ( ".ppsx"    , S.append "application/vnd.openxmlformats-officedocument"
                            ".presentationml.slideshow"                     ),
    ( ".ppt"     , "application/vnd.ms-powerpoint"                          ),
    ( ".pptx"    , S.append "application/vnd.openxmlformats-officedocument"
                            ".presentationml.presentation"                  ),
    ( ".ps"      , "application/postscript"                                 ),
    ( ".qt"      , "video/quicktime"                                        ),
    ( ".rtf"     , "text/rtf"                                               ),
    ( ".sig"     , "application/pgp-signature"                              ),
    ( ".sldx"    , S.append "application/vnd.openxmlformats-officedocument"
                            ".presentationml.slide"                         ),
    ( ".spl"     , "application/futuresplash"                               ),
    ( ".svg"     , "image/svg+xml"                                          ),
    ( ".swf"     , "application/x-shockwave-flash"                          ),
    ( ".tar"     , "application/x-tar"                                      ),
    ( ".tar.bz2" , "application/x-bzip-compressed-tar"                      ),
    ( ".tar.gz"  , "application/x-tgz"                                      ),
    ( ".tbz"     , "application/x-bzip-compressed-tar"                      ),
    ( ".text"    , "text/plain"                                             ),
    ( ".tgz"     , "application/x-tgz"                                      ),
    ( ".tiff"    , "image/tiff"                                             ),
    ( ".tif"     , "image/tiff"                                             ),
    ( ".torrent" , "application/x-bittorrent"                               ),
    ( ".ts"      , "video/mp2t"                                             ),
    ( ".ttf"     , "font/ttf"                                               ),
    ( ".txt"     , "text/plain"                                             ),
    ( ".wav"     , "audio/x-wav"                                            ),
    ( ".wax"     , "audio/x-ms-wax"                                         ),
    ( ".webm"    , "video/webm"                                             ),
    ( ".wma"     , "audio/x-ms-wma"                                         ),
    ( ".wmv"     , "video/x-ms-wmv"                                         ),
    ( ".xbm"     , "image/x-xbitmap"                                        ),
    ( ".xlam"    , "application/vnd.ms-excel.addin.macroEnabled.12"         ),
    ( ".xls"     , "application/vnd.ms-excel"                               ),
    ( ".xlsb"    , "application/vnd.ms-excel.sheet.binary.macroEnabled.12"  ),
    ( ".xlsx"    , S.append "application/vnd.openxmlformats-officedocument."
                            "spreadsheetml.sheet"                           ),
    ( ".xltx"    , S.append "application/vnd.openxmlformats-officedocument."
                            "spreadsheetml.template"                        ),
    ( ".xml"     , "text/xml"                                               ),
    ( ".xpm"     , "image/x-xpixmap"                                        ),
    ( ".xwd"     , "image/x-xwindowdump"                                    ),
    ( ".zip"     , "application/zip"                                        ) ]


------------------------------------------------------------------------------
-- | A collection of options for serving static files out of a directory.
data DirectoryConfig m = DirectoryConfig {
    -- | Files to look for when a directory is requested (e.g., index.html)
    indexFiles      :: [FilePath],

    -- | Handler to generate a directory listing if there is no index.
    indexGenerator  :: FilePath -> m (),

    -- | Map of extensions to pass to dynamic file handlers.  This could be
    -- used, for example, to implement CGI dispatch, pretty printing of source
    -- code, etc.
    dynamicHandlers :: HandlerMap m,

    -- | MIME type map to look up content types.
    mimeTypes       :: MimeMap,

    -- | Handler that is called before a file is served.  It will only be
    -- called when a file is actually found, not for generated index pages.
    preServeHook    :: FilePath -> m ()
    }


------------------------------------------------------------------------------
-- | Style information for the default directory index generator.
snapIndexStyles :: ByteString
snapIndexStyles =
    S.intercalate "\n"
        [ "body { margin: 0px 0px 0px 0px; font-family: sans-serif }"
        , "div.header {"
        ,     "padding: 40px 40px 0px 40px; height:35px;"
        ,     "background:rgb(25,50,87);"
        ,     "background-image:-webkit-gradient("
        ,         "linear,left bottom,left top,"
        ,         "color-stop(0.00, rgb(31,62,108)),"
        ,         "color-stop(1.00, rgb(19,38,66)));"
        ,     "background-image:-moz-linear-gradient("
        ,         "center bottom,rgb(31,62,108) 0%,rgb(19,38,66) 100%);"
        ,     "text-shadow:-1px 3px 1px rgb(16,33,57);"
        ,     "font-size:16pt; letter-spacing: 2pt; color:white;"
        ,     "border-bottom:10px solid rgb(46,93,156) }"
        , "div.content {"
        ,     "background:rgb(255,255,255);"
        ,     "background-image:-webkit-gradient("
        ,         "linear,left bottom, left top,"
        ,         "color-stop(0.50, rgb(255,255,255)),"
        ,         "color-stop(1.00, rgb(224,234,247)));"
        ,     "background-image:-moz-linear-gradient("
        ,         "center bottom, white 50%, rgb(224,234,247) 100%);"
        ,     "padding: 40px 40px 40px 40px }"
        , "div.footer {"
        ,     "padding: 16px 0px 10px 10px; height:31px;"
        ,     "border-top: 1px solid rgb(194,209,225);"
        ,     "color: rgb(160,172,186); font-size:10pt;"
        ,     "background: rgb(245,249,255) }"
        , "table { max-width:100%; margin: 0 auto;" `S.append`
          " border-collapse: collapse; }"
        , "tr:hover { background:rgb(256,256,224) }"
        , "td { border:0; font-family:monospace; padding: 2px 0; }"
        , "td.filename, td.type { padding-right: 2em; }"
        , "th { border:0; background:rgb(28,56,97);"
        ,      "text-shadow:-1px 3px 1px rgb(16,33,57); color: white}"
        ]


------------------------------------------------------------------------------
-- | An automatic index generator, which is fairly small and does not rely on
-- any external files (which may not be there depending on external request
-- routing).
--
-- A 'MimeMap' is passed in to display the types of files in the directory
-- listing based on their extension.  Preferably, this is the same as the map
-- in the 'DirectoryConfig'
--
-- The styles parameter allows you to apply styles to the directory listing.
-- The listing itself consists of a table, containing a header row using
-- th elements, and one row per file using td elements, so styles for those
-- pieces may be attached to the appropriate tags.
defaultIndexGenerator :: MonadSnap m
                      => MimeMap    -- ^ MIME type mapping for reporting types
                      -> ByteString -- ^ Style info to insert in header
                      -> FilePath   -- ^ Directory to generate index for
                      -> m ()
defaultIndexGenerator mm styles d = do
    modifyResponse $ setContentType "text/html; charset=utf-8"
    rq      <- getRequest

    let uri   = uriWithoutQueryString rq
    let pInfo = rqPathInfo rq

    writeBS "<!DOCTYPE html>\n<html>\n<head>"
    writeBS "<title>Directory Listing: "
    writeBS uri
    writeBS "</title>"
    writeBS "<style type='text/css'>"
    writeBS styles
    writeBS "</style></head><body>"
    writeBS "<div class=\"header\">Directory Listing: "
    writeBS uri
    writeBS "</div><div class=\"content\">"
    writeBS "<table><tr><th>File Name</th><th>Type</th><th>Last Modified"
    writeBS "</th></tr>"

    when (pInfo /= "") $
        writeBS "<tr><td><a href='../'>..</a></td><td colspan=2>DIR</td></tr>"

    entries <- liftIO $ getDirectoryContents d
    dirs    <- liftIO $ filterM (doesDirectoryExist . (d </>)) entries
    files   <- liftIO $ filterM (doesFileExist . (d </>)) entries

    forM_ (sort $ filter (not . (`elem` ["..", "."])) dirs) $ \f0 -> do
        f <- liftIO $ liftM (\s -> T.encodeUtf8 s `mappend` "/")
                    $ decodeFilePath f0
        writeBS "<tr><td class='filename'><a href='"
        writeBS f
        writeBS "'>"
        writeBS f
        writeBS "</a></td><td class='type' colspan=2>DIR</td></tr>"

    forM_ (sort files) $ \f0 -> do
        f <- liftIO $ liftM T.encodeUtf8 $ decodeFilePath f0
        stat <- liftIO $ getFileStatus (d </> f0)
        tm   <- liftIO $ formatHttpTime (modificationTime stat)
        writeBS "<tr><td class='filename'><a href='"
        writeBS f
        writeBS "'>"
        writeBS f
        writeBS "</a></td><td class='type'>"
        writeBS (fileType mm f0)
        writeBS "</td><td>"
        writeBS tm
        writeBS "</tr>"

    writeBS "</table></div><div class=\"footer\">Powered by "
    writeBS "<b><a href=\"http://snapframework.com/\">Snap</a></b></div>"
    writeBS "</body>"


------------------------------------------------------------------------------
decodeFilePath :: FilePath -> IO T.Text
decodeFilePath fp = do
    evaluate (T.decodeUtf8 bs) `catch`
        (\(_::SomeException) -> return (T.pack fp))
  where
    bs = S.pack fp

------------------------------------------------------------------------------
-- | A very simple configuration for directory serving.  This configuration
-- uses built-in MIME types from 'defaultMimeTypes', and has no index files,
-- index generator, dynamic file handlers, or 'preServeHook'.
simpleDirectoryConfig :: MonadSnap m => DirectoryConfig m
simpleDirectoryConfig = DirectoryConfig {
    indexFiles      = [],
    indexGenerator  = const pass,
    dynamicHandlers = Map.empty,
    mimeTypes       = defaultMimeTypes,
    preServeHook    = const $ return $! ()
    }


------------------------------------------------------------------------------
-- | A reasonable default configuration for directory serving.  This
-- configuration uses built-in MIME types from 'defaultMimeTypes', serves
-- common index files @index.html@ and @index.htm@, but does not autogenerate
-- directory indexes, nor have any dynamic file handlers. The 'preServeHook'
-- will not do anything.
defaultDirectoryConfig :: MonadSnap m => DirectoryConfig m
defaultDirectoryConfig = DirectoryConfig {
    indexFiles      = ["index.html", "index.htm"],
    indexGenerator  = const pass,
    dynamicHandlers = Map.empty,
    mimeTypes       = defaultMimeTypes,
    preServeHook    = const $ return $! ()
    }


------------------------------------------------------------------------------
-- | A more elaborate configuration for file serving.  This configuration
-- uses built-in MIME types from 'defaultMimeTypes', serves common index files
-- @index.html@ and @index.htm@, and autogenerates directory indexes with a
-- Snap-like feel.  It still has no dynamic file handlers, nor 'preServeHook',
-- which should be added as needed.
--
-- Files recognized as indexes include @index.html@, @index.htm@,
-- @default.html@, @default.htm@, @home.html@
--
-- Example of how the autogenerated directory index looks like:
--
-- <<data:image/png;base64,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>>

fancyDirectoryConfig :: MonadSnap m => DirectoryConfig m
fancyDirectoryConfig = DirectoryConfig {
    indexFiles      = ["index.html", "index.htm"],
    indexGenerator  = defaultIndexGenerator defaultMimeTypes snapIndexStyles,
    dynamicHandlers = Map.empty,
    mimeTypes       = defaultMimeTypes,
    preServeHook    = const $ return $! ()
    }


------------------------------------------------------------------------------
-- | Serves static files from a directory using the default configuration
-- as given in 'defaultDirectoryConfig'.
serveDirectory :: MonadSnap m
               => FilePath           -- ^ Directory to serve from
               -> m ()
serveDirectory = serveDirectoryWith defaultDirectoryConfig
{-# INLINE serveDirectory #-}


------------------------------------------------------------------------------
-- | Serves static files from a directory.  Configuration options are
-- passed in a 'DirectoryConfig' that captures various choices about desired
-- behavior.  The relative path given in 'rqPathInfo' is searched for a
-- requested file, and the file is served with the appropriate mime type if it
-- is found. Absolute paths and \"@..@\" are prohibited to prevent files from
-- being served from outside the sandbox.
serveDirectoryWith :: MonadSnap m
                   => DirectoryConfig m  -- ^ Configuration options
                   -> FilePath           -- ^ Directory to serve from
                   -> m ()
serveDirectoryWith cfg base = do
    b <- directory <|> file <|> redir
    when (not b) pass

  where
    idxs     = indexFiles cfg
    generate = indexGenerator cfg
    mimes    = mimeTypes cfg
    dyns     = dynamicHandlers cfg
    pshook   = preServeHook cfg

    -- Serves a file if it exists; passes if not
    serve f = do
        liftIO (doesFileExist f) >>= flip unless pass
        let fname          = takeFileName f
        let staticServe f' = pshook f >> serveFileAs (fileType mimes fname) f'
        lookupExt staticServe dyns fname f >> return True

    -- Serves a directory via indices if available.  Returns True on success,
    -- False on failure to find an index.  Passes /only/ if the request was
    -- not for a directory (no trailing slash).
    directory = do
        rq  <- getRequest
        let uri = uriWithoutQueryString rq
        unless ("/" `S.isSuffixOf` uri) pass
        rel <- (base </>) <$> getSafePath
        b   <- liftIO $ doesDirectoryExist rel
        if b then do let serveRel f = serve (rel </> f)
                     foldl' (<|>) pass (Prelude.map serveRel idxs)
                         <|> (generate rel >> return True)
                         <|> return False
             else return False

    -- Serves a file requested by name.  Passes if the file doesn't exist.
    file = serve =<< ((base </>) <$> getSafePath)

    -- If the request is for a directory but lacks a trailing slash, redirects
    -- to the directory name with a trailing slash.
    redir = do
        rel <- (base </>) <$> getSafePath
        liftIO (doesDirectoryExist rel) >>= flip unless pass
        rq <- getRequest
        let uri = uriWithoutQueryString rq
        let qss = queryStringSuffix rq
        let u = S.concat [uri, "/", qss]
        redirect u


------------------------------------------------------------------------------
-- | Serves a single file specified by a full or relative path.  If the file
-- does not exist, throws an exception (not that it does /not/ pass to the
-- next handler).   The path restrictions on 'serveDirectory' don't apply to
-- this function since the path is not being supplied by the user.
serveFile :: MonadSnap m
          => FilePath          -- ^ path to file
          -> m ()
serveFile fp = serveFileAs (fileType defaultMimeTypes (takeFileName fp)) fp
{-# INLINE serveFile #-}


------------------------------------------------------------------------------
-- | Same as 'serveFile', with control over the MIME mapping used.
serveFileAs :: MonadSnap m
            => ByteString        -- ^ MIME type
            -> FilePath          -- ^ path to file
            -> m ()
serveFileAs mime fp = do
    reqOrig <- getRequest

    -- If-Range header must be ignored if there is no Range: header in the
    -- request (RFC 2616 section 14.27)
    let req = if isNothing $ getHeader "range" reqOrig
                then deleteHeader "if-range" reqOrig
                else reqOrig

    -- check "If-Modified-Since" and "If-Range" headers
    let mbH = getHeader "if-modified-since" req
    mbIfModified <- liftIO $ case mbH of
                               Nothing  -> return Nothing
                               (Just s) -> liftM Just $ parseHttpTime s

    -- If-Range header could contain an entity, but then parseHttpTime will
    -- fail and return 0 which means a 200 response will be generated anyways
    mbIfRange <- liftIO $ case getHeader "if-range" req of
                            Nothing  -> return Nothing
                            (Just s) -> liftM Just $ parseHttpTime s

    dbg $ "mbIfModified: " ++ Prelude.show mbIfModified
    dbg $ "mbIfRange: " ++ Prelude.show mbIfRange

    -- check modification time and bug out early if the file is not modified.
    --
    -- TODO: a stat cache would be nice here, but it'd need the date thread
    -- stuff from snap-server to be folded into snap-core
    filestat <- liftIO $ getFileStatus fp
    let mt = modificationTime filestat
    maybe (return $! ()) (\lt -> when (mt <= lt) notModified) mbIfModified

    let sz = fromIntegral $ fileSize filestat
    lm <- liftIO $ formatHttpTime mt

    -- ok, at this point we know the last-modified time and the
    -- content-type. set those.
    modifyResponse $ setHeader "Last-Modified" lm
                   . setHeader "Accept-Ranges" "bytes"
                   . setContentType mime


    -- now check: is this a range request? If there is an 'If-Range' header
    -- with an old modification time we skip this check and send a 200
    -- response
    let skipRangeCheck = maybe (False)
                               (\lt -> mt > lt)
                               mbIfRange

    -- checkRangeReq checks for a Range: header in the request and sends a
    -- partial response if it matches.
    wasRange <- if skipRangeCheck
                  then return False
                  else liftSnap $ checkRangeReq req fp sz

    dbg $ "was this a range request? " ++ Prelude.show wasRange

    -- if we didn't have a range request, we just do normal sendfile
    unless wasRange $ do
      modifyResponse $ setResponseCode 200
                     . setContentLength sz
      liftSnap $ sendFile fp

  where
    --------------------------------------------------------------------------
    notModified = finishWith $
                  setResponseCode 304 emptyResponse


------------------------------------------------------------------------------
lookupExt :: a -> HashMap FilePath a -> FilePath -> a
lookupExt def m f =
    if null ext
      then def
      else fromMaybe (lookupExt def m (next ext)) mbe

  where
    next            = dropWhile (/= '.') . drop 1
    ext             = takeExtensions f
    mbe             = Map.lookup ext m


------------------------------------------------------------------------------
-- | Determine a given file's MIME type from its filename and the provided MIME
-- map.
fileType :: MimeMap -> FilePath -> ByteString
fileType = lookupExt defaultMimeType


------------------------------------------------------------------------------
defaultMimeType :: ByteString
defaultMimeType = "application/octet-stream"


------------------------------------------------------------------------------
data RangeReq = RangeReq !Word64 !(Maybe Word64)
              | SuffixRangeReq !Word64


------------------------------------------------------------------------------
rangeParser :: Parser RangeReq
rangeParser = string "bytes=" *>
              (byteRangeSpec <|> suffixByteRangeSpec) <*
              endOfInput
  where
    byteRangeSpec = do
        start <- fromIntegral <$> parseNum
        void $! char '-'
        end   <- option Nothing $ liftM Just parseNum

        return $! RangeReq start (fromIntegral <$> end)

    suffixByteRangeSpec =
        liftM (SuffixRangeReq . fromIntegral) $ char '-' *> parseNum


------------------------------------------------------------------------------
checkRangeReq :: (MonadSnap m) => Request -> FilePath -> Word64 -> m Bool
checkRangeReq req fp sz = do
    -- TODO/FIXME: multiple ranges
    maybe (return False)
          (\s -> either (const $ return False)
                        withRange
                        (fullyParse s rangeParser))
          (getHeader "range" req)

  where
    withRange (RangeReq start mend) = do
        let end = fromMaybe (sz-1) mend
        dbg $ "withRange: start=" ++ Prelude.show start
                  ++ ", end=" ++ Prelude.show end

        if start < 0 || end < start || start >= sz || end >= sz
           then send416
           else send206 start end

    withRange (SuffixRangeReq nbytes) = do
        let end   = sz-1
        let start = sz - nbytes

        dbg $ "withRange: start=" ++ Prelude.show start
                  ++ ", end=" ++ Prelude.show end

        if start < 0 || end < start || start >= sz || end >= sz
           then send416
           else send206 start end

    -- note: start and end INCLUSIVE here
    send206 start end = do
        dbg "inside send206"
        let !len = end-start+1
        let crng = S.concat . L.toChunks $
                   toLazyByteString $
                   mconcat [ byteString "bytes "
                           , fromShow start
                           , char8 '-'
                           , fromShow end
                           , char8 '/'
                           , fromShow sz ]

        modifyResponse $ setResponseCode 206
                       . setHeader "Content-Range" crng
                       . setContentLength len

        dbg $ "send206: sending range (" ++ Prelude.show start
                ++ "," ++ Prelude.show (end+1) ++ ") to sendFilePartial"

        -- end here was inclusive, sendFilePartial is exclusive
        sendFilePartial fp (start,end+1)
        return True


    send416 = do
        dbg "inside send416"
        -- if there's an "If-Range" header in the request, then we just send
        -- back 200
        if getHeader "If-Range" req /= Nothing
           then return False
           else do
               let crng = S.concat . L.toChunks $
                          toLazyByteString $
                          mconcat [ byteString "bytes */"
                                  , fromShow sz ]

               modifyResponse $ setResponseCode 416
                              . setHeader "Content-Range" crng
                              . setContentLength 0
                              . deleteHeader "Content-Type"
                              . deleteHeader "Content-Encoding"
                              . deleteHeader "Transfer-Encoding"
                              . setResponseBody (return . id)

               return True


------------------------------------------------------------------------------
dbg :: (MonadIO m) => String -> m ()
dbg s = debug $ "FileServe:" ++ s


------------------------------------------------------------------------------
uriWithoutQueryString :: Request -> ByteString
uriWithoutQueryString rq = S.takeWhile (/= '?') uri
  where
    uri   = rqURI rq


------------------------------------------------------------------------------
queryStringSuffix :: Request -> ByteString
queryStringSuffix rq = S.concat [ s, qs ]
  where
    qs = rqQueryString rq
    s  = if S.null qs then "" else "?"


------------------------------------------------------------------------------
fromShow :: Show a => a -> Builder
fromShow = stringUtf8 . show
