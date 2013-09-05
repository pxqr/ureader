module UReader.RSS
       ( filterItems
       , reverseItems
       , emptyFeed

       , getRSS
       , fetchFeeds

       , resolveComments
       ) where

import Control.Applicative
import Control.Arrow
import Control.Concurrent.ParallelIO
import Control.Exception
import Data.Char
import Data.Either
import Data.List as L
import Data.Maybe
import Data.Text.Encoding as T
import Data.Time
import Network.URI
import Network.Curl
import Network.Curl.Download
import Text.RSS.Syntax
import Text.RSS.Import
import Text.XML.Light.Input
import System.Locale


-- TODO filter by time = drop . find
filterItems :: (RSSItem -> Bool) -> RSS -> RSS
filterItems p rss = rss
    { rssChannel = let ch = rssChannel rss
                   in ch { rssItems = L.filter p (rssItems ch) }
    } -- TODO use lens

reverseItems :: RSS -> RSS
reverseItems rss = rss
  { rssChannel = let ch = rssChannel rss
                 in ch { rssItems = L.reverse (rssItems ch) }
  }

emptyFeed :: RSS -> Bool
emptyFeed = L.null . rssItems . rssChannel

-- TODO openAsFeed
getRSS :: URI -> IO RSS
getRSS uri = do
  body <- either (throwIO . userError) return =<< openURI (show uri)
  xml  <- maybe (throwIO $ userError "invalid XML") return $
            parseXMLDoc (T.decodeUtf8 body)
  rss  <- maybe  (throwIO $ userError "invalid RSS") return $
            elementToRSS xml
  return rss

hdrLastModified :: String
hdrLastModified = "last-modified"

lastModifiedFormat :: String
lastModifiedFormat = "%a, %e %b %Y %H:%M:%S %Z"

parseLastModified :: String -> Maybe UTCTime
parseLastModified = parseTime defaultTimeLocale lastModifiedFormat

resourceExpired :: UTCTime -> URI -> IO Bool
resourceExpired threshold uri = do
    (_, headers) <- curlHead (show uri) []
    let cheaders = L.map (first canonicalize) headers
    return $ case L.lookup hdrLastModified cheaders of
      Just (parseLastModified -> Just lm) -> threshold < lm
      _ -> True
  where
    canonicalize = L.map toLower


getRSSFrom :: Maybe UTCTime -> URI -> IO (Maybe RSS)
getRSSFrom  Nothing uri = Just <$> getRSS uri
getRSSFrom (Just t) uri = do
  updated <- resourceExpired t uri
  if updated
    then Just <$> getRSS uri
    else return Nothing

fetchFeeds :: Maybe UTCTime -> [URI]
           -> IO ([(URI, SomeException)], [RSS])
fetchFeeds mtime urls = do
    res <- parallelE (L.map (getRSSFrom mtime) urls)
    return $ second catMaybes $ partitionEithers $ urlfy res
  where
    urlfy = L.zipWith (\url -> either (Left .  (,) url) Right) urls


resolveComments :: RSS -> IO RSS
resolveComments = error "resolveComments"