module UReader.RSS
       ( filterItems
       , reverseItems
       , emptyFeed

       , getRSS
       , fetchFeeds
       ) where

import Control.Applicative
import Control.Concurrent.ParallelIO
import Control.Exception
import Data.ByteString as BS
import Data.Either
import Data.List as L
import Data.Text.Encoding as T
import Network.URI
import Network.HTTP
import Text.RSS.Syntax
import Text.RSS.Import
import Text.XML.Light.Input


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

getRSS :: URI -> IO RSS
getRSS uri = do
  resp <- simpleHTTP $ Request uri GET [] ("" :: ByteString)
  body <- getResponseBody resp
  xml  <- maybe (throwIO $ userError "invalid XML") return $
            parseXMLDoc (T.decodeUtf8 body)
  rss  <- maybe (throwIO $ userError "invalid RSS") return $
            elementToRSS xml
  return rss

fetchFeeds :: [URI] -> IO ([(URI, SomeException)], [RSS])
fetchFeeds urls = (partitionEithers . urlfy) <$> parallelE (L.map getRSS urls)
  where
    urlfy = L.zipWith (\url -> either (Left .  (,) url) Right) urls
