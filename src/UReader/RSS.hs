module UReader.RSS
       ( filterItems
       , reverseItems
       , emptyFeed

       , getRSS
       , fetchFeeds

       , resolveComments
       ) where

import Control.Applicative
import Control.Concurrent.ParallelIO
import Control.Exception
import Data.Either
import Data.List as L
import Data.Text.Encoding as T
import Network.URI
import Network.Curl.Download
import Text.RSS.Syntax
import Text.RSS.Import
import Text.XML.Light.Input

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

fetchFeeds :: [URI] -> IO ([(URI, SomeException)], [RSS])
fetchFeeds urls = (partitionEithers . urlfy) <$> parallelE (L.map getRSS urls)
  where
    urlfy = L.zipWith (\url -> either (Left .  (,) url) Right) urls

resolveComments :: RSS -> IO RSS
resolveComments = error "resolveComments"