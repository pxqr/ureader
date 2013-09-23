-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This module provides network related stuff.
--
module UReader.Feed
       ( filterItems
       , reverseItems
       , emptyFeed
       , isNew

       , getFeed
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
import Data.Time
import Network.URI
import Network.Curl
import Network.Curl.Download
import Text.Feed.Types as Generic
import Text.Atom.Feed  as Atom hiding (URI)
import Text.RSS.Syntax as RSS
import System.Locale

import UReader.Localization


-- TODO filter by time = drop . find
filterItems :: (Item -> Bool) -> Generic.Feed -> Generic.Feed
filterItems p (AtomFeed atom) = AtomFeed $ atom
    { feedEntries = L.filter (p . Generic.AtomItem) $ feedEntries atom
    }
filterItems p (RSSFeed rss) = RSSFeed $ rss
    { rssChannel = let ch = rssChannel rss
                   in ch { rssItems = L.filter (p . Generic.RSSItem)
                                      $ rssItems ch }
    } -- TODO use lens

reverseItems :: Generic.Feed -> Generic.Feed
reverseItems (RSSFeed rss) = RSSFeed $ rss
  { rssChannel = let ch = rssChannel rss
                 in ch { rssItems = L.reverse (rssItems ch) }
  }

emptyFeed :: Generic.Feed -> Bool
emptyFeed (RSSFeed rss) = L.null . rssItems . rssChannel $ rss

isNew :: UTCTime -> Generic.Item -> Bool
isNew lastSeen (Generic.RSSItem item) = pubDate item > Just lastSeen

-- TODO openAsFeed
getFeed :: URI -> IO Generic.Feed
getFeed uri = either (throwIO . userError) return =<< openAsFeed (show uri)

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


getFeedUpdate :: Maybe UTCTime -> URI -> IO (Maybe Generic.Feed)
getFeedUpdate  Nothing uri = Just <$> getFeed uri
getFeedUpdate (Just t) uri = do
  updated <- resourceExpired t uri
  if updated
    then Just <$> getFeed uri
    else return Nothing

fetchFeeds :: Maybe UTCTime -> [URI]
           -> IO ([(URI, SomeException)], [Generic.Feed])
fetchFeeds mtime urls = do
    res <- parallelE (L.map (getFeedUpdate mtime) urls)
    return $ second catMaybes $ partitionEithers $ urlfy res
  where
    urlfy = L.zipWith (\url -> either (Left .  (,) url) Right) urls


resolveComments :: RSS -> IO RSS
resolveComments = error "resolveComments"