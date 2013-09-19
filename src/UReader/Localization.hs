-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This module provides simple localization for feed items.
--   Currently an UTC time (pubdate) translated to the LocalTime which
--   user expect to see.
--
--   +0000(UTC) -> +0004(MSK)
--
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module UReader.Localization
       ( pubDate
       , parsePubDate
       , formatPubDate

       , LocalZone (..)
       , setCurrentZone
       ) where

import Control.Applicative
import Control.Monad
import Data.Implicit
import Data.Time
import Text.Atom.Feed   as Atom
import Text.RSS.Syntax  as RSS2
import Text.RSS1.Syntax as RSS1
import System.Locale

pubDate :: RSSItem -> Maybe UTCTime
pubDate = parsePubDate <=< rssItemPubDate

pubDateFormat :: String
pubDateFormat = "%a, %e %b %Y %H:%M:%S %Z"

parsePubDate :: DateString -> Maybe UTCTime
parsePubDate = parseTime defaultTimeLocale pubDateFormat

formatPubDate :: FormatTime t => t -> DateString
formatPubDate = formatTime defaultTimeLocale pubDateFormat

localizeUTC :: Implicit_ TimeZone => UTCTime -> LocalTime
localizeUTC = utcToLocalTime param_

setCurrentZone :: LocalZone a => a -> IO a
setCurrentZone x = (localize x $~) <$> getCurrentTimeZone

class LocalZone a where
  localize :: Implicit_ TimeZone => a -> a

instance (Functor f, LocalZone a) => LocalZone (f a) where
  localize = fmap localize

instance LocalZone DateString where
  localize ds = maybe ds (formatPubDate . localizeUTC) $ parsePubDate ds

{-----------------------------------------------------------------------
-- RSS1
-----------------------------------------------------------------------}

instance LocalZone RSS1.Channel where
  localize ch @ RSS1.Channel {..}
    = ch { channelUpdateBase = localize channelUpdateBase }

instance LocalZone RSS1.Feed where
  localize rss @ RSS1.Feed {..} =
    rss { feedChannel = localize feedChannel }

{-----------------------------------------------------------------------
-- RSS2
-----------------------------------------------------------------------}

instance LocalZone RSSItem where
  localize item = item { rssItemPubDate = localize (rssItemPubDate item) }

instance LocalZone RSSChannel where
  localize chan @ RSSChannel {..} = chan
    { rssPubDate    = localize rssPubDate
    , rssLastUpdate = localize rssLastUpdate
    , rssItems      = fmap localize rssItems
    }

instance LocalZone RSS where
  localize rss @ RSS {..} = rss { rssChannel = localize rssChannel }

{-----------------------------------------------------------------------
-- Atom
-----------------------------------------------------------------------}

instance LocalZone Source where
  localize source @ Atom.Source {..}
    = source { sourceUpdated = localize sourceUpdated }

instance LocalZone Entry where
  localize entry @ Atom.Entry {..}
    = entry { entryUpdated   = localize entryUpdated
            , entryPublished = localize entryPublished
            , entrySource    = localize entrySource
            }

instance LocalZone Atom.Feed where
  localize atom @ Atom.Feed {..}
    = atom { feedUpdated = localize feedUpdated
           , feedEntries = localize feedEntries }