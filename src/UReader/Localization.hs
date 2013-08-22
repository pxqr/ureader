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
import Text.RSS.Syntax
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
