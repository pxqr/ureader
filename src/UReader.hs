{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS -fno-warn-orphans #-}
module UReader
       ( getRSS
       , LocalZone (..), setCurrentZone
       ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString as BS
import Data.Function
import Data.Implicit
import Data.Monoid
import Data.List as L
import Data.Text.Encoding as T
import Data.Time
import Data.Time.Format
import Network.URI
import Network.HTTP
import Text.HTML.TagSoup
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))
import Text.RSS.Import
import Text.RSS.Syntax
import Text.XML.Light.Input
import System.Locale

import Debug.Trace


getRSS :: URI -> IO RSS
getRSS uri = do
  resp <- simpleHTTP $ Request uri GET [] ("" :: ByteString)
  body <- getResponseBody resp
  xml  <- maybe (throwIO $ userError "invalid XML") return $
            parseXMLDoc (T.decodeUtf8 body)
  rss  <- maybe (throwIO $ userError "invalid RSS") return $
            elementToRSS xml
  return rss

pubDateFormat :: String
pubDateFormat = "%a, %e %b %Y %H:%M:%S %Z"

parsePubDate :: String -> Maybe UTCTime
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

instance Monoid RSS where
  mempty  = nullRSS "" ""
  mappend a b = mempty
      { rssVersion = rssVersion a <> ", " <> rssVersion b
      , rssAttrs   = rssAttrs   a <> rssAttrs   b
      , rssChannel = rssChannel a <> rssChannel b
      , rssOther   = rssOther   a <> rssOther   b
      }

instance Monoid RSSChannel where
  mempty = nullChannel "" ""
  mappend a b = mempty
      { rssTitle = rssTitle a <> ", " <> rssTitle b
      , rssLink  = rssLink  a <> ", " <> rssLink  b
      , rssDescription = rssDescription a
      , rssItems = mergeBy cmpPubDate (rssItems a) (rssItems b)
      }
    where
      cmpPubDate = (>) `on` (parsePubDate <=< rssItemPubDate)

      mergeBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
      mergeBy _ [] xs = xs
      mergeBy _ xs [] = xs
      mergeBy f (x : xs) (y : ys)
        |   f x y   = x : mergeBy f xs (y : ys)
        | otherwise = y : mergeBy f (x : xs) ys

instance Pretty RSS where
  pretty RSS {..} =
    pretty rssChannel </>
    pretty ("rss version" :: String) <+> pretty rssVersion

instance Pretty RSSChannel where
  pretty RSSChannel {..} =
    pretty rssTitle </>
    pretty rssLink </>
    pretty rssDescription </>
    pretty rssPubDate <$$>
    vcat (punctuate linebreak $ L.map pretty rssItems)


instance Pretty RSSItem where
  pretty RSSItem {..} =
    (blue (pretty rssItemTitle) </> pretty rssItemLink) <$$>
     nest 4 (maybe mempty (prettySoup . extDesc) rssItemDescription) <$$>
    (pretty rssItemPubDate </> pretty rssItemAuthor)

instance Pretty RSSGuid where
  pretty RSSGuid {..} =
    pretty rssGuidPermanentURL </>
    pretty rssGuidValue


extDesc :: String -> [Tag String]
extDesc = canonicalizeTags . parseTags

prettySoup :: [Tag String] -> Doc
prettySoup []       = mempty
prettySoup (x : xs) = case x of
  TagText t -> text t <> prettySoup xs
  TagOpen t _
    | t == "p"  -> linebreak <> prettySoup xs
    | t == "i"  -> let (a, b) = L.break (== TagClose "i") xs
                   in underline (prettySoup a) <> prettySoup b
    | t == "b"  -> let (a, b) = L.break (== TagClose "b") xs
                   in bold (prettySoup a) <> prettySoup b
    | otherwise -> red (text t) <> prettySoup xs
  TagClose t -> red (text t) <> prettySoup xs
  t          -> text (show t) <> prettySoup xs
