{-# OPTIONS -fno-warn-orphans #-}
module UReader
       ( getRSS
       ) where

import Control.Exception
import Data.ByteString as BS
import Data.Function
import Data.Monoid
import Data.List as L
import Data.Text.Encoding as T
import Network.URI
import Network.HTTP
import Text.HTML.TagSoup
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))
import Text.RSS.Import
import Text.RSS.Syntax
import Text.XML.Light.Input


getRSS :: URI -> IO RSS
getRSS uri = do
  resp <- simpleHTTP $ Request uri GET [] ("" :: ByteString)
  body <- getResponseBody resp
  xml  <- maybe (throwIO $ userError "invalid XML") return $
            parseXMLDoc (T.decodeUtf8 body)
  rss  <- maybe (throwIO $ userError "invalid RSS") return $
            elementToRSS xml
  return rss

instance Monoid RSS where
  mempty  = nullRSS "" ""
  mappend a b = mempty
      { rssVersion = rssVersion a <> ", " <> rssVersion b
      , rssAttrs   = rssAttrs   a <> rssAttrs   b
      , rssChannel = rssChannel a <> rssChannel b
      , rssOther   = rssOther   a <> rssOther   b
      }

mergeBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
mergeBy _ [] xs = xs
mergeBy _ xs [] = xs
mergeBy f (x : xs) (y : ys)
  |   f x y   = x : mergeBy f xs (y : ys)
  | otherwise = y : mergeBy f (x : xs) ys

instance Monoid RSSChannel where
  mempty = nullChannel "" ""
  mappend a b = mempty
      { rssTitle = rssTitle a <> ", " <> rssTitle b
      , rssLink  = rssLink  a <> ", " <> rssLink  b
      , rssDescription = rssDescription a
      , rssItems = mergeBy cmpPubDate (rssItems a) (rssItems b)
      }
    where
      cmpPubDate = const (const True) `on` rssItemPubDate

instance Pretty RSS where
  pretty RSS {..} =
    pretty rssChannel </>
    pretty ("rss version" :: String) <+> pretty rssVersion

instance Pretty RSSChannel where
  pretty RSSChannel {..} =
    pretty rssTitle </>
    pretty rssLink </>
    pretty rssDescription </>
    pretty rssPubDate </>
    vsep (L.map pretty rssItems)


instance Pretty RSSItem where
  pretty RSSItem {..} =
    blue (pretty rssItemTitle) </>
    pretty rssItemLink  </>
    maybe mempty (prettySoup . extDesc) rssItemDescription </>
    pretty rssItemAuthor </>
    pretty rssItemPubDate

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
    | t == "i"  -> underline (prettySoup a) <> prettySoup b
    | otherwise -> prettySoup xs
    where (a, b) = L.break (== TagClose "i") xs
  TagClose _ -> prettySoup xs
  t          -> text (show t) <> prettySoup xs
