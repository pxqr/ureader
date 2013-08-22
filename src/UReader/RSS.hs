module UReader.RSS
       ( getRSS
       , filterItems
       ) where

import Control.Exception
import Data.ByteString as BS
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


getRSS :: URI -> IO RSS
getRSS uri = do
  resp <- simpleHTTP $ Request uri GET [] ("" :: ByteString)
  body <- getResponseBody resp
  xml  <- maybe (throwIO $ userError "invalid XML") return $
            parseXMLDoc (T.decodeUtf8 body)
  rss  <- maybe (throwIO $ userError "invalid RSS") return $
            elementToRSS xml
  return rss
