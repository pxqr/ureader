module UReader.Outline
       ( uriQName
       , lookupAttr
       , extractURIs
       , getFeedList
       , lookupGroup
       ) where

import Prelude as P

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Char
import Data.Function
import Data.Maybe
import Data.List as L
import Network.URI
import Text.OPML.Syntax
import Text.OPML.Reader
import Text.XML.Light.Types


uriQName :: String
uriQName = "xmlUrl"

lookupAttr :: String -> [Attr] -> Maybe String
lookupAttr _     [] = Nothing
lookupAttr qname (Attr {..} : xs)
  | qName attrKey == qname = Just attrVal
  |         otherwise      = lookupAttr qname xs

extractURIs :: OPML -> [URI]
extractURIs OPML {..} = mapMaybe getURI $ flattenMany opmlBody
  where
    flattenMany          = L.concatMap flatten
    flatten Outline {..} = opmlOutlineAttrs : flattenMany opmlOutlineChildren

    getURI = lookupAttr uriQName >=> parseURI

getFeedList :: FilePath -> IO [URI]
getFeedList feedList = do
  str <- P.readFile feedList
  case parseOPMLString str of
    Nothing -> throwIO $ userError "unable to parse feed list"
    Just us -> return $ extractURIs us

lookupGroup :: String -> OPML -> Maybe OPML
lookupGroup gid opml @ OPML {..}
    | Just grp <- go opmlBody = Just opml { opmlBody = grp }
    |        otherwise        = Nothing
  where
    go [] = Nothing
    go (Outline {..} : xs)
      | L.map toLower opmlText == L.map toLower gid = Just opmlOutlineChildren
      |                     otherwise               = go xs

findModify :: (a -> Bool) -> (a -> a) -> [a] -> Maybe [a]
findModify p f = go
  where
    go [] = Nothing
    go (x : xs)
      |    p x    = Just (f x : xs)
      | otherwise = (x :) <$> go xs
{-
removeSubtree :: [String] -> OPML -> Either String OPML
removeSubtree key doc @ OPML {..} = case go opmlBody key of
    Right body' -> Right $ doc { opmlBody = body' }
    Left  e     -> Left e
  where
    go os []  = Left "empty key"
    go os [k] = Right $ deleteBy ((==) `on` opmlText) (nullOutline k) os
    go os (k : ks)
      | Just a <- findModify ((==) k . opmlText) (`go` ks) os = undefined
      | otherwise = undefined
-}