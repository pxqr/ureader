-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This module provides colored (ansi-terminal compatible) rendering
--   for OPML, RSS, (Atom not yet) and HTML.
--
{-# OPTIONS -fno-warn-orphans #-}
module UReader.Rendering
       ( Order (..)
       , Style (..)
       , renderFeed
       , renderFeedList
       ) where

import Control.Applicative
import Control.Monad
import Data.Default
import Data.Function
import Data.Maybe
import Data.Monoid
import Data.List as L
import Data.List.Split as L
import Data.Set as S
import Text.OPML.Syntax
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>), width)
import Text.RSS1.Syntax as RSS1
import Text.RSS.Syntax  as RSS
import Text.Feed.Types  as Generic
import Text.XML.Light.Types
import Network.URI
import System.IO
import System.Console.Terminal.Size as Terminal

import UReader.Feed
import UReader.Localization
import UReader.Outline
import UReader.Rendering.HTML
import UReader.Rendering.Feed.Atom ()

{-----------------------------------------------------------------------
  Feed list
-----------------------------------------------------------------------}

renderFeedList :: OPML -> IO ()
renderFeedList = print . pretty

instance Pretty Outline where
  pretty Outline  {..} =
    fill 28 (topicTy (pretty opmlText))
                      <+> ppURI opmlOutlineAttrs <>
      if L.null opmlOutlineChildren then mempty else linebreak <>
        indent 4 (vsep $ L.map pretty opmlOutlineChildren)
--    , "type" <+> pretty opmlType
--    , "categories" <+> pretty opmlCategories
--    , "comment"    <+> pretty opmlIsComment
--    , "breakpoint" <+> pretty opmlIsBreakpoint
--    , "other"      <+> pretty (show opmlOutlineOther)
    where
      topicTy
        | L.null opmlOutlineChildren = blue    . underline
        |         otherwise          = magenta . bold

      ppURI (lookupAttr uriQName -> Just uriStr)
        | Just uri <- parseURI uriStr = "<" <> text (show uri) <> ">"
        |         otherwise           = red "invalid URL"
      ppURI _ = mempty

instance Pretty OPMLHead where
  pretty OPMLHead {..} = pretty opmlTitle

instance Pretty OPML where
  pretty OPML {..}
    = --"version" <+> text opmlVersion </>
      --"head   " <+> pretty opmlHead  </>
      vsep (L.map pretty opmlBody)

{-----------------------------------------------------------------------
  Feed
-----------------------------------------------------------------------}

data Order = NewFirst
           | OldFirst
             deriving (Show, Read, Eq, Ord, Bounded, Enum)

data Style = Style
  { feedOrder :: !Order
  , feedDesc  :: !Bool
  , feedMerge :: !Bool
  , newOnly   :: !Bool
  } deriving (Show, Eq)

instance Default Style where
  def = Style
    { feedOrder = NewFirst
    , feedDesc  = True
    , feedMerge = False
    , newOnly   = False
    }

prettyDesc :: Bool -> Generic.Feed -> Doc
prettyDesc keepDesc
    | keepDesc  = pretty
    | otherwise = vsep . punctuate linebreak . prettyChannel

merge :: Bool -> [Generic.Feed] -> [Generic.Feed]
merge byTime
  |   byTime  = return . mconcat
  | otherwise = id

formatOrder :: Order -> Generic.Feed -> Generic.Feed
formatOrder NewFirst = id
formatOrder OldFirst = reverseItems

formatFeeds :: Style -> [Generic.Feed] -> Doc
formatFeeds Style {..}
  = vsep . punctuate linebreak
  . L.map (prettyDesc feedDesc . formatOrder feedOrder) . merge feedMerge

renderFeed :: Style -> [Generic.Feed] -> IO ()
renderFeed style feeds = do
  Window {..} <- fromMaybe (Window 80 60) <$> Terminal.size
  displayIO stdout $ renderPretty 0.8 width $ formatFeeds style feeds

instance Monoid RSS where
  mempty  = nullRSS "" ""
  mappend a b = mempty
      { rssVersion = unwords $ S.toList $
                     mergeVersions (rssVersion a) (rssVersion b)
      , rssAttrs   = rssAttrs   a <> rssAttrs   b
      , rssChannel = rssChannel a <> rssChannel b
      , rssOther   = rssOther   a <> rssOther   b
      }
    where
      mergeVersions = (<>) `on` S.fromList . words

instance Monoid RSSChannel where
  mempty = RSS.nullChannel "" ""
  mappend a b = mempty
      { rssTitle = rssTitle a <> "|" <> rssTitle b
      , rssLink  = rssLink  a <> " " <> rssLink  b
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

instance Monoid Generic.Feed where

{-----------------------------------------------------------------------
-- RSS2
-----------------------------------------------------------------------}

instance Pretty RSS where
  pretty RSS {..} =
    pretty rssChannel </>
    dullblack ("rss version" <+> pretty rssVersion)

instance Pretty RSSChannel where
  pretty RSSChannel {..} =
      vcat (L.zipWith heading (splitOn "|" rssTitle) (words rssLink)) </>
      pretty rssDescription </>
      pretty rssPubDate <$$>
      vcat (punctuate linebreak $ L.map pretty rssItems)
    where
      heading title link = blue (fill 24 (text title)) </> text link

instance Pretty RSSItem where
  pretty RSS.RSSItem {..} =
    (bold (magenta (pretty rssItemTitle)) </> pretty rssItemLink) <$$>
     red  (hsep $ L.map ppCategory rssItemCategories) <$$>
       indent 2 (maybe mempty ppItemDesc rssItemDescription) <$$>
    (maybe mempty ppComments rssItemComments) <$$>
    (green (pretty rssItemGuid))            <$$>
    (yellow (pretty rssItemPubDate) </>
      maybe mempty ppAuthor rssItemAuthor)
    where
      ppItemDesc          = nest 2 . prettyHTML
      ppComments comments = "Comments: "  <+> pretty comments
      ppAuthor   author   = "posted by"   <+> red (pretty author)
      ppCategory category = dullblack "*"  <> pretty category


instance Pretty RSSGuid where
  pretty RSSGuid {..}
    | Just True <- rssGuidPermanentURL = "Permalink:" <+> pretty rssGuidValue
    |          otherwise               = "Link:     " <+> pretty rssGuidValue

instance Pretty RSSCategory where
  pretty RSSCategory {..} =
    dullyellow (maybe mempty text rssCategoryDomain)  <>
    dullred    (hsep $ L.map pretty rssCategoryAttrs) <>
    dullblue   (text rssCategoryValue)

instance Pretty Attr where
  pretty Attr {..} = text (show attrKey) <+> "=" <+> text attrVal

{-----------------------------------------------------------------------
-- RSS1
-----------------------------------------------------------------------}

instance Pretty RSS1.Feed where
  pretty _ = red (text "RSS1 renderer not implemented")

{-----------------------------------------------------------------------
-- Generic
-----------------------------------------------------------------------}

instance Pretty Generic.Feed where
  pretty (AtomFeed a) = pretty a
  pretty (RSSFeed  r) = pretty r
  pretty (RSS1Feed r) = pretty r
  pretty (XMLFeed  e) = red (text "XML")

-- TODO
prettyChannel :: Generic.Feed -> [Doc]
prettyChannel (AtomFeed a) = [pretty a]
prettyChannel (RSSFeed  r) = L.map pretty . rssItems . rssChannel $ r
prettyChannel (RSS1Feed r) = [pretty r]
prettyChannel (XMLFeed  e) = [red (text "XML")]
