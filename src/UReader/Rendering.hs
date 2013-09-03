{-# OPTIONS -fno-warn-orphans #-}
module UReader.Rendering
       ( Order (..)
       , Style (..)
       , renderRSS
       ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Default
import Data.Function
import Data.Maybe
import Data.Monoid
import Data.List as L
import Data.List.Split as L
import Data.Set as S
import Text.HTML.TagSoup
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>), width)
import Text.RSS.Syntax
import Text.XML.Light.Types
import System.IO
import System.Console.Terminal.Size as Terminal

import UReader.RSS
import UReader.Localization


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

prettyDesc :: Bool -> RSS -> Doc
prettyDesc keepDesc
  | keepDesc  = pretty
  | otherwise
  = vsep . punctuate linebreak . L.map pretty . rssItems . rssChannel

merge :: Bool -> [RSS] -> [RSS]
merge byTime
  |   byTime  = return . mconcat
  | otherwise = id

formatOrder :: Order -> RSS -> RSS
formatOrder NewFirst = id
formatOrder OldFirst = reverseItems

formatFeeds :: Style -> [RSS] -> [Doc]
formatFeeds Style {..}
  = L.map (prettyDesc feedDesc . formatOrder feedOrder) . merge feedMerge

renderRSS :: Style -> [RSS] -> IO ()
renderRSS style feeds = do
  Window {..} <- fromMaybe (Window 80 60) <$> Terminal.size
  forM_ (formatFeeds style feeds) $
    displayIO stdout . renderPretty 0.8 width

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
  mempty = nullChannel "" ""
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
  pretty RSSItem {..} =
    (bold (magenta (pretty rssItemTitle)) </> pretty rssItemLink) <$$>
     red  (hsep $ L.map ppCategory rssItemCategories) <$$>
     indent 2 (maybe mempty
               (nest 2 . prettySoup False . extDesc) rssItemDescription) <$$>
    (maybe mempty ppComments rssItemComments) <$$>
    (green (pretty rssItemGuid))            <$$>
    (yellow (pretty rssItemPubDate) </>
      maybe mempty ppAuthor rssItemAuthor)

    where
      ppComments comments = "Comments: "  <+> pretty comments
      ppAuthor   author   = "posted by"   <+> red (pretty author)
      ppCategory category = dullblack "*"  <> pretty category


instance Pretty RSSGuid where
  pretty RSSGuid {..}
    | Just True <- rssGuidPermanentURL = "Permalink:" <+> pretty rssGuidValue
    |          otherwise               = "Link:     " <+> pretty rssGuidValue

instance Pretty RSSCategory where
  pretty RSSCategory {..} =
    dullyellow (maybe mempty text rssCategoryDomain) <>
    dullred    (hsep $ L.map pretty rssCategoryAttrs)  <>
    dullblue   (text rssCategoryValue)

instance Pretty Attr where
  pretty Attr {..} = text (show attrKey) <+> "=" <+> text attrVal

extDesc :: String -> [Tag String]
extDesc = canonicalizeTags . parseTags

{- NOTE: the findCloseTag could lead to serious performance
degradation, but this is very unlikely for HTML embedded in RSS. -}

findCloseTag :: Eq a => a -> [Tag a] -> ([Tag a], [Tag a])
findCloseTag t = go (0 :: Int) []
  where
    go _ acc []       = (acc, [])
    go n acc (x : xs) =
      case x of
        TagOpen  t' _
          | t == t'   -> go (succ n) (acc ++ [x]) xs
          | otherwise -> go n (acc ++ [x]) xs
        TagClose t'
          | t == t'   -> if n == 0
                         then (acc, xs)
                         else go (pred n) (acc ++ [x]) xs
          | otherwise -> go n (acc ++ [x]) xs
        _             -> go n (acc ++ [x]) xs

prettySoup :: Bool -> [Tag String] -> Doc
prettySoup _   []       = mempty
prettySoup raw (x : xs) = case x of
  TagText t -> f t <> prettySoup raw xs
    where
      f = if raw then text else text . L.filter isPrint

  TagOpen t attrs -> maybe err closeTag $ L.lookup t rules
    where
      rules =
        [ "p"  --> \par -> linebreak <> par <> linebreak
        , "i"  --> underline
        , "em" --> underline
        , "u"  --> underline
        , "strong" --> bold
        , "b"  --> bold
        , "tt" --> dullwhite
        , "hr" --> \body -> body <> linebreak <>
                            underline (text (L.replicate 72 ' ')) <> linebreak
        , "a"  --> \desc -> blue desc </> pretty (L.lookup "href" attrs)
        , "br" --> (linebreak <>)
        , "ul" --> id
        , "li" --> \li -> green "*" <+> li <> linebreak
        , "span" --> id
        , "code" ~-> (onwhite . black)
        , "img"  --> \desc -> blue desc </> pretty (L.lookup "src" attrs)
        , "pre"  ~-> \body -> linebreak <> align body <> linebreak

        , "h1" --> heading -- TODO heading UPPER
        , "h2" --> heading -- TODO heading UPPER
        , "h3" --> heading -- TODO heading UPPER
        , "h4" --> heading
        , "h5" --> heading
        , "h6" --> heading

        , "div" --> \body -> linebreak <> body <> linebreak
        , "blockquote" --> indent 4

        , "table" --> \body -> linebreak <> body <> linebreak
        , "tbody" --> id
        , "tr"    --> \body -> linebreak <> body <> linebreak
        , "td"    --> fill 40
        ]
        where
          a --> f = (a, f . prettySoup False)
          a ~-> f = (a, f . prettySoup True)

          heading body = linebreak <> bold (underline body) <> linebreak

      err = red ("<" <> text t <+> def_attrs <> ">") <+> prettySoup raw xs
        where
          def_attrs = hcat $ punctuate space $ L.map pattr attrs
            where pattr (n, v) = text n <> "=" <> text v

      closeTag m = m a <> prettySoup raw b
        where
          (a, b) = findCloseTag t xs

  TagClose t -> red ("</" <> text t <> ">") <+> prettySoup raw xs
  t          -> text (show t) <> prettySoup raw xs
