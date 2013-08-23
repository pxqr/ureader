{-# OPTIONS -fno-warn-orphans #-}
module UReader.Rendering ( renderRSS ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Function
import Data.Maybe
import Data.Monoid
import Data.List as L
import Data.List.Split as L
import Data.Set as S
import Text.HTML.TagSoup
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>), width)
import Text.RSS.Syntax
import System.IO
import System.Console.Terminal.Size as Terminal

import UReader.Localization


renderRSS :: Bool -> RSS -> IO ()
renderRSS desc feed = do
  Window {..} <- fromMaybe (Window 80 60) <$> Terminal.size
  let pp = vcat . punctuate linebreak . L.map pretty . rssItems . rssChannel
  let doc = if desc then pretty feed else pp feed
  displayIO stdout $ renderPretty 0.8 width doc
  Prelude.putStrLn ("" :: String)

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
     indent 2 (maybe mempty
               (nest 2 . prettySoup False . extDesc) rssItemDescription) <$$>
    (green (pretty rssItemGuid)) <$$>
    (yellow (pretty rssItemPubDate) </> red (pretty rssItemAuthor))

instance Pretty RSSGuid where
  pretty RSSGuid {..}
    | Just True <- rssGuidPermanentURL = "Permalink: " <+> pretty rssGuidValue
    |          otherwise               = pretty rssGuidValue



extDesc :: String -> [Tag String]
extDesc = canonicalizeTags . parseTags

prettySoup :: Bool -> [Tag String] -> Doc
prettySoup _   []       = mempty
prettySoup raw (x : xs) = case x of
  TagText t -> f t <> prettySoup raw xs
    where
      f = if raw then text else text . L.filter isPrint

  TagOpen t attrs -> maybe def closeTag $ L.lookup t rules
    where
      rules =
        [ "p"  --> \par -> linebreak <> par <> linebreak
        , "i"  --> underline
        , "em" --> underline
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

        , "h1" --> heading
        , "h2" --> heading
        , "h3" --> heading
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

      def = red ("<" <> text t <+> def_attrs <> ">") <+> prettySoup raw xs
        where
          def_attrs = hcat $ punctuate space $ L.map pattr attrs
            where pattr (n, v) = text n <> "=" <> text v

      closeTag m = m a <> prettySoup raw (L.drop 1 b)
        where
          (a, b) = L.break (== TagClose t) xs

  TagClose t -> red ("</" <> text t <> ">") <+> prettySoup raw xs
  t          -> text (show t) <> prettySoup raw xs
