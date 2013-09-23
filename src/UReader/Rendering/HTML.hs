-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This module provides rendering of HTML subset frequently used in
--   RSS.
--
module UReader.Rendering.HTML
       ( prettyHTML
       ) where

import Data.Char
import Data.List as L
import Data.Monoid
import Text.HTML.TagSoup
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))


importSoup :: String -> [Tag String]
importSoup = canonicalizeTags . parseTags

{- NOTE: the findCloseTag could lead to serious performance
degradation, but this is very unlikely for HTML embedded in RSS. -}

findCloseTag :: Eq a => a -> [Tag a] -> ([Tag a], [Tag a])
findCloseTag t = go (0 :: Int) []
  where
    go _ acc []       = (reverse acc, [])
    go n acc (x : xs) =
      case x of
        TagOpen  t' _
          | t == t'   -> go (succ n) (x : acc) xs
          | otherwise -> go       n  (x : acc) xs
        TagClose t'
          | t == t'   -> if n == 0
                    then (reverse acc, xs)
                    else go (pred n) (x : acc) xs
          | otherwise -> go       n  (x : acc) xs
        _             -> go       n  (x : acc) xs

prettySoup :: Bool -> Bool -> [Tag String] -> Doc
prettySoup _     _   []       = mempty
prettySoup upper raw (x : xs) = case x of
  TagText t -> text (upperize (canonicalize t))
            <> prettySoup upper raw xs
    where
      canonicalize |    raw    = id
                   | otherwise = L.filter isPrint
      upperize     |   upper   = L.map toUpper
                   | otherwise = id

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
        , "ol" --> id
        , "li" --> \li -> green "*" <+> li <> linebreak

        , "aside" --> id
        , "cite" --> bold
        , "span" --> id
        , "code" ~-> (onwhite . black)
        , "img"  --> \desc -> blue desc </> pretty (L.lookup "src" attrs)
        , "pre"  ~-> \body -> linebreak <> align body <> linebreak

        , "h1" ==> heading
        , "h2" --> heading
        , "h3" --> heading
        , "h4" --> heading
        , "h5" --> heading
        , "h6" --> heading
        , "small" --> dullwhite

        , "div" --> \body -> linebreak <> body <> linebreak
        , "blockquote" --> indent 4

        , "table" --> \body -> linebreak <> body <> linebreak
        , "tbody" --> id
        , "tr"    --> \body -> linebreak <> body <> linebreak
        , "td"    --> fill 40

        , "dl" --> id
        , "dt" --> (<> linebreak)
        , "dd" --> ("        " <>)

        , "sub" --> ("_" <>)
        , "sup" --> ("^" <>)
        ]
        where
          a --> f = (a, f . prettySoup False False)
          a ~-> f = (a, f . prettySoup False True)
          a ==> f = (a, f . prettySoup True  False)

          heading body = linebreak <> bold (underline body) <> linebreak

      err = red ("<" <> text t <+> def_attrs <> ">")
        <+> prettySoup upper raw xs
        where
          def_attrs = hcat $ punctuate space $ L.map pattr attrs
            where pattr (n, v) = text n <> "=" <> text v

      closeTag m = m a <> prettySoup upper raw b
        where
          (a, b) = findCloseTag t xs

  TagClose t -> red ("</" <> text t <> ">") <+> prettySoup upper raw xs
  t          -> text (show t) <> prettySoup upper raw xs

prettyHTML :: String -> Doc
prettyHTML = prettySoup False False . importSoup
