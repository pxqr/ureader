module UReader.Rendering.XHTML
       ( prettyXHTML
       ) where

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))
import Text.XML.Light


prettyXHTML :: Element -> Doc
prettyXHTML _ = error "prettyXHTML: not implemented"