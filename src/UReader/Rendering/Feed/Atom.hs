{-# OPTIONS -fno-warn-orphans #-}
module UReader.Rendering.Feed.Atom where

import Data.List as L
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))
import Text.Atom.Feed

import UReader.Rendering.HTML
import UReader.Rendering.XHTML


instance Pretty TextContent where
  pretty (TextString  s) = text s
  pretty (HTMLString  h) = prettyHTML  h
  pretty (XHTMLString h) = prettyXHTML h

instance Pretty Person where
  pretty Person {..} = text personName

instance Pretty Category where
  pretty Category {..} = text catTerm

instance Pretty Generator where
  pretty Generator {..}
    = pretty genURI     </>
      pretty genVersion </>
      pretty genText

instance Pretty Link where
  pretty Link {..} = text linkHref

instance Pretty EntryContent where
  pretty (TextContent  s)      = text s
  pretty (HTMLContent  s)      = prettyHTML s
  pretty (XHTMLContent e)      = prettyXHTML e
  pretty (MixedContent    _ c) = text (show c)
  pretty (ExternalContent _ u) = text (show u)

instance Pretty Source where
  pretty Source {..} = pretty sourceId

instance Pretty Entry where
  pretty Entry {..}
    = magenta (pretty entryTitle) </> text entryId <$$>
      ppCats entryCategories  <$$>
      yellow (pretty entryContent)   <$$>
      ppLinks entryLinks             </>
--      blue (pretty entryPublished)   <$$>
--      red  (pretty entryRights)      </>
      maybe empty pretty entrySource <$$>
      yellow (text   entryUpdated)  </> ppAuthors entryAuthors
    where
      ppCats    = hsep . punctuate "," . L.map (blue . pretty)
      ppLinks   = hsep . punctuate "," . L.map pretty
      ppAuthors = hsep . punctuate "," . L.map (red . pretty)

instance Pretty Feed where
  pretty f @ Feed {..}
    = text   feedId           </>
      pretty feedTitle        </>
      text   feedUpdated      </>
      pretty feedAuthors      </>
      pretty feedCategories   </>
      pretty feedContributors </>
      pretty feedGenerator    </>
      pretty feedIcon         </>
      pretty feedLinks        </>
      vcat (punctuate linebreak (L.map pretty feedEntries))
