module UReader.Options
       ( Order (..)
       , Style (..)

       , Options (..)
       , getOptions
       ) where

import Control.Monad
import Data.Implicit
import Data.Monoid
import Network.URI
import Options.Applicative
import System.Directory
import System.FilePath

import UReader.Rendering


styleParser :: Parser Style
styleParser = Style
    <$> option
      ( long    "order"
     <> short   'o'
     <> metavar "ORDER"
     <> value NewFirst <> showDefault
     <> help    "Specifies entries order: either NewFirst or OldFirst"
      )
    <*> switch
      ( long    "description"
     <> short   'd'
     <> help    "Show auxilary info like rss version and channel title"
      )
    <*> switch
      ( long    "merge"
     <> short   'm'
     <> help    "Merge multiple feed info one by time"
      )
    <*> switch
      ( long    "unread"
     <> short   'u'
     <> help    "Show only unread feed and ignore feed will shown in future"
      )

feedListParser :: Implicit_ FilePath => Parser FilePath
feedListParser = option
   ( long    "feeds"
  <> metavar "PATH"
  <> value   param_ <> showDefault
  <> help    "Path to file with list of feed urls"
   )

feedLinkParser :: Parser URI
feedLinkParser = argument parseURI
   ( metavar "URI"
  <> help    "URI to feed"
   )

data Options
   = Preview { feedURI    :: URI  }
   | Stream  { feedList   :: FilePath }
   | Batch   { feedList   :: FilePath
             , feedStyle  :: Style
             }
     deriving (Show, Eq)

previewParser :: Parser Options
previewParser = Preview <$> feedLinkParser

previewInfo :: ParserInfo Options
previewInfo = info (helper <*> previewParser) modifier
  where
    modifier = progDesc "Show feed at specified URI"

streamParser :: Implicit_ String => Parser Options
streamParser = Stream <$> feedListParser

streamInfo :: Implicit_ String => ParserInfo Options
streamInfo = info (helper <*> streamParser) modifier
  where
    modifier = progDesc "Show stream"

feedParser :: Implicit_ String => Parser Options
feedParser = Batch <$> feedListParser <*> styleParser

feedInfo :: Implicit_ String => ParserInfo Options
feedInfo = info (helper <*> feedParser) modifier
  where
    modifier = progDesc "Show feed"

optionsParser :: Implicit_ String => Parser Options
optionsParser = subparser $ mconcat
  [ command "view"   previewInfo
  , command "stream" streamInfo
  , command "feed"   feedInfo
  ]

optionsInfo :: Implicit_ String => ParserInfo Options
optionsInfo = info (helper <*> optionsParser) modifier
  where
    modifier = fullDesc <> progDesc "" <> header ""

getDefaultFeeds :: IO FilePath
getDefaultFeeds = do
  udir <- getAppUserDataDirectory "ureader"
  createDirectoryIfMissing False udir
  let configPath = udir </> "feeds"
  exist <- doesFileExist configPath
  unless exist $ do
    writeFile configPath ""
  return configPath

getOptions :: IO Options
getOptions = getDefaultFeeds >>= (execParser optionsInfo $~)