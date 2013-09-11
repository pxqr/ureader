module UReader.Options
       ( Order (..)
       , Style (..)
       , Selector
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
     <> help    "Include auxilary info like rss version and channel title"
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
feedListParser = strOption
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

intervalTime :: Parser Int
intervalTime = option
   ( long    "interval"
  <> short   'i'
  <> metavar "SECONDS"
  <> value   600 <> showDefault
  <> help    "Minimal interval between feed updates"
   )

type Selector = Maybe String

feedGroupParser :: Parser Selector
feedGroupParser = optional $ argument Just
   ( metavar "GROUP"
  <> help    "Feed group used to specify some subset of feeds"
   )

data Options
   = Add     { feedList   :: FilePath
             , feedURI    :: URI
             }
   | Batch   { feedList   :: FilePath
             , feedStyle  :: Style
             }
   | Preview { feedURI    :: URI  }
   | Stream  { feedList   :: FilePath
             , feedInterval :: Int
             }
   | Index   { feedList   :: FilePath
             , feedGroup  :: Selector
             }
   | Version
     deriving (Show, Eq)

addParser :: Implicit_ String => Parser Options
addParser = Add <$> feedListParser <*> feedLinkParser

addInfo :: Implicit_ String => ParserInfo Options
addInfo = info (helper <*> addParser) modifier
  where
    modifier = progDesc "Add a feed to the feed list"

streamParser :: Implicit_ String => Parser Options
streamParser = Stream <$> feedListParser <*> intervalTime

streamInfo :: Implicit_ String => ParserInfo Options
streamInfo = info (helper <*> streamParser) modifier
  where
    modifier = progDesc "Show feed as never ending stream"

feedParser :: Implicit_ String => Parser Options
feedParser = Batch <$> feedListParser <*> styleParser

feedInfo :: Implicit_ String => ParserInfo Options
feedInfo = info (helper <*> feedParser) modifier
  where
    modifier = progDesc "Show all feeds specified in feed list"

previewParser :: Parser Options
previewParser = Preview <$> feedLinkParser

previewInfo :: ParserInfo Options
previewInfo = info (helper <*> previewParser) modifier
  where
    modifier = progDesc "Show feed specified by URI or file path"

indexParser :: Implicit_ FilePath => Parser Options
indexParser = Index <$> feedListParser <*> feedGroupParser

indexInfo :: Implicit_ FilePath => ParserInfo Options
indexInfo = info (helper <*> indexParser) modifier
  where
    modifier = progDesc "Show your feed list"

versionParser :: Parser Options
versionParser = flag' Version
    ( long  "version"
   <> short 'V'
   <> hidden
   <> help  "Show program version and exit"
    )

optionsParser :: Implicit_ String => Parser Options
optionsParser = subparser $ mconcat
  [ command "add"    addInfo
  , command "feed"   feedInfo
  , command "index"  indexInfo
  , command "stream" streamInfo
  , command "view"   previewInfo
  ]

optionsInfo :: Implicit_ String => ParserInfo Options
optionsInfo = info (helper <*> (versionParser <|> optionsParser)) modifier
  where
    modifier = fullDesc <> header hdr <> progDesc desc
    hdr  = "ureader is minimalistic CLI RSS reader"
    desc =
       "$ ureader COMMAND --help # for info about particular command"

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