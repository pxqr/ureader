module Main (main) where

import Control.Applicative as A
import Control.Concurrent.ParallelIO
import Control.Monad
import Data.Implicit
import Data.List as L
import Data.Maybe
import Data.Monoid
import Data.Text.IO as T
import Network.URI
import Options.Applicative
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))
import System.Directory
import System.FilePath (combine)

import UReader


data Options
   = Preview { feedURI  :: URI  }
   | Feed    { feedList :: FilePath
             , newOnly  :: Bool
             }
     deriving Show

previewParser :: Parser Options
previewParser = Preview
    <$> argument parseURI
      ( metavar "URI"
     <> help    "URI to feed"
      )

feedParser :: Implicit_ String => Parser Options
feedParser = Feed
    <$> option
      ( long    "feeds"
     <> metavar "PATH"
     <> value   param_ <> showDefault
     <> help    "Path to file with list of feed urls"
      )
    <*> switch
      ( long "new"
     <> help "Show only unread feed"
      )

optionsParser :: Implicit_ String => Parser Options
optionsParser = previewParser <|> feedParser

optionsInfo :: Implicit_ String => ParserInfo Options
optionsInfo = info (helper <*> optionsParser) modifier
  where
    modifier = fullDesc <> progDesc "" <> header ""

getDefaultFeeds :: IO FilePath
getDefaultFeeds = do
  udir <- getAppUserDataDirectory "ureader"
  createDirectoryIfMissing False udir
  let configPath = udir `combine` "feeds"
  exist <- doesFileExist configPath
  unless exist $ do
    T.writeFile configPath ""
  return configPath

parseFeedList :: String -> [URI]
parseFeedList = mapMaybe parseURI . L.lines

run :: Options -> IO ()
run Preview {..} = getRSS feedURI >>= setCurrentZone >>= print . pretty
run Feed    {..} = do
  urls  <- parseFeedList <$> Prelude.readFile feedList
  feeds <- parallel $ L.map (getRSS >=> setCurrentZone) urls
  print $ pretty $ mconcat feeds

main :: IO ()
main = getDefaultFeeds >>= (execParser optionsInfo $~) >>= run
