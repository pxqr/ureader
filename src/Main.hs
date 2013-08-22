module Main (main) where

import Control.Applicative as A
import Control.Concurrent.ParallelIO
import Control.Monad
import Data.Either
import Data.Implicit
import Data.List as L
import Data.Maybe
import Data.Monoid
import Data.Text.IO as T
import Data.Time
import Data.Time.Clock.POSIX
import Network.URI
import Options.Applicative
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>), (</>), width)
import Text.Read
import System.Directory
import System.FilePath ((</>), (<.>))
import System.IO

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
  let configPath = udir </> "feeds"
  exist <- doesFileExist configPath
  unless exist $ do
    T.writeFile configPath ""
  return configPath

parseFeedList :: String -> [URI]
parseFeedList = mapMaybe parseURI . L.lines

getLastSeen :: FilePath -> IO UTCTime
getLastSeen lastPath = do
    exist <- doesFileExist lastPath
    if exist
      then do
        !mLastSeen <- readMaybe <$> Prelude.readFile lastPath
        Prelude.writeFile lastPath . show =<< getCurrentTime
        return $ fromMaybe epochStart mLastSeen
      else do
        Prelude.writeFile lastPath . show =<< getCurrentTime
        return epochStart
  where
    epochStart = posixSecondsToUTCTime 0

run :: Options -> IO ()
run Preview {..} = getRSS feedURI >>= setCurrentZone >>= renderRSS
run Feed    {..} = do
  urls  <- parseFeedList <$> Prelude.readFile feedList
  res   <- parallelE $ L.map getRSS urls
  let urlfy = L.zipWith (\url -> either (Left .  (,) url) Right) urls
  let (broken, feeds) = partitionEithers $ urlfy res

  forM_ broken $ \(url, e) ->
    hPrint stderr $ red $ text $ show url ++ " - " ++ show e

  userFeed <- if newOnly
    then do
      lastSeen  <- getLastSeen (feedList <.> "lastseen")
      localTime <- utcToLocalTime <$> getCurrentTimeZone <*> pure lastSeen
      print $ green $ "Showed from:" <+> text (formatPubDate localTime)
      let isNew item = pubDate item > Just lastSeen
      return $ L.map (filterItems isNew) feeds
    else return feeds

  renderRSS . mconcat =<< setCurrentZone userFeed

main :: IO ()
main = getDefaultFeeds >>= (execParser optionsInfo $~) >>= run
