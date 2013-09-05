module Main (main) where

import Control.Applicative as A
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Default
import Data.List as L
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import Data.Version (showVersion)
import Network.URI
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>), (</>), width)
import Text.RSS.Syntax
import System.Directory
import System.FilePath ((<.>))
import System.IO

import Paths_ureader

import UReader.Localization
import UReader.Options
import UReader.Rendering
import UReader.RSS


parseFeedList :: String -> [URI]
parseFeedList = mapMaybe parseURI . L.lines

getFeedList :: FilePath -> IO [URI]
getFeedList feedList = parseFeedList <$> Prelude.readFile feedList


getLastSeen :: FilePath -> IO UTCTime
getLastSeen lastPath = do
    exist <- doesFileExist lastPath
    if exist
      then do
        !mLastSeen <- parsePubDate <$> Prelude.readFile lastPath
        Prelude.writeFile lastPath . formatPubDate =<< getCurrentTime
        return $ fromMaybe epochStart mLastSeen
      else do
        Prelude.writeFile lastPath . formatPubDate =<< getCurrentTime
        return epochStart
  where
    epochStart = posixSecondsToUTCTime 0

putBroken :: [(URI, SomeException)] -> IO ()
putBroken broken = do
  forM_ broken $ \(url, e) ->
    hPrint stderr $ red $ text $ show url ++ " - " ++ show e

timestampExt :: FilePath
timestampExt = "lastseen"

fetch :: Maybe UTCTime -> [URI] -> IO [RSS]
fetch t uris = do
  (broken, feeds) <- fetchFeeds t uris
  putBroken broken
  return feeds

filterNew :: FilePath -> [URI] -> IO [RSS]
filterNew feedList uris = do
  lastSeen  <- getLastSeen (feedList <.> timestampExt)
  feeds     <- fetch (Just lastSeen) uris
  let isNew item = pubDate item > Just lastSeen
  let userFeeds  =  L.map (filterItems isNew) feeds
  unless (L.all emptyFeed userFeeds) $ do
    localTime <- utcToLocalTime <$> getCurrentTimeZone <*> pure lastSeen
    print $ green $ "Showed from:" <+> text (formatPubDate localTime)
  return userFeeds

previewFeed :: URI -> IO ()
previewFeed = getRSS >=> setCurrentZone >=> renderRSS def . return

showBatch :: Style -> FilePath -> [URI] -> IO ()
showBatch style @ Style {..} feedList uris = do
  renderRSS style =<< setCurrentZone =<<
    (if newOnly then filterNew feedList else fetch Nothing) uris

streamStyle :: Style
streamStyle = Style
    { feedOrder = OldFirst
    , feedDesc  = False
    , feedMerge = True
    , newOnly   = True
    }

streamFeeds :: FilePath -> Int -> [URI] -> IO ()
streamFeeds feedList interval uris = forever $ do
    userFeed <- filterNew feedList uris
    renderRSS streamStyle =<< setCurrentZone userFeed
    threadDelay $ interval * 1000000

run :: Options -> IO ()
run Add     {..} = appendFile feedList $ show feedURI ++ "\n"
run Batch   {..} = getFeedList feedList >>= showBatch feedStyle feedList
run Preview {..} = previewFeed feedURI
run Stream  {..} = getFeedList feedList >>= streamFeeds feedList feedInterval
run Version      = putStrLn $ "ureader version " ++ showVersion version

main :: IO ()
main = getOptions >>= run
