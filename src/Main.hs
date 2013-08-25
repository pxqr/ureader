module Main (main) where

import Control.Applicative as A
import Control.Exception
import Control.Monad
import Data.Default
import Data.List as L
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import Network.URI
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>), (</>), width)
import Text.RSS.Syntax
import System.Directory
import System.FilePath ((<.>))
import System.IO

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

filterNew :: FilePath -> [RSS] -> IO [RSS]
filterNew feedList feeds = do
  lastSeen  <- getLastSeen (feedList <.> timestampExt)
  localTime <- utcToLocalTime <$> getCurrentTimeZone <*> pure lastSeen
  let isNew item = pubDate item > Just lastSeen
  let userFeeds  =  L.map (filterItems isNew) feeds
  unless (L.all emptyFeed userFeeds) $
    print $ green $ "Showed from:" <+> text (formatPubDate localTime)
  return userFeeds

previewFeed :: URI -> IO ()
previewFeed = getRSS >=> setCurrentZone >=> renderRSS def . return

showBatch :: Style -> FilePath -> [URI] -> IO ()
showBatch style @ Style {..} feedList uris = do
  (broken, feeds) <- fetchFeeds uris
  putBroken broken
  userFeed <- if newOnly then filterNew feedList feeds else return feeds
  renderRSS style =<< setCurrentZone userFeed

streamFeeds :: [URI] -> IO ()
streamFeeds = error "not implemented"

run :: Options -> IO ()
run Add     {..} = appendFile feedList $ show feedURI ++ "\n"
run Batch   {..} = getFeedList feedList >>= showBatch feedStyle feedList
run Preview {..} = previewFeed feedURI
run Stream  {..} = getFeedList feedList >>= streamFeeds

main :: IO ()
main = getOptions >>= run
