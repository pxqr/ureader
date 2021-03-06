module Main (main) where

import Prelude as P

import Control.Applicative as A
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Default
import Data.List as L
import Data.Maybe
import Data.Monoid
import Data.Time
import Data.Time.Clock.POSIX
import Data.Version (showVersion)
import Network.URI
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>), (</>), width)
import Text.Feed.Types as Generic
import System.Directory
import System.FilePath ((<.>))
import System.IO

import Paths_ureader

import UReader.Localization
import UReader.Options
import UReader.Outline
import UReader.Rendering
import UReader.Feed


getLastSeen :: FilePath -> IO UTCTime
getLastSeen lastPath = do
    exist <- doesFileExist lastPath
    if exist
      then do
        !mLastSeen <- parsePubDate <$> P.readFile lastPath
        P.writeFile lastPath . formatPubDate =<< getCurrentTime
        return $ fromMaybe epochStart mLastSeen
      else do
        P.writeFile lastPath . formatPubDate =<< getCurrentTime
        return epochStart
  where
    epochStart = posixSecondsToUTCTime 0

putBroken :: [(URI, SomeException)] -> IO ()
putBroken broken = do
  forM_ broken $ \(url, e) ->
    hPrint stderr $ red $ text $ show url ++ " - " ++ show e

timestampExt :: FilePath
timestampExt = "lastseen"

fetch :: Maybe UTCTime -> [URI] -> IO [Feed]
fetch t uris = do
  (broken, feeds) <- fetchFeeds t uris
  putBroken broken
  return feeds

filterNew :: FilePath -> [URI] -> IO [Feed]
filterNew feedList uris = do
  lastSeen  <- getLastSeen (feedList <.> timestampExt)
  feeds     <- fetch (Just lastSeen) uris
  let userFeeds  = L.map (filterItems (isNew lastSeen)) feeds
  unless (L.all emptyFeed userFeeds) $ do
    localTime <- utcToLocalTime <$> getCurrentTimeZone <*> pure lastSeen
    print $ green $ linebreak <>
      "Showed from:" <+> text (formatPubDate localTime)
  return userFeeds

previewFeed :: URI -> IO ()
previewFeed = getFeed >=> setCurrentZone >=> renderFeed def . return

showBatch :: Style -> FilePath -> [URI] -> IO ()
showBatch style @ Style {..} feedList uris = do
  renderFeed style =<< setCurrentZone =<<
    (if newOnly then filterNew feedList else fetch Nothing) uris
  putStrLn ([] :: String)

streamStyle :: Style
streamStyle = Style
    { feedOrder = OldFirst
    , feedDesc  = False
    , feedMerge = True
    , newOnly   = True
    }

updateStream :: FilePath -> [URI] -> IO ()
updateStream feedList uris
  = filterNew feedList uris >>= setCurrentZone >>= renderFeed streamStyle

pollBy :: Int -> IO () -> IO ()
pollBy interval action = forever $ do
    end <- async $ handle handler action
    threadDelay $ interval * 1000000
    wait end
  where
    handler :: SomeException -> IO ()
    handler = print

streamFeeds :: FilePath -> Int -> [URI] -> IO ()
streamFeeds feedList interval uris =
  pollBy interval $ do updateStream feedList uris

addFeed :: FilePath -> URI -> String -> String -> IO ()
addFeed filePath uri grp topic
  = modifyOPML filePath (insertURI [grp, topic] uri)

putVersion :: IO ()
putVersion = P.putStrLn $ "ureader version " ++ showVersion version

run :: Options -> IO ()
run Add     {..} = addFeed      feedList   feedURI
                                feedParent feedTopic
run Batch   {..} = getFeedList  feedList  feedGroup
               >>= showBatch    feedStyle feedList
run Index   {..} = getIndex     feedList  feedGroup
               >>= renderFeedList
run Preview {..} = previewFeed  feedURI
run Stream  {..} = getFeedList  feedList feedGroup
               >>= streamFeeds  feedList feedInterval
run Version      = putVersion

main :: IO ()
main = getOptions >>= run
