module Main (main) where

import Control.Applicative as A
import Control.Concurrent.ParallelIO
import Control.Monad
import Data.Either
import Data.List as L
import Data.Maybe
import Data.Monoid
import Data.Time
import Data.Time.Clock.POSIX
import Network.URI
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>), (</>), width)
import Text.Read
import System.Directory
import System.FilePath ((<.>))
import System.IO

import UReader
import UReader.Localization
import UReader.Options
import UReader.RSS


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
run Batch   {..} = do
  let Style {..} = feedStyle
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

run opt = error $ "unsupported option" ++ show opt

main :: IO ()
main = getOptions >>= run
