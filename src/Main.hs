module Main where

import Protolude
import Data.Time
import System.Process (callProcess, readProcess)
import System.Directory (doesDirectoryExist)

import Lektor
import Event
import ChaostreffScheduler


main :: IO ()
main = do
  [url] <- getArgs
  (y, m) <- currentYearMonth
  gitCheckout url
  scheduleChaostreffs y m (MonthCount 2)
  scheduleTechEvents y m (MonthCount 2)
  gitPush


gitCheckout :: [Char] -> IO ()
gitCheckout url = ifM (doesDirectoryExist "website/.git")
                     (putText "update website repo" >> callProcess "git" ["-C", "website", "pull", url])
                     (putText "clone website repo" >> callProcess "git" ["clone", url, "website"])


gitPush :: IO ()
gitPush = do
  status <- readProcess "git" ["-C", "website", "status", "-s"] ""
  when (not $ null status) $ do
    callProcess "git" ["-C", "website", "add", "-A"]
    callProcess "git" ["-C", "website", "commit", "-q", "-m", "chaostreff-scheduler"]
--  callProcess "git" ["-C", "website", "push"]

currentYearMonth :: IO (Year, Month)
currentYearMonth = do
  (y, m, _) <- toGregorian . utctDay <$> getCurrentTime
  pure (Year y, Month m)

