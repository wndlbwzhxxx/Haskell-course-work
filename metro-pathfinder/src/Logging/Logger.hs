module Logging.Logger (logMsg, readLog, clearLog) where

import Graph.Types
import System.Directory (doesFileExist, getFileSize)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale, utcToLocalZonedTime, getCurrentTimeZone)
import Control.Monad (when)

logFile :: FilePath
logFile = "app.log"

maxSize :: Integer
maxSize = 10 * 1024 * 1024

logMsg :: LogLevel -> String -> IO ()
logMsg level msg = do
  now <- getCurrentTime
  timezone <- getCurrentTimeZone
  localTime <- utcToLocalZonedTime now
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" localTime
  let entry = timestamp ++ " [" ++ show level ++ "] " ++ msg ++ "\n"
  exists <- doesFileExist logFile
  if exists
    then do
      size <- getFileSize logFile
      when (size > maxSize) $ do
        writeFile (logFile ++ ".old") =<< readFile logFile
        writeFile logFile ""
    else return ()
  appendFile logFile entry

readLog :: IO [String]
readLog = do
  exists <- doesFileExist logFile
  if exists
    then lines <$> readFile logFile
    else return []

clearLog :: IO ()
clearLog = writeFile logFile ""