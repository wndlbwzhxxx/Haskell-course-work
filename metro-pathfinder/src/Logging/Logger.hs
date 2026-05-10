{-# LANGUAGE OverloadedStrings #-}

module Logging.Logger where

import Graph.Types
import Data.Time (getCurrentTime, UTCTime)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, getFileSize)
import System.IO (hFlush, stdout)
import Control.Monad (when)

logFilePath :: FilePath
logFilePath = "app.log"

maxLogSize :: Integer
maxLogSize = 10 * 1024 * 1024

logMessage :: LogLevel -> T.Text -> IO ()
logMessage level msg = do
    timestamp <- getCurrentTime
    let entry = formatEntry timestamp level msg
    checkLogSize
    TIO.appendFile logFilePath entry

formatEntry :: UTCTime -> LogLevel -> T.Text -> T.Text
formatEntry ts level msg = 
    T.pack (show ts) <> " [" <> T.pack (show level) <> "] " <> msg <> "\n"

checkLogSize :: IO ()
checkLogSize = do
    exists <- doesFileExist logFilePath
    when exists $ do
        size <- getFileSize logFilePath
        when (size > maxLogSize) rotateLog

rotateLog :: IO ()
rotateLog = do
    backupPath <- rotateFileName
    TIO.readFile logFilePath >>= TIO.writeFile backupPath
    TIO.writeFile logFilePath ""

rotateFileName :: IO FilePath
rotateFileName = do
    now <- getCurrentTime
    return $ "app." <> T.unpack (T.pack (show now)) <> ".log"

readLog :: IO [T.Text]
readLog = do
    exists <- doesFileExist logFilePath
    if exists
        then TIO.lines <$> TIO.readFile logFilePath
        else return []

clearLog :: IO ()
clearLog = TIO.writeFile logFilePath ""