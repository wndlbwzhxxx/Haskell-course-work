{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graph.Types where

import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.Map as Map

newtype Weight = Weight Double
  deriving (Show, Eq, Ord, Num)

type StationId = Text

data Coordinates = Coordinates
  { lat :: Double
  , lng :: Double
  } deriving (Show, Eq)

data Station = Station
  { stationId :: StationId
  , stationName :: Text
  , stationLine :: Maybe Text
  , stationCoords :: Maybe Coordinates
  } deriving (Show, Eq)

data Graph = Graph
  { stations :: Map.Map StationId Station
  , adjList :: Map.Map StationId [(StationId, Weight)]
  , isDirected :: Bool
  } deriving (Show)

data PathResult
  = NoPath
  | Path
      { pathNodes :: [StationId]
      , totalWeight :: Weight
      } deriving (Show, Eq)

data LogLevel
  = Debug
  | Info
  | Warning
  | Error
  deriving (Show, Eq, Ord)

data LogEntry = LogEntry
  { logTimestamp :: UTCTime
  , logLevel :: LogLevel
  , logMessage :: Text
  } deriving (Show)