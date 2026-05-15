{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graph.Types (
  StationName,
  Weight(..),
  Coordinates(..),
  Station(..),
  Graph(..),
  PathResult(..),
  LogLevel(..),
  haversineDistance,
  roundToTenth
) where

import Data.Text (Text)
import qualified Data.Map as Map

type StationName = Text

newtype Weight = Weight Double
  deriving (Show, Eq, Ord, Num)

-- Округление до 0.1 (один знак после запятой)
roundToTenth :: Double -> Double
roundToTenth x = fromIntegral (round (x * 10)) / 10.0

data Coordinates = Coordinates
  { lat :: Double
  , lng :: Double
  } deriving (Show, Eq)

data Station = Station
  { stationId :: StationName
  , stationName :: Text
  , stationLine :: Maybe Text
  , stationCoords :: Maybe Coordinates
  } deriving (Show, Eq)

data Graph = Graph
  { stations :: Map.Map StationName Station
  , adjacency :: Map.Map StationName [(StationName, Weight)]
  , isDirected :: Bool
  } deriving (Show)

data PathResult = NoPath | Path [StationName] Weight
  deriving (Show, Eq)

data LogLevel = Info | Warning | Error
  deriving (Show, Eq, Ord)

-- Формула гаверсинусов с округлением до 0.1 км
haversineDistance :: Coordinates -> Coordinates -> Double
haversineDistance (Coordinates lat1 lng1) (Coordinates lat2 lng2) =
  let r = 6371.0
      dlat = (lat2 - lat1) * pi / 180
      dlng = (lng2 - lng1) * pi / 180
      lat1Rad = lat1 * pi / 180
      lat2Rad = lat2 * pi / 180
      a = sin(dlat/2) * sin(dlat/2) + cos(lat1Rad) * cos(lat2Rad) * sin(dlng/2) * sin(dlng/2)
      c = 2 * atan2 (sqrt a) (sqrt (1 - a))
  in roundToTenth (r * c)