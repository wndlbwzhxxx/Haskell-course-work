{-# LANGUAGE OverloadedStrings #-}

module Graph.Loader (loadGraphFromJSON, exportGraphToJSON) where

import Graph.Types
import Data.Aeson
import Data.Aeson.Types (object, (.=))
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import qualified Data.Text as T

data JsonStation = JsonStation
  { jsName :: T.Text
  , jsLat :: Maybe Double
  , jsLng :: Maybe Double
  } deriving (Show)

data JsonLine = JsonLine
  { jlName :: T.Text
  , jlStations :: [JsonStation]
  } deriving (Show)

data JsonMetro = JsonMetro
  { jmName :: T.Text
  , jmLines :: [JsonLine]
  } deriving (Show)

instance FromJSON JsonStation where
  parseJSON = withObject "JsonStation" $ \v ->
    JsonStation
      <$> v .: "name"
      <*> v .:? "lat"
      <*> v .:? "lng"

instance FromJSON JsonLine where
  parseJSON = withObject "JsonLine" $ \v ->
    JsonLine
      <$> v .: "name"
      <*> v .: "stations"

instance FromJSON JsonMetro where
  parseJSON = withObject "JsonMetro" $ \v ->
    JsonMetro
      <$> v .: "name"
      <*> v .: "lines"

instance ToJSON JsonStation where
  toJSON (JsonStation name lat lng) = object
    [ "name" .= name
    , "lat" .= lat
    , "lng" .= lng
    ]

instance ToJSON JsonLine where
  toJSON (JsonLine name stations) = object
    [ "name" .= name
    , "stations" .= stations
    ]

instance ToJSON JsonMetro where
  toJSON (JsonMetro name lines) = object
    [ "name" .= name
    , "lines" .= lines
    ]

loadGraphFromJSON :: FilePath -> IO (Either String Graph)
loadGraphFromJSON path = do
  content <- B.readFile path
  case eitherDecode content of
    Left err -> return (Left err)
    Right metro -> return (Right (buildGraph metro))

buildGraph :: JsonMetro -> Graph
buildGraph metro =
  Graph
    { stations = stationsMap
    , adjacency = edgesMap
    , isDirected = False
    }
  where
    stationsMap = Map.fromList $ do
      line <- jmLines metro
      station <- jlStations line
      let sid = jsName station
      let coords = case (jsLat station, jsLng station) of
            (Just lat', Just lng') -> Just (Coordinates lat' lng')
            _ -> Nothing
      return (sid, Station sid (jsName station) (Just (jlName line)) coords)

    edgesMap = foldr addLineEdges Map.empty (jmLines metro)

    addLineEdges line acc =
      let sts = jlStations line
          pairs = zip sts (tail sts)
      in foldr (\(s1, s2) m -> addEdge (jsName s1) (jsName s2) 1.0 m) acc pairs

    addEdge from to weight m =
      Map.insertWith (++) from [(to, Weight weight)] $
      Map.insertWith (++) to [(from, Weight weight)] m

exportGraphToJSON :: Graph -> FilePath -> IO ()
exportGraphToJSON graph path = do
  let jsonMetro = graphToJson graph
  B.writeFile path (encode jsonMetro)

graphToJson :: Graph -> JsonMetro
graphToJson graph =
  JsonMetro
    { jmName = "Exported Metro"
    , jmLines = linesList
    }
  where
    stationToLineMap :: Map.Map StationName T.Text
    stationToLineMap = Map.mapMaybe stationLine (stations graph)
    
    stationCoordsMap :: Map.Map StationName (Maybe (Double, Double))
    stationCoordsMap = Map.map (\s -> case stationCoords s of
                                        Just (Coordinates lat lng) -> Just (lat, lng)
                                        Nothing -> Nothing) (stations graph)
    
    lineStations :: Map.Map T.Text [StationName]
    lineStations = Map.foldrWithKey addToLine Map.empty (stations graph)
    
    addToLine :: StationName -> Station -> Map.Map T.Text [StationName] -> Map.Map T.Text [StationName]
    addToLine sid s acc =
      case stationLine s of
        Just lineName -> Map.insertWith (++) lineName [sid] acc
        Nothing -> acc
    
    linesList = map (\(lineName, sts) ->
      JsonLine
        { jlName = lineName
        , jlStations = map (\s -> JsonStation
            { jsName = s
            , jsLat = case Map.lookup s stationCoordsMap of
                Just (Just (lat, _)) -> Just lat
                _ -> Nothing
            , jsLng = case Map.lookup s stationCoordsMap of
                Just (Just (_, lng)) -> Just lng
                _ -> Nothing
            }) sts
        }) (Map.toList lineStations)