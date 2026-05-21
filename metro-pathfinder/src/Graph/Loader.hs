{-# LANGUAGE OverloadedStrings #-}

module Graph.Loader (loadGraphFromJSON, exportGraphToJSON) where

import Graph.Types
import Data.Aeson
import Data.Aeson.Types (object, (.=))
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.List (sortOn)
import Data.Maybe (isJust)

data JsonStation = JsonStation
  { jsName :: T.Text
  , jsLat :: Maybe Double
  , jsLng :: Maybe Double
  , jsOrder :: Int
  , jsTransfers :: [T.Text]
  } deriving (Show)

data JsonLine = JsonLine
  { jlName :: T.Text
  , jlHexColor :: Maybe T.Text
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
      <*> v .: "order"
      <*> v .:? "transfers" .!= []

instance FromJSON JsonLine where
  parseJSON = withObject "JsonLine" $ \v ->
    JsonLine
      <$> v .: "name"
      <*> v .:? "hex_color"
      <*> v .: "stations"

instance FromJSON JsonMetro where
  parseJSON = withObject "JsonMetro" $ \v ->
    JsonMetro
      <$> v .: "name"
      <*> v .: "lines"

instance ToJSON JsonStation where
  toJSON (JsonStation name lat lng order transfers) = object
    [ "name" .= name
    , "lat" .= lat
    , "lng" .= lng
    , "order" .= order
    , "transfers" .= transfers
    ]

instance ToJSON JsonLine where
  toJSON (JsonLine name hexColor stations) = object
    [ "name" .= name
    , "hex_color" .= hexColor
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
    Right metro -> 
      let graph = buildGraph metro
      in case validateGraph graph of
           Left err -> return (Left err)
           Right _ -> return (Right graph)

validateGraph :: Graph -> Either String Graph
validateGraph graph =
  let stationNames = Map.keys (stations graph)
      allTransfers = getTransfersList graph
      invalidTransfers = filter (\(from, to) -> not (to `elem` stationNames)) allTransfers
  in if null invalidTransfers
     then Right graph
     else Left ("Invalid transfers (station not found): " ++ show invalidTransfers)

getTransfersList :: Graph -> [(StationName, StationName)]
getTransfersList graph =
  [ (from, to)
  | (from, tos) <- Map.toList (adjacency graph)
  , (to, Weight w) <- tos
  , w == 0
  ]

getDistanceBetweenStations :: Station -> Station -> Double
getDistanceBetweenStations s1 s2 =
  case (stationCoords s1, stationCoords s2) of
    (Just c1, Just c2) -> haversineDistance c1 c2
    _ -> 1.0

buildGraph :: JsonMetro -> Graph
buildGraph metro =
  Graph
    { stations = stationsMap
    , adjacency = edgesMapWithTransfers
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

    lineEdges = foldr addLineEdges Map.empty (jmLines metro)

    addLineEdges line acc =
      let stationsSorted = sortOn jsOrder (jlStations line)
          pairs = zip stationsSorted (tail stationsSorted)
      in foldr (\(s1, s2) m -> 
           let station1 = stationsMap Map.! jsName s1
               station2 = stationsMap Map.! jsName s2
               dist = getDistanceBetweenStations station1 station2
           in addEdge (jsName s1) (jsName s2) dist m
         ) acc pairs

    addEdge from to weight m =
      Map.insertWith (++) from [(to, Weight weight)] $
      Map.insertWith (++) to [(from, Weight weight)] m

    allTransfers = do
      line <- jmLines metro
      station <- jlStations line
      transfer <- jsTransfers station
      return (jsName station, transfer)

    edgesMapWithTransfers = foldr (\(from, to) acc -> addEdge from to 0.1 acc) lineEdges allTransfers

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
    lineMap :: Map.Map T.Text [(StationName, Maybe (Double, Double), [StationName])]
    lineMap = Map.foldrWithKey addToLine Map.empty (stations graph)
    
    addToLine :: StationName -> Station -> Map.Map T.Text [(StationName, Maybe (Double, Double), [StationName])] -> Map.Map T.Text [(StationName, Maybe (Double, Double), [StationName])]
    addToLine sid s acc =
      let coords = case stationCoords s of
            Just (Coordinates lat lng) -> Just (lng, lat)
            Nothing -> Nothing
          transfers = getTransfersForStation graph sid
          entry = (sid, coords, transfers)
      in case stationLine s of
           Just lineName -> Map.insertWith (++) lineName [entry] acc
           Nothing -> acc
    
    getTransfersForStation :: Graph -> StationName -> [StationName]
    getTransfersForStation g sid =
      case Map.lookup sid (adjacency g) of
        Just tos -> [to | (to, Weight w) <- tos, w == 0]
        Nothing -> []
    
    sortByOrder :: [(StationName, Maybe (Double, Double), [StationName])] -> [JsonStation]
    sortByOrder sts = 
      let withOrder = [(s, c, t, getOrderFromCoords c) | (s, c, t) <- sts]
          sorted = sortOn (\(_, _, _, o) -> o) withOrder
      in [JsonStation s (fmap snd c) (fmap fst c) o t | (s, c, t, o) <- sorted]
    
    getOrderFromCoords :: Maybe (Double, Double) -> Int
    getOrderFromCoords Nothing = 0
    getOrderFromCoords (Just (lng, lat)) = floor ((lat - 50) * 100)
    
    linesList = map (\(lineName, sts) ->
      JsonLine
        { jlName = lineName
        , jlHexColor = Nothing
        , jlStations = sortByOrder sts
        }) (Map.toList lineMap)