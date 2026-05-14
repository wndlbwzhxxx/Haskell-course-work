{-# LANGUAGE OverloadedStrings #-}

module Graph.Loader (loadGraphFromJSON) where

import Graph.Types
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import qualified Data.Text as T

data JsonStation = JsonStation
  { jsName :: T.Text
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
    JsonStation <$> v .: "name"

instance FromJSON JsonLine where
  parseJSON = withObject "JsonLine" $ \v ->
    JsonLine <$> v .: "name" <*> v .: "stations"

instance FromJSON JsonMetro where
  parseJSON = withObject "JsonMetro" $ \v ->
    JsonMetro <$> v .: "name" <*> v .: "lines"

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
      return (sid, sid)

    edgesMap = foldr addLineEdges Map.empty (jmLines metro)

    addLineEdges line acc =
      let sts = jlStations line
          pairs = zip sts (tail sts)
      in foldr (\(s1, s2) m -> addEdge (jsName s1) (jsName s2) 1.0 m) acc pairs

    addEdge from to weight m =
      Map.insertWith (++) from [(to, Weight weight)] $
      Map.insertWith (++) to [(from, Weight weight)] m