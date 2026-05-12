{-# LANGUAGE OverloadedStrings #-}

module Graph.Loader where

import Graph.Types
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

data JsonStation = JsonStation
  { jsName :: T.Text
  , jsLat :: Maybe Double
  , jsLng :: Maybe Double
  , jsOrder :: Int
  , jsLine :: Maybe T.Text
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
      <*> pure Nothing

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