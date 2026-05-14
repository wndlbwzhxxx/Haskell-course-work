{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graph.Types (StationName, Weight(..), Graph(..), PathResult(..), LogLevel(..)) where

import Data.Text (Text)
import qualified Data.Map as Map

type StationName = Text

newtype Weight = Weight Double
  deriving (Show, Eq, Ord, Num)

data Graph = Graph
  { stations :: Map.Map StationName StationName
  , adjacency :: Map.Map StationName [(StationName, Weight)]
  , isDirected :: Bool
  } deriving (Show)

data PathResult = NoPath | Path [StationName] Weight
  deriving (Show, Eq)

data LogLevel = Info | Warning | Error
  deriving (Show, Eq, Ord)