{-# LANGUAGE OverloadedStrings #-}

module Visualization.Render (renderGraphToPNG, renderPathToPNG) where

import Graph.Types
import qualified Data.Graph.UGraph as UG
import qualified Data.Graph.Types as GT
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Graph.Visualize (plotUGraphPng)

toGraphiteGraph :: Graph -> UG.UGraph String String
toGraphiteGraph graph =
  let vertices = map T.unpack (Map.keys (stations graph))
      edges = collectEdges (adjacency graph)
      ug = foldr (\v g -> GT.insertVertex v g) GT.empty vertices
      ug2 = foldr (\(f,t) g -> UG.insertEdge (GT.Edge f t "") g) ug edges
  in ug2
  where
    collectEdges adj =
      [ (T.unpack from, T.unpack to)
      | (from, tos) <- Map.toList adj
      , (to, _) <- tos
      ]

renderGraphToPNG :: Graph -> FilePath -> IO ()
renderGraphToPNG graph path = do
  let ug = toGraphiteGraph graph
  _ <- plotUGraphPng ug path
  putStrLn $ "Graph saved to: " ++ path

renderPathToPNG :: Graph -> PathResult -> FilePath -> IO ()
renderPathToPNG graph (Path nodes _) path = do
  let ug = toGraphiteGraph graph
  _ <- plotUGraphPng ug path
  putStrLn $ "Path nodes: " ++ show (map T.unpack nodes)
  putStrLn $ "Path saved to: " ++ path
renderPathToPNG _ NoPath _ =
  putStrLn "No path found"