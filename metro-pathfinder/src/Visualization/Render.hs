{-# LANGUAGE OverloadedStrings #-}

module Visualization.Render (renderGraphToPNG, renderPathToPNG) where

import Graph.Types
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Process (system)
import System.Directory (removeFile)
import System.FilePath (replaceExtension)
import Data.List (minimum, maximum)

scaleCoords :: [(StationName, (Double, Double))] -> [(StationName, (Double, Double))]
scaleCoords coords =
  let xs = [x | (_, (x, _)) <- coords]
      ys = [y | (_, (_, y)) <- coords]
      minX = minimum xs
      maxX = maximum xs
      minY = minimum ys
      maxY = maximum ys
      rangeX = maxX - minX
      rangeY = maxY - minY
      -- Уменьшаем масштаб
      scaleX x = if rangeX < 0.05
                 then (x - minX) * 80 + 10
                 else (x - minX) / rangeX * 25 + 10
      scaleY y = if rangeY < 0.05
                 then (y - minY) * 60 + 10
                 else (y - minY) / rangeY * 20 + 10
  in [(name, (scaleX x, scaleY y)) | (name, (x, y)) <- coords]

getCoords :: Graph -> StationName -> Maybe (Double, Double)
getCoords graph name = do
  station <- Map.lookup name (stations graph)
  case stationCoords station of
    Just (Coordinates lat lng) -> Just (lng, lat)
    Nothing -> Nothing

generateDot :: Graph -> Maybe (Set.Set StationName) -> T.Text
generateDot graph mHighlight =
  T.unlines $
    [ "graph G {"
    , "  charset=latin1;"
    , "  node [shape=box, style=filled, fillcolor=lightblue, fontname=\"Arial\", fontsize=11];"
    , "  edge [fontname=\"Arial\", fontsize=10];"
    ] ++
    map nodeLine scaledNodes ++
    map edgeLine (uniqueEdges (adjacency graph)) ++
    ["}"]
  where
    highlightSet = maybe Set.empty id mHighlight
    
    coordsList :: [(StationName, (Double, Double))]
    coordsList = [(name, coord) | name <- Map.keys (stations graph), Just coord <- [getCoords graph name]]
    
    scaledNodes = scaleCoords coordsList
    
    cleanName :: String -> String
    cleanName = filter (\c -> c /= '"' && c /= '\\' && c /= '{' && c /= '}' && c /= '\n' && c /= '\r')
    
    nodeLine :: (StationName, (Double, Double)) -> T.Text
    nodeLine (name, (x, y)) =
      let label = T.unpack name
          cleanLabel = cleanName label
          color = if Set.member name highlightSet
                  then " [label=\"" ++ cleanLabel ++ "\", color=red, penwidth=3]"
                  else " [label=\"" ++ cleanLabel ++ "\"]"
      in T.pack $ "  \"" ++ cleanLabel ++ "\"" ++ color ++ " [pos=\"" ++ show x ++ "," ++ show y ++ "!\"];"
    
    edgeLine :: (StationName, StationName, Double, Bool) -> T.Text
    edgeLine (from, to, w, isTransfer) =
      let fromLabel = T.unpack from
          toLabel = T.unpack to
          cleanFrom = cleanName fromLabel
          cleanTo = cleanName toLabel
          attrs = if isTransfer
                  then " [label=\"\", color=green, penwidth=3]"
                  else if w == 1.0
                       then " [label=\"\"]"
                       else " [label=\"" ++ show w ++ "\"]"
      in T.pack $ "  \"" ++ cleanFrom ++ "\" -- \"" ++ cleanTo ++ "\"" ++ attrs ++ ";"

uniqueEdges :: Map.Map StationName [(StationName, Weight)] -> [(StationName, StationName, Double, Bool)]
uniqueEdges adj = 
  [ (from, to, w', False)
  | (from, tos) <- Map.toList adj
  , (to, Weight w') <- tos
  , from < to
  , w' > 0
  ] ++
  [ (from, to, 0, True)
  | (from, tos) <- Map.toList adj
  , (to, Weight w') <- tos
  , from < to
  , w' == 0
  ]

renderGraphToPNG :: Graph -> FilePath -> IO ()
renderGraphToPNG graph outputPath = do
  let dotContent = generateDot graph Nothing
      dotFile = replaceExtension outputPath "dot"
  TIO.writeFile dotFile dotContent
  _ <- system (unwords ["neato", "-Tpng", dotFile, "-o", outputPath])
  removeFile dotFile
  putStrLn $ "Graph saved to: " ++ outputPath

renderPathToPNG :: Graph -> PathResult -> FilePath -> IO ()
renderPathToPNG graph (Path nodes _) outputPath = do
  let dotContent = generateDot graph (Just (Set.fromList nodes))
      dotFile = replaceExtension outputPath "dot"
  TIO.writeFile dotFile dotContent
  _ <- system (unwords ["neato", "-Tpng", dotFile, "-o", outputPath])
  removeFile dotFile
  putStrLn $ "Path saved to: " ++ outputPath
renderPathToPNG _ NoPath _ =
  putStrLn "No path found"