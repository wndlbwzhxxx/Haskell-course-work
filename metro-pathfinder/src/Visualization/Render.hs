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

-- Раздвигаем станции, которые находятся слишком близко (расстояние < 0.9)
spreadCloseStations :: [(StationName, (Double, Double))] -> [(StationName, (Double, Double))]
spreadCloseStations stations =
  let minDist = 0.9
      shift = 0.5
      result = foldl (\acc _ -> applyShift minDist shift acc) stations [1..5]
  in result
  where
    applyShift :: Double -> Double -> [(StationName, (Double, Double))] -> [(StationName, (Double, Double))]
    applyShift minDist shiftAmount sts = foldr (checkAndShift minDist shiftAmount) sts sts
    
    checkAndShift :: Double -> Double -> (StationName, (Double, Double)) -> [(StationName, (Double, Double))] -> [(StationName, (Double, Double))]
    checkAndShift minDist shiftAmount current sts =
      foldr (\other acc -> if fst current /= fst other then shiftIfTooClose minDist shiftAmount current other acc else acc) sts sts
    
    shiftIfTooClose :: Double -> Double -> (StationName, (Double, Double)) -> (StationName, (Double, Double)) -> [(StationName, (Double, Double))] -> [(StationName, (Double, Double))]
    shiftIfTooClose minDist shiftAmount (name1, (x1, y1)) (name2, (x2, y2)) sts =
      let dx = x1 - x2
          dy = y1 - y2
          dist = sqrt (dx*dx + dy*dy)
      in if dist < minDist && dist > 0.001
         then let shiftX = (dx / dist) * shiftAmount
                  shiftY = (dy / dist) * shiftAmount
              in map (\(n, (x, y)) ->
                       if n == name1 then (n, (x + shiftX, y + shiftY))
                       else if n == name2 then (n, (x - shiftX, y - shiftY))
                       else (n, (x, y))) sts
         else sts

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
      scaleX x = if rangeX < 0.05
                 then (x - minX) * 1000 + 20
                 else (x - minX) / rangeX * 500 + 20
      scaleY y = if rangeY < 0.05
                 then (y - minY) * 600 + 20
                 else (y - minY) / rangeY * 250 + 20
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
    , "  node [shape=box, style=filled, fillcolor=lightblue, fontname=\"Arial\", fontsize=160];"
    , "  edge [fontname=\"Arial\", fontsize=140];"
    ] ++
    map nodeLine scaledNodes ++
    map edgeLine (uniqueEdges (adjacency graph)) ++
    ["}"]
  where
    highlightSet = maybe Set.empty id mHighlight
    
    coordsList = [(name, coord) | name <- Map.keys (stations graph), Just coord <- [getCoords graph name]]
    scaled = scaleCoords coordsList
    spreaded = spreadCloseStations scaled
    scaledNodes = spreaded
    
    cleanName str = filter (\c -> c /= '"' && c /= '\\' && c /= '{' && c /= '}' && c /= '\n' && c /= '\r') str
    
    nodeLine (name, (x, y)) =
      let label = T.unpack name
          cleanLabel = cleanName label
          color = if Set.member name highlightSet
                  then " [label=\"" ++ cleanLabel ++ "\", color=red, penwidth=30]"
                  else " [label=\"" ++ cleanLabel ++ "\"]"
      in T.pack $ "  \"" ++ cleanLabel ++ "\"" ++ color ++ " [pos=\"" ++ show x ++ "," ++ show y ++ "!\"];"
    
    edgeLine (from, to, w, isTransfer) =
      let fromLabel = T.unpack from
          toLabel = T.unpack to
          cleanFrom = cleanName fromLabel
          cleanTo = cleanName toLabel
          attrs = if isTransfer
                  then " [color=green, penwidth=30, fontcolor=green]"
                  else " [label=\"" ++ show w ++ " km\", fontsize=130]"
      in T.pack $ "  \"" ++ cleanFrom ++ "\" -- \"" ++ cleanTo ++ "\"" ++ attrs ++ ";"

uniqueEdges :: Map.Map StationName [(StationName, Weight)] -> [(StationName, StationName, Double, Bool)]
uniqueEdges adj = 
  [ (from, to, w', False) | (from, tos) <- Map.toList adj, (to, Weight w') <- tos, from < to, w' > 0 ] ++
  [ (from, to, 0, True) | (from, tos) <- Map.toList adj, (to, Weight w') <- tos, from < to, w' == 0 ]

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