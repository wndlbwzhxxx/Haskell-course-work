module Graph.Dijkstra (shortestPath) where

import Graph.Types
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

shortestPath :: Graph -> StationName -> StationName -> PathResult
shortestPath graph start target
  | start == target = Path [start] (Weight 0)
  | otherwise = case dijkstra graph start of
      Nothing -> NoPath
      Just (dist, prev) -> case Map.lookup target dist of
        Nothing -> NoPath
        Just d -> Path (reconstruct prev start target) d

dijkstra :: Graph -> StationName -> Maybe (Map.Map StationName Weight, Map.Map StationName StationName)
dijkstra graph start =
  let initialDist = Map.singleton start (Weight 0)
      initialPrev = Map.empty
  in loop Set.empty initialDist initialPrev (Map.singleton start (Weight 0))
  where
    loop visited dist prev queue
      | Map.null queue = Just (dist, prev)
      | otherwise =
          let ((current, _), queue') = Map.deleteFindMin queue
          in if Set.member current visited
             then loop visited dist prev queue'
             else let visited' = Set.insert current visited
                      neighbors = fromMaybe [] (Map.lookup current (adjacency graph))
                      (dist', prev', queue'') = foldr (\(n, w) (d, p, q) ->
                        let currentDist = fromMaybe (Weight 0) (Map.lookup current d)
                            newDist = currentDist + w
                        in if newDist < fromMaybe (Weight (1/0)) (Map.lookup n d)
                           then (Map.insert n newDist d, Map.insert n current p, Map.insert n newDist q)
                           else (d, p, q)) (dist, prev, queue') neighbors
                  in loop visited' dist' prev' queue''

reconstruct :: Map.Map StationName StationName -> StationName -> StationName -> [StationName]
reconstruct prev start target =
  reverse $ go [target]
  where
    go (x:xs)
      | x == start = x : xs
      | otherwise = case Map.lookup x prev of
          Just p -> go (p : x : xs)
          Nothing -> x : xs
    go [] = []