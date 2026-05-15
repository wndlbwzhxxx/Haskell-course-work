module Main where

import Test.QuickCheck
import Graph.Types
import Graph.Dijkstra
import qualified Data.Map as Map
import qualified Data.Text as T

-- ============================================================
-- Экземпляры Arbitrary для типов
-- ============================================================

instance Arbitrary T.Text where
  arbitrary = elements [T.pack "A", T.pack "B", T.pack "C", T.pack "D", 
                        T.pack "E", T.pack "F", T.pack "G", T.pack "H"]

instance Arbitrary Weight where
  arbitrary = do
    w <- choose (0.1, 100.0) :: Gen Double
    return (Weight (fromIntegral (round (w * 10)) / 10.0))

instance Arbitrary Graph where
  arbitrary = do
    stationsCount <- choose (2, 5) :: Gen Int
    let stationNames = take stationsCount [T.pack [c] | c <- ['A'..'Z']]
    let stationsMap = Map.fromList [(name, Station name name Nothing Nothing) | name <- stationNames]
    
    -- Генерируем простой линейный граф (без петель, симметричный)
    let pairs = zip stationNames (tail stationNames)
    edgesWithWeights <- mapM (\(a, b) -> do
      w <- arbitrary :: Gen Weight
      return [(a, (b, w)), (b, (a, w))]) pairs
    
    let adj = Map.fromListWith (++) [(from, [(to, w)]) | edges <- edgesWithWeights, (from, (to, w)) <- edges]
    return (Graph stationsMap adj False)

-- ============================================================
-- Свойства
-- ============================================================

-- 1. Путь из станции в саму себя имеет длину 0
prop_selfPath :: Graph -> Property
prop_selfPath graph =
  let stationsList = Map.keys (stations graph)
  in not (null stationsList) ==>
     let s = head stationsList
         result = shortestPath graph s s
     in case result of
          Path nodes w -> nodes == [s] && w == 0
          NoPath -> False

-- 2. Нет отрицательных весов
prop_noNegativeWeights :: Graph -> Bool
prop_noNegativeWeights graph =
  let edgesList = [(from, to, w) | (from, tos) <- Map.toList (adjacency graph), (to, w) <- tos]
  in all (\(_, _, Weight w) -> w >= 0) edgesList

-- 3. Симметричность неориентированного графа
prop_undirectedSymmetric :: Graph -> Bool
prop_undirectedSymmetric graph =
  let edgesList = [(from, to, w) | (from, tos) <- Map.toList (adjacency graph), (to, w) <- tos]
      symmetric = all (\(from, to, w) -> 
        case Map.lookup to (adjacency graph) of
          Just tos' -> any (\(to2, w2) -> to2 == from && w2 == w) tos'
          Nothing -> False) edgesList
  in symmetric

-- 4. Длина пути неотрицательна
prop_pathLengthNonNegative :: Graph -> Property
prop_pathLengthNonNegative graph =
  let stationsList = Map.keys (stations graph)
      len = length stationsList
  in len >= 2 ==>
     let a = stationsList !! 0
         b = stationsList !! (len - 1)
         result = shortestPath graph a b
     in case result of
          Path _ (Weight w) -> w >= 0
          NoPath -> True

-- ============================================================
-- Запуск всех тестов
-- ============================================================

main :: IO ()
main = do
  putStrLn "\n========================================"
  putStrLn "       QUICKCHECK TESTS"
  putStrLn "========================================\n"
  
  putStrLn "1. Path from station to itself (distance 0)"
  quickCheck prop_selfPath
  
  putStrLn "\n2. No negative weights"
  quickCheck prop_noNegativeWeights
  
  putStrLn "\n3. Undirected graph symmetry"
  quickCheck prop_undirectedSymmetric
  
  putStrLn "\n4. Path length is non-negative"
  quickCheck prop_pathLengthNonNegative
  
  putStrLn "\n========================================"
  putStrLn "       ALL TESTS COMPLETED"
  putStrLn "========================================\n"