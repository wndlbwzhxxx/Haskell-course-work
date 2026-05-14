import Test.QuickCheck
import Graph.Types
import Graph.Dijkstra
import qualified Data.Map as Map
import qualified Data.Text as T

prop_shortestPathSelf :: Graph -> StationName -> Property
prop_shortestPathSelf graph s =
  Map.member s (stations graph) ==>
    case shortestPath graph s s of
      Path [_] (Weight 0) -> property True
      _ -> property False

prop_noNegativeWeights :: Graph -> Bool
prop_noNegativeWeights graph =
  all (\(_, _, Weight w) -> w >= 0) (edgesList graph)
  where
    edgesList g = do
      (from, tos) <- Map.toList (adjacency g)
      (to, w) <- tos
      return (from, to, w)

main :: IO ()
main = do
  putStrLn "Running QuickCheck tests..."
  quickCheck (prop_noNegativeWeights :: Graph -> Bool)