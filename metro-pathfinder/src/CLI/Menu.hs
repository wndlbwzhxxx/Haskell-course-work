module CLI.Menu (runCLI) where

import Graph.Types
import Graph.Dijkstra
import Graph.Loader (loadGraphFromJSON, exportGraphToJSON)
import Visualization.Render (renderGraphToPNG, renderPathToPNG)
import Logging.Logger
import qualified Data.Map as Map
import qualified Data.Text as T
import System.IO (hFlush, stdout)
import Data.Char (toLower)

data AppState = AppState
  { currentGraph :: Maybe Graph
  , lastPathResult :: Maybe PathResult
  }

showWeight :: Weight -> String
showWeight (Weight w) = 
  let rounded = fromIntegral (round (w * 10)) / 10.0
  in show rounded

runCLI :: IO ()
runCLI = do
  logMsg Info "Application started"
  loop (AppState Nothing Nothing)

loop :: AppState -> IO ()
loop state = do
  putStrLn "\n========================================"
  putStrLn "       METRO PATHFINDER"
  putStrLn "========================================"
  putStrLn "1. Load graph from JSON"
  putStrLn "2. Export graph to JSON"
  putStrLn "3. Find shortest path"
  putStrLn "4. List all stations"
  putStrLn "5. Find station by name"
  putStrLn "6. Visualize entire graph"
  putStrLn "7. Visualize path"
  putStrLn "8. View log"
  putStrLn "9. Clear log"
  putStrLn "0. Exit"
  putStrLn "========================================"
  putStr "Your choice: "
  hFlush stdout
  choice <- getLine
  case choice of
    "1" -> loadJSON state >>= loop
    "2" -> exportJSON state >>= loop
    "3" -> findPath state >>= loop
    "4" -> listStations state >>= loop
    "5" -> findStation state >>= loop
    "6" -> visualizeGraph state >>= loop
    "7" -> visualizePath state >>= loop
    "8" -> viewLog >>= loop
    "9" -> clearLogHandler >>= loop
    "0" -> do
      logMsg Info "Application exited"
      putStrLn "Goodbye!"
    _ -> do
      putStrLn "Invalid choice."
      loop state

loadJSON :: AppState -> IO AppState
loadJSON state = do
  putStrLn "\n--- Load Graph from JSON ---"
  putStr "Path to JSON file: "
  path <- getLine
  if null path
    then do
      putStrLn "Error: Path cannot be empty."
      return state
    else do
      result <- loadGraphFromJSON path
      case result of
        Left err -> do
          putStrLn $ "Error: " ++ err
          logMsg Error ("Failed to load: " ++ err)
          return state
        Right graph -> do
          let stationCount = Map.size (stations graph)
          putStrLn $ "Loaded " ++ show stationCount ++ " stations."
          logMsg Info ("Graph loaded from " ++ path)
          return (state { currentGraph = Just graph })

exportJSON :: AppState -> IO AppState
exportJSON state = do
  case currentGraph state of
    Nothing -> do
      putStrLn "No graph loaded."
      return state
    Just graph -> do
      putStrLn "\n--- Export Graph to JSON ---"
      putStr "Save path: "
      path <- getLine
      if null path
        then do
          putStrLn "Error: Path cannot be empty."
          return state
        else do
          exportGraphToJSON graph path
          putStrLn "Graph exported."
          logMsg Info ("Graph exported to " ++ path)
          return state

findPath :: AppState -> IO AppState
findPath state = do
  case currentGraph state of
    Nothing -> do
      putStrLn "No graph loaded."
      return state
    Just graph -> do
      putStrLn "\n--- Find Shortest Path ---"
      putStr "Start station: "
      start <- getLine
      putStr "Target station: "
      target <- getLine
      if null start || null target
        then do
          putStrLn "Station names cannot be empty."
          return state
        else do
          let startT = T.pack start
              targetT = T.pack target
          if not (Map.member startT (stations graph))
            then do
              putStrLn $ "Station '" ++ start ++ "' not found."
              return state
            else if not (Map.member targetT (stations graph))
              then do
                putStrLn $ "Station '" ++ target ++ "' not found."
                return state
              else do
                let result = shortestPath graph startT targetT
                case result of
                  NoPath -> do
                    putStrLn "No path found."
                    logMsg Warning "No path found"
                    return state
                  Path nodes w -> do
                    putStrLn $ "Path found!"
                    putStrLn $ "Distance: " ++ showWeight w ++ " km"
                    putStrLn "Route:"
                    mapM_ (\n -> putStrLn $ "  -> " ++ T.unpack n) nodes
                    logMsg Info ("Path found from " ++ start ++ " to " ++ target)
                    return (state { lastPathResult = Just result })

listStations :: AppState -> IO AppState
listStations state = do
  case currentGraph state of
    Nothing -> do
      putStrLn "No graph loaded."
      return state
    Just graph -> do
      putStrLn "\n--- List All Stations ---"
      let stationsList = Map.keys (stations graph)
      putStrLn $ "Total stations: " ++ show (length stationsList)
      mapM_ (\s -> putStrLn $ "  - " ++ T.unpack s) stationsList
      return state

findStation :: AppState -> IO AppState
findStation state = do
  case currentGraph state of
    Nothing -> do
      putStrLn "No graph loaded."
      return state
    Just graph -> do
      putStrLn "\n--- Find Station ---"
      putStr "Station name (or part): "
      query <- getLine
      if null query
        then do
          putStrLn "Query cannot be empty."
          return state
        else do
          let results = filter (\s -> T.isInfixOf (T.pack query) s) (Map.keys (stations graph))
          if null results
            then putStrLn "No stations found."
            else do
              putStrLn $ "Found " ++ show (length results) ++ " stations:"
              mapM_ (\s -> putStrLn $ "  - " ++ T.unpack s) results
          return state

visualizeGraph :: AppState -> IO AppState
visualizeGraph state = do
  case currentGraph state of
    Nothing -> do
      putStrLn "No graph loaded."
      return state
    Just graph -> do
      putStrLn "\n--- Visualize Graph ---"
      putStr "PNG filename: "
      path <- getLine
      if null path
        then do
          putStrLn "Filename cannot be empty."
          return state
        else do
          renderGraphToPNG graph path
          logMsg Info ("Graph visualization saved to " ++ path)
          return state

visualizePath :: AppState -> IO AppState
visualizePath state = do
  case (currentGraph state, lastPathResult state) of
    (Nothing, _) -> do
      putStrLn "No graph loaded."
      return state
    (_, Nothing) -> do
      putStrLn "No path found. Run find path first."
      return state
    (Just graph, Just path) -> do
      putStrLn "\n--- Visualize Path ---"
      putStr "PNG filename: "
      pathStr <- getLine
      if null pathStr
        then do
          putStrLn "Filename cannot be empty."
          return state
        else do
          renderPathToPNG graph path pathStr
          logMsg Info ("Path visualization saved to " ++ pathStr)
          return state

viewLog :: IO AppState
viewLog = do
  putStrLn "\n--- View Log ---"
  entries <- readLog
  if null entries
    then putStrLn "Log is empty."
    else do
      mapM_ putStrLn (take 30 entries)
  return (AppState Nothing Nothing)

clearLogHandler :: IO AppState
clearLogHandler = do
  putStrLn "\n--- Clear Log ---"
  putStr "Clear log? (y/n): "
  confirm <- getLine
  let lowerConfirm = map toLower confirm
  if lowerConfirm == "y" || lowerConfirm == "yes"
    then do
      clearLog
      putStrLn "Log cleared."
      logMsg Info "Log cleared by user"
    else do
      putStrLn "Cancelled."
  return (AppState Nothing Nothing)