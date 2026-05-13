module CLI.Menu where

import Graph.Types
import Graph.Loader
import Graph.Dijkstra
import Logging.Logger
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (isJust, fromMaybe)
import System.IO (hFlush, stdout)

data AppState = AppState
  { currentGraph :: Maybe Graph
  , lastPath :: Maybe PathResult
  }

runCLI :: IO ()
runCLI = do
  logMessage Info "Application started"
  loop (AppState Nothing Nothing)
  where
    loop state = do
      showMenu
      choice <- getLine
      case choice of
        "1" -> loadGraphHandler state >>= loop
        "2" -> exportGraphHandler state >>= loop
        "3" -> listStationsHandler state >>= loop
        "4" -> findStationHandler state >>= loop
        "5" -> findPathHandler state >>= loop
        "6" -> visualizeGraphHandler state >>= loop
        "7" -> visualizePathHandler state >>= loop
        "8" -> viewLogHandler >>= loop
        "9" -> clearLogHandler >>= loop
        "0" -> do
          logMessage Info "Application exited"
          putStrLn "Goodbye!"
        _ -> do
          putStrLn "Invalid choice. Press Enter to continue..."
          _ <- getLine
          loop state

showMenu :: IO ()
showMenu = do
  putStrLn "\n========================================"
  putStrLn "       METRO PATHFINDER"
  putStrLn "========================================"
  putStrLn "1. Load graph from JSON"
  putStrLn "2. Export graph to JSON"
  putStrLn "3. List all stations"
  putStrLn "4. Find station by name"
  putStrLn "5. Find shortest path"
  putStrLn "6. Visualize entire graph"
  putStrLn "7. Visualize path"
  putStrLn "8. View log"
  putStrLn "9. Clear log"
  putStrLn "0. Exit"
  putStrLn "========================================"
  putStr "Your choice: "
  hFlush stdout

loadGraphHandler :: AppState -> IO AppState
loadGraphHandler state = do
  putStr "Enter path to JSON file: "
  path <- getLine
  if null path
    then do
      putStrLn "Path cannot be empty"
      return state
    else do
      result <- loadGraphFromJSON path
      case result of
        Left err -> do
          putStrLn $ "Error loading graph: " ++ err
          logMessage Error (T.pack ("Failed to load graph: " ++ err))
          return state
        Right graph -> do
          putStrLn $ "Graph loaded successfully! Stations: " ++ show (Map.size (stations graph))
          logMessage Info (T.pack ("Graph loaded from " ++ path))
          return (state { currentGraph = Just graph })

exportGraphHandler :: AppState -> IO AppState
exportGraphHandler state = do
  case currentGraph state of
    Nothing -> do
      putStrLn "No graph loaded. Please load a graph first."
      return state
    Just graph -> do
      putStr "Enter path to save JSON: "
      path <- getLine
      if null path
        then do
          putStrLn "Path cannot be empty"
          return state
        else do
          exportGraphToJSON graph path
          putStrLn "Graph exported successfully"
          logMessage Info (T.pack ("Graph exported to " ++ path))
          return state

listStationsHandler :: AppState -> IO AppState
listStationsHandler state = do
  case currentGraph state of
    Nothing -> do
      putStrLn "No graph loaded. Please load a graph first."
      return state
    Just graph -> do
      let sts = Map.toList (stations graph)
      putStrLn $ "\nTotal stations: " ++ show (length sts)
      putStrLn "----------------------------------------"
      mapM_ (\(_, s) -> putStrLn $ T.unpack (stationName s)) (take 20 sts)
      when (length sts > 20) $ putStrLn "... and " ++ show (length sts - 20) ++ " more"
      return state

findStationHandler :: AppState -> IO AppState
findStationHandler state = do
  case currentGraph state of
    Nothing -> do
      putStrLn "No graph loaded. Please load a graph first."
      return state
    Just graph -> do
      putStr "Enter station name (or part of it): "
      query <- getLine
      if null query
        then do
          putStrLn "Query cannot be empty"
          return state
        else do
          let results = Map.filter (\s -> query `T.isInfixOf` T.toLower (stationName s)) (stations graph)
          if Map.null results
            then putStrLn "No stations found"
            else do
              putStrLn $ "Found " ++ show (Map.size results) ++ " stations:"
              mapM_ (\s -> putStrLn $ "  - " ++ T.unpack (stationName s)) (Map.elems results)
          return state

findPathHandler :: AppState -> IO AppState
findPathHandler state = do
  case currentGraph state of
    Nothing -> do
      putStrLn "No graph loaded. Please load a graph first."
      return state
    Just graph -> do
      putStr "Enter start station: "
      start <- getLine
      putStr "Enter target station: "
      target <- getLine
      if null start || null target
        then do
          putStrLn "Station names cannot be empty"
          return state
        else do
          let startId = T.pack start
              targetId = T.pack target
          if Map.notMember startId (stations graph)
            then do
              putStrLn "Start station not found"
              return state
            else if Map.notMember targetId (stations graph)
              then do
                putStrLn "Target station not found"
                return state
              else do
                let result = shortestPath graph startId targetId
                case result of
                  NoPath -> do
                    putStrLn "No path found between these stations"
                    logMessage Warning "No path found"
                    return state
                  Path nodes w -> do
                    putStrLn $ "Path found! Total weight: " ++ show w
                    putStrLn "Route:"
                    mapM_ (\n -> putStrLn $ "  -> " ++ T.unpack n) nodes
                    logMessage Info (T.pack ("Path found from " ++ start ++ " to " ++ target))
                    return (state { lastPath = Just result })

visualizeGraphHandler :: AppState -> IO AppState
visualizeGraphHandler state = do
  case currentGraph state of
    Nothing -> do
      putStrLn "No graph loaded. Please load a graph first."
      return state
    Just _ -> do
      putStrLn "Visualization will be implemented in Render module"
      putStrLn "(requires graphviz library)"
      return state

visualizePathHandler :: AppState -> IO AppState
visualizePathHandler state = do
  case (currentGraph state, lastPath state) of
    (Nothing, _) -> do
      putStrLn "No graph loaded. Please load a graph first."
      return state
    (_, Nothing) -> do
      putStrLn "No path found. Please find a path first (option 5)."
      return state
    (Just _, Just _) -> do
      putStrLn "Path visualization will be implemented in Render module"
      putStrLn "(requires graphviz library)"
      return state

viewLogHandler :: IO AppState
viewLogHandler = do
  entries <- readLog
  if null entries
    then putStrLn "Log is empty"
    else do
      putStrLn "\n=== LOG ==="
      mapM_ TIO.putStrLn (take 50 entries)
      putStrLn "==========="
  return (AppState Nothing Nothing)

clearLogHandler :: IO AppState
clearLogHandler = do
  clearLog
  putStrLn "Log cleared"
  logMessage Info "Log cleared by user"
  return (AppState Nothing Nothing)