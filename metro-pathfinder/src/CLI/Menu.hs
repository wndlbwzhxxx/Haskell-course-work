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
  putStr "Your choice (0-9): "
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
      putStrLn "Invalid choice. Please enter a number from 0 to 9."
      loop state

loadJSON :: AppState -> IO AppState
loadJSON state = do
  putStrLn "\n--- Load Graph from JSON ---"
  putStrLn "Enter the path to a JSON file containing the metro schema."
  putStrLn "Example: 'kiev.json' or 'samara.json'"
  putStr "Filename: "
  path <- getLine
  if null path
    then do
      putStrLn "Error: Filename cannot be empty. Please try again."
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
          putStrLn $ "Success! Loaded " ++ show stationCount ++ " stations."
          logMsg Info ("Graph loaded from " ++ path)
          return (state { currentGraph = Just graph })

exportJSON :: AppState -> IO AppState
exportJSON state = do
  case currentGraph state of
    Nothing -> do
      putStrLn "\n--- Export Graph to JSON ---"
      putStrLn "Error: No graph is currently loaded."
      putStrLn "Please load a graph first (option 1)."
      return state
    Just graph -> do
      putStrLn "\n--- Export Graph to JSON ---"
      putStrLn "Enter the path where to save the JSON file."
      putStrLn "Example: 'exported.json'"
      putStr "Filename: "
      path <- getLine
      if null path
        then do
          putStrLn "Error: Filename cannot be empty."
          return state
        else do
          exportGraphToJSON graph path
          putStrLn ("Success! Graph exported to '" ++ path ++ "'.")
          logMsg Info ("Graph exported to " ++ path)
          return state

findPath :: AppState -> IO AppState
findPath state = do
  case currentGraph state of
    Nothing -> do
      putStrLn "\n--- Find Shortest Path ---"
      putStrLn "Error: No graph is currently loaded."
      putStrLn "Please load a graph first (option 1)."
      return state
    Just graph -> do
      putStrLn "\n--- Find Shortest Path ---"
      putStrLn "Enter the names of stations exactly as they appear in the list."
      putStrLn "Use option 4 to see all available stations."
      putStrLn "Example: 'Khreshchatyk' or 'Maidan Nezalezhnosti'"
      putStr "Start station: "
      start <- getLine
      if null start
        then do
          putStrLn "Error: Start station cannot be empty."
          return state
        else do
          putStr "Target station: "
          target <- getLine
          if null target
            then do
              putStrLn "Error: Target station cannot be empty."
              return state
            else do
              let startT = T.pack start
                  targetT = T.pack target
              if not (Map.member startT (stations graph))
                then do
                  putStrLn $ "Error: Station '" ++ start ++ "' not found."
                  putStrLn "Use option 4 to see all stations."
                  return state
                else if not (Map.member targetT (stations graph))
                  then do
                    putStrLn $ "Error: Station '" ++ target ++ "' not found."
                    putStrLn "Use option 4 to see all stations."
                    return state
                  else do
                    putStrLn "Calculating shortest path..."
                    let result = shortestPath graph startT targetT
                    case result of
                      NoPath -> do
                        putStrLn "Result: No path found between these stations."
                        logMsg Warning "No path found"
                        return state
                      Path nodes w -> do
                        putStrLn "Result: Path found!"
                        putStrLn $ "Distance: " ++ showWeight w ++ " km"
                        putStrLn "Route:"
                        mapM_ (\n -> putStrLn $ "  -> " ++ T.unpack n) nodes
                        logMsg Info ("Path found from " ++ start ++ " to " ++ target)
                        return (state { lastPathResult = Just result })

listStations :: AppState -> IO AppState
listStations state = do
  case currentGraph state of
    Nothing -> do
      putStrLn "\n--- List All Stations ---"
      putStrLn "Error: No graph is currently loaded."
      putStrLn "Please load a graph first (option 1)."
      return state
    Just graph -> do
      putStrLn "\n--- List All Stations ---"
      let stationsList = Map.keys (stations graph)
      putStrLn $ "Total stations: " ++ show (length stationsList)
      putStrLn "----------------------------------------"
      mapM_ (\s -> putStrLn $ "  - " ++ T.unpack s) stationsList
      putStrLn "----------------------------------------"
      return state

findStation :: AppState -> IO AppState
findStation state = do
  case currentGraph state of
    Nothing -> do
      putStrLn "\n--- Find Station ---"
      putStrLn "Error: No graph is currently loaded."
      putStrLn "Please load a graph first (option 1)."
      return state
    Just graph -> do
      putStrLn "\n--- Find Station ---"
      putStrLn "Enter a station name or a part of it."
      putStrLn "Example: 'Kresh' will find 'Khreshchatyk'"
      putStr "Search query: "
      query <- getLine
      if null query
        then do
          putStrLn "Error: Search query cannot be empty."
          return state
        else do
          let results = filter (\s -> T.isInfixOf (T.pack query) s) (Map.keys (stations graph))
          if null results
            then putStrLn ("No stations found matching '" ++ query ++ "'.")
            else do
              putStrLn $ "Found " ++ show (length results) ++ " station(s):"
              mapM_ (\s -> putStrLn $ "  - " ++ T.unpack s) results
          return state

visualizeGraph :: AppState -> IO AppState
visualizeGraph state = do
  case currentGraph state of
    Nothing -> do
      putStrLn "\n--- Visualize Graph ---"
      putStrLn "Error: No graph is currently loaded."
      putStrLn "Please load a graph first (option 1)."
      return state
    Just graph -> do
      putStrLn "\n--- Visualize Graph ---"
      putStrLn "Enter the filename for the PNG image."
      putStrLn "Example: 'my_graph.png'"
      putStr "PNG filename: "
      path <- getLine
      if null path
        then do
          putStrLn "Error: Filename cannot be empty."
          return state
        else do
          putStrLn "Generating visualization... (requires Graphviz installed)"
          renderGraphToPNG graph path
          logMsg Info ("Graph visualization saved to " ++ path)
          return state

visualizePath :: AppState -> IO AppState
visualizePath state = do
  case (currentGraph state, lastPathResult state) of
    (Nothing, _) -> do
      putStrLn "\n--- Visualize Path ---"
      putStrLn "Error: No graph is currently loaded."
      putStrLn "Please load a graph first (option 1)."
      return state
    (_, Nothing) -> do
      putStrLn "\n--- Visualize Path ---"
      putStrLn "Error: No path has been found yet."
      putStrLn "Please find a path first (option 3)."
      return state
    (Just graph, Just path) -> do
      putStrLn "\n--- Visualize Path ---"
      putStrLn "Enter the filename for the PNG image."
      putStrLn "Example: 'path.png'"
      putStr "PNG filename: "
      pathStr <- getLine
      if null pathStr
        then do
          putStrLn "Error: Filename cannot be empty."
          return state
        else do
          putStrLn "Generating path visualization... (requires Graphviz installed)"
          renderPathToPNG graph path pathStr
          logMsg Info ("Path visualization saved to " ++ pathStr)
          return state

viewLog :: IO AppState
viewLog = do
  putStrLn "\n--- View Log ---"
  entries <- readLog
  if null entries
    then putStrLn "Log is empty. No actions have been recorded yet."
    else do
      putStrLn "Last 30 log entries:"
      putStrLn "----------------------------------------"
      mapM_ putStrLn (take 30 entries)
      putStrLn "----------------------------------------"
  return (AppState Nothing Nothing)

clearLogHandler :: IO AppState
clearLogHandler = do
  putStrLn "\n--- Clear Log ---"
  putStrLn "This will permanently delete all log entries."
  putStr "Are you sure you want to clear the log? (y/n): "
  confirm <- getLine
  let lowerConfirm = map toLower confirm
  if lowerConfirm == "y" || lowerConfirm == "yes"
    then do
      clearLog
      putStrLn "Log cleared successfully."
      logMsg Info "Log cleared by user"
    else do
      putStrLn "Operation cancelled."
  return (AppState Nothing Nothing)