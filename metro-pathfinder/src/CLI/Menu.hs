module CLI.Menu (runCLI) where

import Graph.Types
import Graph.Dijkstra
import Graph.Loader (loadGraphFromJSON, exportGraphToJSON)
import Logging.Logger
import qualified Data.Map as Map
import qualified Data.Text as T
import System.IO (hFlush, stdout)

data AppState = AppState
  { currentGraph :: Maybe Graph
  , lastPathResult :: Maybe PathResult
  }

runCLI :: IO ()
runCLI = do
  logMsg Info "Application started"
  loop (AppState Nothing Nothing)

loop :: AppState -> IO ()
loop state = do
  putStrLn "\n=== METRO PATHFINDER ==="
  putStrLn "1. Load graph from JSON"
  putStrLn "2. Export graph to JSON"
  putStrLn "3. Load sample graph (A,B,C)"
  putStrLn "4. Find shortest path"
  putStrLn "5. List all stations"
  putStrLn "6. Find station by name"
  putStrLn "7. View log"
  putStrLn "8. Clear log"
  putStrLn "0. Exit"
  putStr "Choice: "
  hFlush stdout
  choice <- getLine
  case choice of
    "1" -> loadJSON state >>= loop
    "2" -> exportJSON state >>= loop
    "3" -> loadSample state >>= loop
    "4" -> findPath state >>= loop
    "5" -> listStations state >>= loop
    "6" -> findStation state >>= loop
    "7" -> viewLog >>= loop
    "8" -> clearLogHandler >>= loop
    "0" -> do
      logMsg Info "Application exited"
      putStrLn "Goodbye!"
    _ -> do
      putStrLn "Invalid choice"
      loop state

loadJSON :: AppState -> IO AppState
loadJSON state = do
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
          putStrLn $ "Error: " ++ err
          logMsg Error ("Failed to load: " ++ err)
          return state
        Right graph -> do
          putStrLn $ "Graph loaded! Stations: " ++ show (Map.size (stations graph))
          logMsg Info ("Graph loaded from " ++ path)
          return (state { currentGraph = Just graph })

exportJSON :: AppState -> IO AppState
exportJSON state = do
  case currentGraph state of
    Nothing -> do
      putStrLn "No graph loaded. Load a graph first (option 1 or 3)."
      return state
    Just graph -> do
      putStr "Enter path to save JSON file: "
      path <- getLine
      if null path
        then do
          putStrLn "Path cannot be empty"
          return state
        else do
          exportGraphToJSON graph path
          putStrLn $ "Graph exported to " ++ path
          logMsg Info ("Graph exported to " ++ path)
          return state

loadSample :: AppState -> IO AppState
loadSample _ = do
  let stationA = Station (T.pack "A") (T.pack "A") Nothing Nothing
      stationB = Station (T.pack "B") (T.pack "B") Nothing Nothing
      stationC = Station (T.pack "C") (T.pack "C") Nothing Nothing
      stationsMap = Map.fromList [(T.pack "A", stationA), (T.pack "B", stationB), (T.pack "C", stationC)]
      edgesMap = Map.fromList 
        [ (T.pack "A", [(T.pack "B", Weight 5), (T.pack "C", Weight 10)])
        , (T.pack "B", [(T.pack "C", Weight 3)])
        , (T.pack "C", []) ]
  let sampleGraph = Graph stationsMap edgesMap False
  putStrLn "Sample graph loaded: A--5-->B--3-->C"
  logMsg Info "Sample graph loaded"
  return (AppState (Just sampleGraph) Nothing)

findPath :: AppState -> IO AppState
findPath state = do
  case currentGraph state of
    Nothing -> do
      putStrLn "No graph loaded. Choose option 1 or 3 first."
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
          let startT = T.pack start
              targetT = T.pack target
          if not (Map.member startT (stations graph)) || not (Map.member targetT (stations graph))
            then do
              putStrLn "Station not found"
              return state
            else do
              let result = shortestPath graph startT targetT
              case result of
                NoPath -> do
                  putStrLn "No path found"
                  logMsg Warning "No path found"
                  return state
                Path nodes w -> do
                  putStrLn $ "Path found! Distance: " ++ show w
                  putStrLn $ "Route: " ++ show (map T.unpack nodes)
                  logMsg Info ("Path found from " ++ start ++ " to " ++ target)
                  return (state { lastPathResult = Just result })

listStations :: AppState -> IO AppState
listStations state = do
  case currentGraph state of
    Nothing -> do
      putStrLn "No graph loaded. Choose option 1 or 3 first."
      return state
    Just graph -> do
      putStrLn $ "\nTotal stations: " ++ show (Map.size (stations graph))
      putStrLn "----------------------------------------"
      mapM_ (\s -> putStrLn $ "  - " ++ T.unpack s) (Map.keys (stations graph))
      return state

findStation :: AppState -> IO AppState
findStation state = do
  case currentGraph state of
    Nothing -> do
      putStrLn "No graph loaded. Choose option 1 or 3 first."
      return state
    Just graph -> do
      putStr "Enter station name (or part of it): "
      query <- getLine
      if null query
        then do
          putStrLn "Query cannot be empty"
          return state
        else do
          let results = filter (\s -> T.isInfixOf (T.pack query) s) (Map.keys (stations graph))
          if null results
            then putStrLn "No stations found"
            else do
              putStrLn $ "Found " ++ show (length results) ++ " stations:"
              mapM_ (\s -> putStrLn $ "  - " ++ T.unpack s) results
          return state

viewLog :: IO AppState
viewLog = do
  entries <- readLog
  if null entries
    then putStrLn "Log is empty"
    else do
      putStrLn "\n=== LOG ==="
      mapM_ putStrLn (take 30 entries)
      putStrLn "==========="
  return (AppState Nothing Nothing)

clearLogHandler :: IO AppState
clearLogHandler = do
  clearLog
  putStrLn "Log cleared"
  logMsg Info "Log cleared by user"
  return (AppState Nothing Nothing)