{-# LANGUAGE InstanceSigs #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    Statements(..),
    Command(..),
    renderStatements,
    renderQuery
    ) where

import qualified Lib2
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad 
import qualified Data.List as L 
import qualified Data.Char as C 
import Debug.Trace (trace)


data StorageOp = Save String (Chan ()) | Load (Chan String)

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)
type Parser a = String -> Either String (a, String)

storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = forever $ do
    op <- readChan chan 
    case op of
        Save content responseChan -> do
            writeFile "state.txt" content
            writeChan responseChan () 

        Load responseChan -> do
            fileContent <- tryReadFile "C:\\Users\\Auguste\\fp-2024\\.devcontainer\\load.txt"
            writeChan responseChan fileContent  

tryReadFile :: FilePath -> IO String
tryReadFile filePath = do
    content <- readFile filePath
    return content

parseWord :: Parser String
parseWord input =
    let letters = L.takeWhile C.isLetter input
        rest = L.drop (length letters) input
    in if not (null letters)
        then Right (letters, rest)
        else Left ("Expected a word")
        
parseWhitespace :: Parser String
parseWhitespace input = 
    let (spaces, rest) = span (== '\n') input
    --in trace ("parseWhitespace: spaces = " ++ show spaces ++ ", rest = " ++ show rest) $
    in   Right (spaces, rest)
 -- apple; orange; banana; END    ->    [apple\n, orange\n, banana\n, END\n]?


parseCommand :: String -> Either String (Command, String)
parseCommand input =
  case parseWord input of
    Right ("Save", rest) -> Right (SaveCommand, rest)
    Right ("Load", rest) -> Right (LoadCommand, rest)
    Right ("BEGIN", rest) -> parseBeginCommand rest
    Right (_, _) -> parseQueryCommand input
    Left err -> Left $ "Error parsing command: " ++ err

parseBeginCommand :: String -> Either String (Command, String)
parseBeginCommand rest =
    let trimmedRest = trim rest
        queries = map trim (splitBySemicolon trimmedRest)
 -- in trace ("parseCommand: queries = " ++ show queries) $
   in    if not (null queries) && last queries == "END" then
         parseBatchCommand (init queries)  
       else
         Left "Error parsing queries: Missing END"

parseBatchCommand :: [String] -> Either String (Command, String)
parseBatchCommand queries =
  case mapM Lib2.parseQuery queries of
    Right parsedQueries -> Right (StatementCommand (Batch parsedQueries), "")
    Left err -> Left $ "Error parsing queries: " ++ err

parseQueryCommand :: String -> Either String (Command, String)
parseQueryCommand input =
  case Lib2.parseQuery input of
    Right query -> Right (StatementCommand (Single query), "")
    Left err -> Left $ "Unknown command or invalid query: " ++ err

parseStatements :: String -> Either String (Statements, String)
parseStatements query =
  let trimmedQuery = trim query
      withoutBegin = removePrefix "BEGIN" trimmedQuery
      withoutEnd = removeSuffix "END" withoutBegin
      queries = filter (not . null) (map trim (splitBySemicolon withoutEnd))
  --in trace ("parseStatements: queries = " ++ show queries) $
  in     case mapM Lib2.parseQuery queries of
         Right parsedQueries -> Right (Batch parsedQueries, "")
         Left err -> Left $ "Failed to parse one or more queries: " ++ show err

removePrefix :: String -> String -> String
removePrefix prefix str
  | prefix `L.isPrefixOf` str = drop (length prefix) str
  | otherwise = str

removeSuffix :: String -> String -> String
removeSuffix suffix str
  | suffix `L.isSuffixOf` str = take (length str - length suffix) str
  | otherwise = str

splitBySemicolon :: String -> [String]
splitBySemicolon [] = []
splitBySemicolon input = 
  let (part, rest) = span (/= ';') input
  in case parseWhitespace part of
       Right (_, commandWithoutSpace) -> 
         trim commandWithoutSpace : case rest of
           []     -> []
           (_:xs) -> splitBySemicolon xs
       Left _ -> []

trim :: String -> String
trim = dropWhile C.isSpace . reverse . dropWhile C.isSpace . reverse

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False



-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
-- | Converts program's state into Statements (single or batch query)
marshallState :: Lib2.State -> Statements 
marshallState currentState =
    let addedItems = getNewItems currentState
        addQueries = map (\(item, quantity) -> Lib2.Add item quantity) addedItems

        restockedItems = getRestockedItems currentState
        restockQueries = map (\(item, quantity) -> Lib2.Restock item quantity) restockedItems

        deletedItems = getDeletedItems currentState
        deleteQueries = map Lib2.Delete deletedItems

        allQueries = addQueries ++ restockQueries ++ deleteQueries
    in if null allQueries
       then Single (Lib2.Check ["No Operations"])  
       else Batch allQueries 

getNewItems :: Lib2.State -> [(String, Int)]
getNewItems state =
    filter (\(item, quantity) -> quantity > 0 && not (isRestockable item)) (getAllItems state)

getRestockedItems :: Lib2.State -> [(String, Int)]
getRestockedItems state =
    filter (\(item, quantity) -> quantity > 0 && isRestockable item) (getAllItems state)

getDeletedItems :: Lib2.State -> [String]
getDeletedItems state =
    let currentItems = map fst (getAllItems state)  
    in filter (\item -> item `notElem` currentItems) restockableItems

getAllItems :: Lib2.State -> [(String, Int)]
getAllItems state = 
    (Lib2.writingUtensils state) ++
    (Lib2.books state) ++
    (Lib2.artSupplies state) ++
    (Lib2.otherItems state)

restockableItems :: [String]
restockableItems = 
    ["graphite", "mechanical", "ballpoint", "fountain", "gel", "fiction", "mystery", "poetry", 
     "brush", "canvases", "oil", "watercolors", "acrylics", "sketchpads", "notebooks"]

isRestockable :: String -> Bool
isRestockable item = item `elem` restockableItems


--EVERY
getNames :: Lib2.State -> [String]
getNames state =
  let getItemNames items = map fst items
  in (getItemNames (Lib2.writingUtensils state)) ++
     (getItemNames (Lib2.books state)) ++
     (getItemNames (Lib2.artSupplies state)) ++
     (getItemNames (Lib2.otherItems state))


-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
-- | Renders Statements into a String which can be parsed back by parseStatements
renderStatements :: Statements -> String
renderStatements (Single query) =
    "BEGIN\n" ++ renderQuery query ++ ";\nEND"
renderStatements (Batch queries) =
    "BEGIN\n" ++ unlines (map (\q -> renderQuery q ++ ";") queries) ++ "END"



renderQuery :: Lib2.Query -> String
renderQuery (Lib2.Add item quantity) = "Add " ++ item ++ " " ++ show quantity
renderQuery (Lib2.Delete item) = "Delete " ++ item
renderQuery (Lib2.Restock item quantity) = "Restock " ++ item ++ " " ++ show quantity
renderQuery (Lib2.Sell item quantity) = "Sell " ++ item ++ " " ++ show quantity
renderQuery (Lib2.Check items) = "Check " ++ L.intercalate ", " items


-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
-- Refactor to avoid repetition in state item updates
-- Update items in state using the provided function (either add or restock)
updateStateItems :: Lib2.State -> String -> Int -> ([(String, Int)] -> String -> Int -> [(String, Int)]) -> Lib2.State
updateStateItems state item quantity updateFunc =
  state { Lib2.writingUtensils = updateFunc (Lib2.writingUtensils state) item quantity
        , Lib2.books = updateFunc (Lib2.books state) item quantity
        , Lib2.artSupplies = updateFunc (Lib2.artSupplies state) item quantity
        , Lib2.otherItems = updateFunc (Lib2.otherItems state) item quantity
        }
           
deleteItemFromState :: Lib2.State -> String -> Lib2.State
deleteItemFromState state item =
  state { Lib2.writingUtensils = deleteItemFromList (Lib2.writingUtensils state) item
        , Lib2.books = deleteItemFromList (Lib2.books state) item
        , Lib2.artSupplies = deleteItemFromList (Lib2.artSupplies state) item
        , Lib2.otherItems = deleteItemFromList (Lib2.otherItems state) item
        }
           
deleteItemFromList :: [(String, Int)] -> String -> [(String, Int)]
deleteItemFromList items item = filter (\(i, _) -> i /= item) items

sellItemFromState :: Lib2.State -> String -> Int -> Lib2.State
sellItemFromState state item quantity
  | isWritingUtensil item = state { Lib2.writingUtensils = sellItemFromList (Lib2.writingUtensils state) item quantity }
  | isBook item = state { Lib2.books = sellItemFromList (Lib2.books state) item quantity }
  | isArtSupply item = state { Lib2.artSupplies = sellItemFromList (Lib2.artSupplies state) item quantity }
  | otherwise = state { Lib2.otherItems = sellItemFromList (Lib2.otherItems state) item quantity }

sellItemFromList :: [(String, Int)] -> String -> Int -> [(String, Int)]
sellItemFromList [] _ _ = [] 
sellItemFromList ((i, q) : rest) item quantityToSell
  | i == item = 
      if q - quantityToSell > 0
      then (i, q - quantityToSell) : sellItemFromList rest item quantityToSell 
      else sellItemFromList rest item quantityToSell 
  | otherwise = (i, q) : sellItemFromList rest item quantityToSell  

isWritingUtensil :: String -> Bool
isWritingUtensil item = item `elem` ["graphite", "mechanical", "ballpoint", "fountain", "gel"]

isBook :: String -> Bool
isBook item = item `elem`  ["fiction", "mystery", "poetry"]

isArtSupply :: String -> Bool
isArtSupply item = item `elem`["brush", "canvases", "oil", "watercolors", "acrylics", "sketchpads", "notebooks"]



addItemToState :: Lib2.State -> String -> Int -> Lib2.State
addItemToState state item quantity
  | isWritingUtensil item = state { Lib2.writingUtensils = addItemToList (Lib2.writingUtensils state) item quantity }
  | isBook item = state { Lib2.books = addItemToList (Lib2.books state) item quantity }
  | isArtSupply item = state { Lib2.artSupplies = addItemToList (Lib2.artSupplies state) item quantity }
  | otherwise = state { Lib2.otherItems = addItemToList (Lib2.otherItems state) item quantity }


restockItemInState :: Lib2.State -> String -> Int -> Lib2.State
restockItemInState state item quantity
  | isWritingUtensil item = state { Lib2.writingUtensils = restockItem (Lib2.writingUtensils state) item quantity }
  | isBook item = state { Lib2.books = restockItem (Lib2.books state) item quantity }
  | isArtSupply item = state { Lib2.artSupplies = restockItem (Lib2.artSupplies state) item quantity }
  | otherwise = state { Lib2.otherItems = restockItem (Lib2.otherItems state) item quantity }

addItemToList :: [(String, Int)] -> String -> Int -> [(String, Int)]
addItemToList items item quantity =
  case lookup item items of
    Just oldQuantity -> map (\(i, q) -> if i == item then (i, q + quantity) else (i, q)) items
    Nothing -> (item, quantity) : items

restockItem :: [(String, Int)] -> String -> Int -> [(String, Int)]
restockItem items item quantity =
  case lookup item items of
    Just oldQuantity -> map (\(i, q) -> if i == item then (i, q + quantity) else (i, q)) items
    Nothing -> (item, quantity) : items


applyStatementsToState :: Lib2.State -> Statements -> Lib2.State
applyStatementsToState oldState (Single query) = applyQueryToState oldState query
applyStatementsToState oldState (Batch queries) = L.foldl' applyQueryToState oldState queries

applyQueryToState :: Lib2.State -> Lib2.Query -> Lib2.State
applyQueryToState state (Lib2.Add item quantity) = addItemToState state item quantity
applyQueryToState state (Lib2.Delete item) = deleteItemFromState state item
applyQueryToState state (Lib2.Restock item quantity) = restockItemInState state item quantity
applyQueryToState state (Lib2.Sell item quantity) = sellItemFromState state item quantity
applyQueryToState state (Lib2.Check _) =
  let
    displayItems items = unlines $ map (\(name, qty) -> name ++ ": " ++ show qty) items
  in
    trace ("Writing Utensils:\n" ++ displayItems (Lib2.writingUtensils state) ++
           "\nBooks:\n" ++ displayItems (Lib2.books state) ++
           "\nArt Supplies:\n" ++ displayItems (Lib2.artSupplies state) ++
           "\nOther Items:\n" ++ displayItems (Lib2.otherItems state)) state


stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp -> IO (Either String (Maybe String))
stateTransition stateVar command ioChan = case command of
    SaveCommand -> do
        currentState <- readTVarIO stateVar
        let serializedState = renderStatements $ marshallState currentState
        responseChan <- newChan
        writeChan ioChan (Save serializedState responseChan)
        readChan responseChan
        return $ Right (Just "State saved successfully.")
    
    LoadCommand -> do
        responseChan <- newChan
        writeChan ioChan (Load responseChan)
        result <- readChan responseChan
        case parseStatements result of
            Right (statements, _) -> do
                let newState = applyStatementsToState Lib2.emptyState statements
                atomically $ writeTVar stateVar newState
                return $ Right (Just "State loaded successfully.")
            Left err -> return $ Left $ "Error loading state: " ++ err
    
    StatementCommand statements -> atomically $ do
        currentState <- readTVar stateVar
        let newState = applyStatementsToState currentState statements
        writeTVar stateVar newState
        return $ Right Nothing
