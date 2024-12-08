{-# LANGUAGE InstanceSigs #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements
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
            writeFile "save.txt" content
            writeChan responseChan ()
        Load responseChan -> do
            content <- readFile "load.txt"
            writeChan responseChan content



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
    in trace ("parseWhitespace: spaces = " ++ show spaces ++ ", rest = " ++ show rest) $
       Right (spaces, rest)
 -- apple; orange; banana; END    ->    [apple\n, orange\n, banana\n, END\n]?


parseCommand :: String -> Either String (Command, String)
parseCommand input =
  case parseWord input of
    Right ("Save", rest) -> 
      Right (SaveCommand, rest)
    Right ("Load", rest) -> 
      Right (LoadCommand, rest)
    Right ("BEGIN", rest) -> 
      let queries = map trim (splitBySemicolon rest)  --  "apple; banana; END" -> ["apple", "banana", "END"]
      in trace ("parseCommand: queries = " ++ show queries) $
         if not (null queries) && last queries == "END" then
           -- Parse all the queries except the last one (because it's "END").
           case mapM Lib2.parseQuery (init queries) of
             Right parsedQueries -> 
               Right (StatementCommand (Batch parsedQueries), "")
             Left err -> 
               Left $ "Error parsing queries: " ++ err
         else 
           Left "Error parsing queries: Missing or malformed END"    
    Right (_, _) -> 
      case Lib2.parseQuery input of
        Right query -> 
          Right (StatementCommand (Single query), "")
        Left err -> 
          Left $ "Unknown command or invalid query: " ++ err
    Left err -> 
      Left $ "Error parsing command: " ++ err




-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements query =
    let queries = splitBySemicolon query
        parsedResults = map Lib2.parseQuery queries
    in trace ("parseStatements: queries = " ++ show queries ++ ", parsedResults = " ++ show parsedResults) $ 
       if all isRight parsedResults
          then
            let parsedQueries = map (\(Right q) -> q) parsedResults
            in Right (Batch parsedQueries, "")
          else Left "Failed to parse one or more queries in the input."



parseBatch :: String -> Either String ([Lib2.Query], String)
parseBatch input = case parseWord input of
  Right (h, _) -> 
    let dropped = L.dropWhileEnd (== '\n') h in 
        if dropped == "END" then Right ([], "")
        else Left ("Expected END but got " ++ dropped)
  Left e -> Left e



splitBySemicolon :: String -> [String]
splitBySemicolon [] = []
splitBySemicolon input =
  let (part, rest) = span (/= ';') input
  in trace ("splitBySemicolon: part = " ++ show part ++ ", rest = " ++ show rest) $
     case parseWhitespace part of
       Right (_, commandWithoutSpace) -> 
         commandWithoutSpace : case rest of
                          []     -> []
                          (_:xs) -> splitBySemicolon xs
       Left _ -> []



trim :: String -> String
trim = dropWhile C.isSpace . reverse . dropWhile C.isSpace . reverse


isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False


-- parseBatch :: String -> Either String ([Lib2.Query], String)
-- parseBatch input = 
--     let sanitizedInput = L.strip input
--         queries = splitBySemicolon sanitizedInput
--         parsedResults = map Lib2.parseQuery queries
--     in if all isRight parsedResults 
--        then Right (rights parsedResults, "")
--        else Left "Failed to parse one or more queries in the batch"

-- rights :: [Either a b] -> [b]
-- rights = foldr (\x acc -> case x of Right v -> v : acc; _ -> acc) []




-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
-- | Converts program's state into Statements (single or batch query)
marshallState :: Lib2.State -> Statements
marshallState state =
    let names = getNames state
    in if null names
       then Single (Lib2.Check ["No Operations"])
       else Batch (map (createQuery state) names)

-- Get the names of items from the state
getNames :: Lib2.State -> [String]
getNames state =
  let getItemNames items = map fst items
  in (getItemNames (Lib2.writingUtensils state)) ++
     (getItemNames (Lib2.books state)) ++
     (getItemNames (Lib2.artSupplies state)) ++
     (getItemNames (Lib2.otherItems state))

-- Create a query based on the name of an item
createQuery :: Lib2.State -> String -> Lib2.Query
createQuery state name
  | name `elem` map fst (Lib2.writingUtensils state) = Lib2.Add name 1  
  | name `elem` map fst (Lib2.books state)           = Lib2.Add name 1 
  | name `elem` map fst (Lib2.artSupplies state)     = Lib2.Add name 1 
  | otherwise                                        = Lib2.Delete name

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
-- | Renders Statements into a String which can be parsed back by parseStatements
renderStatements :: Statements -> String
renderStatements (Single query) = renderQuery query
renderStatements (Batch queries) = L.intercalate ";\n" (map renderQuery queries)


testRenderParse :: Statements -> Bool
testRenderParse stmts =
    case parseStatements (renderStatements stmts) of
        Right (parsed, _) -> stmts == parsed
        Left err -> trace ("Parse error: " ++ show err) False

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
  let 
      updatedWritingUtensils = updateFunc (Lib2.writingUtensils state) item quantity
      updatedBooks = updateFunc (Lib2.books state) item quantity
      updatedArtSupplies = updateFunc (Lib2.artSupplies state) item quantity
      updatedOtherItems = updateFunc (Lib2.otherItems state) item quantity
  in state { Lib2.writingUtensils = updatedWritingUtensils
           , Lib2.books = updatedBooks
           , Lib2.artSupplies = updatedArtSupplies
           , Lib2.otherItems = updatedOtherItems
           }
           
deleteItemFromState :: Lib2.State -> String -> Lib2.State
deleteItemFromState state item =
  let updatedWritingUtensils = deleteItemFromList (Lib2.writingUtensils state) item
      updatedBooks = deleteItemFromList (Lib2.books state) item
      updatedArtSupplies = deleteItemFromList (Lib2.artSupplies state) item
      updatedOtherItems = deleteItemFromList (Lib2.otherItems state) item
  in state { Lib2.writingUtensils = updatedWritingUtensils
           , Lib2.books = updatedBooks
           , Lib2.artSupplies = updatedArtSupplies
           , Lib2.otherItems = updatedOtherItems
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

stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp -> IO (Either String (Maybe String, String))
stateTransition stateVar SaveCommand ioChan = do
  responseChan <- newChan
  writeChan ioChan (Save "state.txt" responseChan)
  _ <- readChan responseChan
  currentState <- atomically $ readTVar stateVar
  return $ Right (Just "State saved", show currentState)

stateTransition stateVar LoadCommand ioChan = do
  responseChan <- newChan
  writeChan ioChan (Load responseChan)
  fileContent <- readChan responseChan
  
  let eitherFileContent = Right fileContent

  either 
    (\err -> return $ Left ("Error loading file: " ++ err))  
    (\content -> do                                   
      let newState = parseStateFromFile content
      atomically $ writeTVar stateVar newState
      currentState <- atomically $ readTVar stateVar
      return $ Right (Just "State loaded", show currentState)
    ) eitherFileContent



stateTransition stateVar (StatementCommand statements) ioChan = do
  newState <- atomically $ do
    oldState <- readTVar stateVar
    let updatedState = applyStatementsToState oldState statements
    writeTVar stateVar updatedState
    return updatedState
  return $ Right (Just "State updated", show newState)




parseStateFromFile :: String -> Lib2.State
parseStateFromFile content = 
  error "parseStateFromFile not implemented"


