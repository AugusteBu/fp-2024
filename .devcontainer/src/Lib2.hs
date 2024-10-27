 {-# LANGUAGE InstanceSigs #-}
 module Lib2
     ( Query(..),
     parseQuery,
     State(..),
     emptyState,
     stateTransition
     ) where

    -- stack run and stack test
    --stack run exec fp-2024 twp?s
    --stack exec  fp-2024-two
import qualified Data.Char as C
    --import qualified Data.List as L

data Query 
    = Add String Int 
    | Delete String 
    | Restock String Int 
    | Sell String Int 
    | Check [String]
    deriving(Eq, Show)

data Storage = ItemType Item | StorageItem Storage Item deriving(Eq, Show)
data Item 
    = WritingUtensils WritingUtensilType 
    | Books BookType 
    | ArtSupplies ArtSupplyType         
    deriving (Eq, Show)

data WritingUtensilType = Pencils PencilType | Pens PenType | Brushes deriving (Eq, Show)
data PencilType = Graphite | Mechanical deriving (Eq, Show)
data PenType = Ballpoint | Fountain | Gel deriving (Eq, Show)
data BookType = Mystery | Fiction | Poetry deriving (Eq, Show)
data ArtSupplyType = Canvases | Paints PaintType | Sketchpads | Notebooks deriving (Eq, Show)
data PaintType = Oil | Watercolors | Acrylics deriving (Eq, Show)


data State = State {
writingUtensils :: [(String, Int)],
    books :: [(String, Int)],
    artSupplies :: [(String, Int)],
    otherItems :: [(String, Int)]
} deriving (Show)

writingItems :: [String]
writingItems = ["graphite", "mechanical", "ballpoint", "fountain", "gel"]

bookItems :: [String]
bookItems = ["fiction", "mystery", "poetry"]

artItems :: [String]
artItems = ["brush", "canvases", "oil", "watercolors", "acrylics", "sketchpads", "notebooks"]

emptyState :: State
emptyState = State {
     writingUtensils = [(item, 0) | item <- writingItems], 
     books = [(item, 0) | item <- bookItems],              
     artSupplies = [(item, 0) | item <- artItems],         
     otherItems = []                                       
}


type Parser a = String -> Either String (a, String)


-- Parse a single word (sequence of letters)
parseWord :: Parser String
parseWord input =
    let (myword, rest) = span C.isLetter input
    in if null myword
    then Left "Expected a word"
    else Right (myword, rest)

parseInt :: Parser Int
parseInt input =
    let (digits, rest) = span C.isDigit input
    in if null digits
    then Left "Expected an integer"
    else Right (read digits, rest)

parseWhitespace :: Parser String
parseWhitespace input = 
    let (spaces, rest) = span (== ' ') input
    in Right (spaces, rest) 
parseQuery :: String -> Either String Query
parseQuery input =
    case parseWord input of
        Left err -> Left err
        Right (command, rest1) -> 
            case parseWhitespace rest1 of
                Left err -> Left err
                Right (_, rest2) -> 
                    case command of
                        "Check" -> parseCheckItems rest2
                        "Delete" -> 
                            case parseWord rest2 of
                                Left err -> Left err
                                Right (item, rest3) -> 
                                    case parseWhitespace rest3 of
                                        Left err -> Left err
                                        Right (_, rest4) -> 
                                            Right (Delete item)
                        _ -> 
                            case parseWord rest2 of
                                Left err -> Left err
                                Right (item, rest3) -> 
                                    case parseWhitespace rest3 of
                                        Left err -> Left err
                                        Right (_, rest4) -> 
                                            case parseInt rest4 of
                                                Left _ -> Left "Parse error: expected quantity after item"
                                                Right (quantity, _) -> 
                                                    case command of
                                                        "Add"     -> Right (Add item quantity)
                                                        "Restock" -> Right (Restock item quantity)
                                                        "Sell"    -> Right (Sell item quantity)
                                                        _         -> Left "Unrecognized command"

-- Helper function to parse multiple items for Check command
parseCheckItems :: String -> Either String Query
parseCheckItems input =
    let items = words input  -- splits the remaining input into individual words
    in if null items
       then Left "Parse error: expected at least one item for Check"
       else Right (Check items)

-----------------------------------------------------------------------------------------------------------------------------------------------------------
type Parser1 a = String -> Either String a

-- The 'or' parser combines two parsers
orParser :: Parser1 a -> Parser1 a -> Parser1 a
orParser p1 p2 input =
    case p1 input of
        Right result -> Right result  
        Left _       -> p2 input      

-- wich category belong to
parseWritingUtensil :: Parser1 String
parseWritingUtensil input =
    if isWritingUtensil input
    then Right input
    else Left "Not a writing utensil"

parseBook :: Parser1 String
parseBook input =
    if isBook input
    then Right input
    else Left "Not a book"

parseArtSupply :: Parser1 String
parseArtSupply input =
    if isArtSupply input
    then Right input
    else Left "Not an art supply"

parseOtherItems :: Parser1 String
parseOtherItems input =
    if isOtherItem input
    then Right input
    else Left "Not an other item"

--determine category
parseItem :: String -> Either String String
parseItem input = orParser (orParser parseWritingUtensil (orParser parseBook parseArtSupply)) parseOtherItems input

--add 
add :: State -> String -> Int -> Either String State
add currentState itemStr quantityInt =
    if itemExists (writingUtensils currentState) itemStr || 
       itemExists (books currentState) itemStr || 
       itemExists (artSupplies currentState) itemStr || 
       itemExists (otherItems currentState) itemStr 
    then 
        Left "Item already exists. Use restock to increase quantity."
    else 
        Right $ addToCategory currentState itemStr quantityInt

-- Add the item to the correct category
addToCategory :: State -> String -> Int -> State
addToCategory currentState itemStr quantityInt
    | isWritingUtensil itemStr = currentState { writingUtensils = addItem (writingUtensils currentState) itemStr quantityInt }
    | isBook itemStr           = currentState { books = addItem (books currentState) itemStr quantityInt }
    | isArtSupply itemStr      = currentState { artSupplies = addItem (artSupplies currentState) itemStr quantityInt }
    | otherwise                = currentState { otherItems = addItem (otherItems currentState) itemStr quantityInt }

-- Function to add an item to the specified list
addItem :: [(String, Int)] -> String -> Int -> [(String, Int)]
addItem currentItems itemStr quantityInt =
    let existingItem = lookup itemStr currentItems
        newQuantity = case existingItem of
            Just qty -> qty + quantityInt  -- Increment existing quantity
            Nothing  -> quantityInt         -- Start new quantity if not found

        updatedItems = case existingItem of
            Just _  -> map (\(name, qty) -> if name == itemStr then (name, newQuantity) else (name, qty)) currentItems
            Nothing  -> currentItems ++ [(itemStr, quantityInt)]  -- Append if not found

    in updatedItems 

--check item belong to specific category
isWritingUtensil :: String -> Bool
isWritingUtensil itemStr = itemStr `elem` ["graphite", "mechanical", "ballpoint", "fountain", "gel"]

isBook :: String -> Bool
isBook itemStr = itemStr `elem` ["fiction", "mystery", "poetry"]

isArtSupply :: String -> Bool
isArtSupply itemStr = itemStr `elem` ["brush", "canvases", "oil", "watercolors", "acrylics", "sketchpads", "notebooks"]

isOtherItem :: String -> Bool
isOtherItem itemStr = True  

-- if item already exists
itemExists :: [(String, Int)] -> String -> Bool
itemExists [] _ = False
itemExists ((name, _) : rest) itemStr = name == itemStr || itemExists rest itemStr

-------------------------------------------------DELETE---------------------------------------------------------------------------------------------------------
    
delete :: State -> String -> State
delete state itemName = 
        State { writingUtensils = newWritingUtensils,
                books = newBooks,
                artSupplies = newArtSupplies,
                otherItems = newOtherItems }
        where
        newWritingUtensils = deleteFromList (writingUtensils state) itemName
        newBooks = deleteFromList (books state) itemName
        newArtSupplies = deleteFromList (artSupplies state) itemName
        newOtherItems = deleteFromList (otherItems state) itemName

    -- Helper function to delete an item from a list of items
deleteFromList :: [(String, Int)] -> String -> [(String, Int)]
deleteFromList items itemName = filter (\(name, _) -> name /= itemName) items


    -- -- Function to help remove an item
    -- removeItem :: [(String, Int)] -> [(String, Int)] -> String -> [(String, Int)]
    -- removeItem [] newList _ = newList  -- If no items left, return the new list
    -- removeItem ((name, count):rest) newList itemName
    --     | name == itemName = newList ++ rest  -- If item matches, skip it
    --     | otherwise = removeItem rest (newList ++ [(name, count)]) itemName 

-------------------------------------------------------------------SELL-----------------------------------------------------------
-- Sell function
sell :: State -> String -> Int -> Either String State
sell state itemName quantity =
    let writingQty = lookup itemName (writingUtensils state) 
        bookQty = lookup itemName (books state)             
        artQty = lookup itemName (artSupplies state)       
        otherQty = lookup itemName (otherItems state)       

        -- Determine the current quantity based on where the item is found
        currentQuantity = case writingQty of
            Just qty -> qty  
            Nothing -> case bookQty of
                Just qty -> qty  
                Nothing -> case artQty of
                    Just qty -> qty 
                    Nothing -> case otherQty of
                        Just qty -> qty  
                        Nothing -> 0 

    in if currentQuantity == 0 
       then Left "Item not found."
       else if currentQuantity < quantity 
            then Left "Not enough quantity to sell." 
            else 
                let newQuantity = currentQuantity - quantity  
                    updatedWritingUtensils = if writingQty /= Nothing
                                              then map (\(name, qty) -> if name == itemName then (name, newQuantity) else (name, qty)) (writingUtensils state)
                                              else writingUtensils state
                    updatedBooks = if bookQty /= Nothing
                                   then map (\(name, qty) -> if name == itemName then (name, newQuantity) else (name, qty)) (books state)
                                   else books state
                    updatedArtSupplies = if artQty /= Nothing
                                         then map (\(name, qty) -> if name == itemName then (name, newQuantity) else (name, qty)) (artSupplies state)
                                         else artSupplies state
                    updatedOtherItems = if otherQty /= Nothing
                                        then map (\(name, qty) -> if name == itemName then (name, newQuantity) else (name, qty)) (otherItems state)
                                        else otherItems state
                in Right $ state { writingUtensils = updatedWritingUtensils,
                                   books = updatedBooks,
                                   artSupplies = updatedArtSupplies,
                                   otherItems = updatedOtherItems }  


        --------------------------------CHECK-----------------------------------------------------------
check :: State -> String -> String
check state itemName = 
        let 
            -- Look up the item in each category
            itemInWritingUtensils = lookup itemName (writingUtensils state)
            itemInBooks = lookup itemName (books state)
            itemInArtSupplies = lookup itemName (artSupplies state)
            itemInOtherItems = lookup itemName (otherItems state)

            itemCount = case (itemInWritingUtensils, itemInBooks, itemInArtSupplies, itemInOtherItems) of
                (Just count, _, _, _) -> formatMessage itemName count "Writing Utensil"
                (_, Just count, _, _) -> formatMessage itemName count "Book"
                (_, _, Just count, _) -> formatMessage itemName count "Art Supply"
                (_, _, _, Just count) -> formatMessage itemName count "Other Item"
                _ -> "Item: " ++ itemName ++ " not found."
            
            formatMessage :: String -> Int -> String -> String
            formatMessage name count category = "Item: " ++ name ++ ", Count: " ++ show count ++ " (" ++ category ++ ")"
        
        in itemCount


    ------------------------------------------------------------RESTOCK-------------------------------------
restock :: String -> Int -> State -> Either String ([String], State)
restock items quantities state =
        let 
            -- Helper function to update a specific items quantity if found
            updateItem :: String -> Int -> [(String, Int)] -> [(String, Int)]
            updateItem itm qty lst = 
                case lookup itm lst of
                    Just currentQty -> (itm, currentQty + qty) : filter (\(i, _) -> i /= itm) lst
                    Nothing         -> lst  --if found not, nothing changes

            -- Apply updateItem to each category list in the state
            updatedWritingUtensils = updateItem items quantities (writingUtensils state)
            updatedBooks           = updateItem items quantities (books state)
            updatedArtSupplies     = updateItem items quantities (artSupplies state)
            updatedOtherItems      = updateItem items quantities (otherItems state)

            -- Check if any category was actually updated
            updated = updatedWritingUtensils /= writingUtensils state ||
                    updatedBooks /= books state ||
                    updatedArtSupplies /= artSupplies state ||
                    updatedOtherItems /= otherItems state

        in if not updated
        then Left ("Item \"" ++ items ++ "\" not found in any category. No items restocked.")
        else Right (["Restocked " ++ show quantities ++ " of " ++ items], state {
                    writingUtensils = updatedWritingUtensils,
                    books = updatedBooks,
                    artSupplies = updatedArtSupplies,
                    otherItems = updatedOtherItems
                })



--------------------------------------------------STATE TRANSITIONS-----------------------------------------------------------

stateTransition :: State -> Query -> Either String ([String], State)
stateTransition currentState (Add item quantity) =
    case add currentState item quantity of
        Right updatedState -> Right ([], updatedState)
        Left err -> Left err

stateTransition state (Delete itemName) =
    let doesItemExist items = any (\(name, _) -> name == items) 
            (writingUtensils state ++ books state ++ artSupplies state ++ otherItems state)
    in if doesItemExist itemName
        then let updatedState = delete state itemName
             in Right ([], updatedState)
        else Left "Item not found"

stateTransition state (Restock itemStr quantityInt) =
    case restock itemStr quantityInt state of  
        Left err -> Left err
        Right (_, newState) -> Right ([], newState)

----------------------------------------------------------------------------------------

stateTransition state (Sell item quantity) =
    -- Check book first
    case lookup item (books state) of
        Just stock | stock >= quantity -> 
            let updatedBooks = map (\(i, s) -> if i == item then (i, s - quantity) else (i, s)) (books state)
                updatedState = state { books = updatedBooks }
            in Right ([], updatedState)  -- updated state
        Just stock -> 
            Left $ "Not enough stock for " ++ item
        Nothing -> 
            -- If not found in books, check writing utensils
            case lookup item (writingUtensils state) of
                Just stock | stock >= quantity -> 
                    let updatedWritingUtensils = map (\(i, s) -> if i == item then (i, s - quantity) else (i, s)) (writingUtensils state)
                        updatedState = state { writingUtensils = updatedWritingUtensils }
                    in Right ([], updatedState)
                Just stock -> 
                    Left $ "Not enough stock for " ++ item
                Nothing -> 
                    -- If not found writing utensils, check art supplies
                    case lookup item (artSupplies state) of
                        Just stock | stock >= quantity -> 
                            let updatedArtSupplies = map (\(i, s) -> if i == item then (i, s - quantity) else (i, s)) (artSupplies state)
                                updatedState = state { artSupplies = updatedArtSupplies }
                            in Right ([], updatedState)
                        Just stock -> 
                            Left $ "Not enough stock for " ++ item
                        Nothing -> 
                            -- If not found in art supplies, check other items
                            case lookup item (otherItems state) of
                                Just stock | stock >= quantity -> 
                                    let updatedOtherItems = map (\(i, s) -> if i == item then (i, s - quantity) else (i, s)) (otherItems state)
                                        updatedState = state { otherItems = updatedOtherItems }
                                    in Right ([], updatedState)
                                Just stock -> 
                                    Left $ "Not enough stock for " ++ item
                                Nothing -> 
                                    -- Final case if not found anywhere
                                    Left $ "Item " ++ item ++ " not found."

------------------------------------------------------------------------------------------------

stateTransition state (Check itemNames) = 
    let checkItem itemName =
            case lookup itemName (writingUtensils state ++ books state ++ artSupplies state ++ otherItems state) of
                Just quantity -> Right (itemName ++ " : " ++ show quantity)
                Nothing -> Left $ "Item " ++ itemName ++ " not found in storage"
        results = map checkItem itemNames
        (foundItems, errors) = foldr (\x (items, errs) ->
            case x of
                Right item -> (item : items, errs)
                Left err -> (items, err : errs)
            ) ([], []) results
    in if null errors
       then Right (foundItems, state)
       else Left (unlines errors)

instance Eq State where
         (State wu1 b1 a1 o1) == (State wu2 b2 a2 o2) =
             wu1 == wu2 && b1 == b2 && a1 == a2 && o1 == o2

    -- data State = State 
    --     {
    --     writingUtensilState :: WritingUtensilState,
    --     artSupplyState :: ArtSupplyState,
    --     bookState :: BookState 
    --     } deriving Show

    -- data WritingUtensilState = WritingUtensilState
    --     { graphiteCount       :: Int
    --     , mechanicalCount     :: Int
    --     , ballpointCount      :: Int
    --     , fountainCount       :: Int
    --     , gelCount            :: Int
    --     , brushCount          :: Int
    -- } deriving Show
    -- data ArtSupplyState = ArtSupplyState
    --     { canvasesCount       :: Int
    --     , oilPaintsCount      :: Int
    --     , watercolorsCount    :: Int
    --     , acrylicsCount       :: Int
    --     , sketchpadsCount     :: Int
    --     , notebooksCount      :: Int
    --     } deriving Show
    -- data BookState = BookState
    --     { mysteryCount        :: Int
    --     , fictionCount        :: Int
    --     , poetryCount         :: Int
    --     } deriving Show
