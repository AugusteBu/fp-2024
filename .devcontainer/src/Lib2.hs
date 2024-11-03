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
    --stack exec  fp2024-two
import qualified Data.Char as C
import qualified Data.List as L

--kelias komandas i viena eilute
--reiks issaugoti state programos, saugosim oer komandsas per komandas, reiks generuoti komandas
--atgamins?

data Query 
    = Add String Int
    | Delete String 
    | Restock String Int 
    | Sell String Int 
    | Check [String]
    deriving(Eq, Show)

--data Storage = ItemType Item | StorageItem Storage Item deriving(Eq, Show)
-- data Item 
--     = WritingUtensils WritingUtensilType 
--     | Books BookType 
--     | ArtSupplies ArtSupplyType         
--     deriving (Eq, Show)

-- data WritingUtensilType = Pencils PencilType | Pens PenType | Brushes deriving (Eq, Show)
-- data PencilType = Graphite | Mechanical deriving (Eq, Show)
-- data PenType = Ballpoint | Fountain | Gel deriving (Eq, Show)
-- data BookType = Mystery | Fiction | Poetry deriving (Eq, Show)
-- data ArtSupplyType = Canvases | Paints PaintType | Sketchpads | Notebooks deriving (Eq, Show)
-- data PaintType = Oil | Watercolors | Acrylics deriving (Eq, Show)

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

or' :: Either String Query -> Either String Query -> Either String Query
or' (Right query) _ = Right query   
or' (Left _) (Right query) = Right query 
or' (Left _) (Left er) = Left (er)   


parseQuery :: String -> Either String Query
parseQuery input =
        or' (parseAdd input)
    (or' (parseDelete input)
    (or' (parseRestock input)
    (or' (parseSell input)
    (parseCheck input))))
    -- case parseAdd input of 
    --     Right addQuery -> Right addQuery
    --     Left _ ->
    --         case parseDelete input of 
    --             Right deleteQuery -> Right deleteQuery 
    --             Left _ -> 
    --                 case parseRestock input of
    --                     Right restockQuery -> Right restockQuery
    --                     Left _ -> 
    --                         case parseSell input of
    --                             Right sellQuery -> Right sellQuery
    --                             Left _ ->
    --                                 case parseCheck input of
    --                                     Right checkQuery -> Right checkQuery
    --                                     Left err -> Left err
                            


parseRestock :: String -> Either String Query
parseRestock input =
    case parseWord input of
        Left err -> Left err
        Right ("Restock", rest) ->
            case parseWhitespace rest of
                Left errr -> Left errr
                Right (_, rest1) -> 
                    case parseWord rest1 of
                        Left errrr -> Left errrr
                        Right (item, qntStr) ->
                            case parseWhitespace qntStr of
                                Left e -> Left e
                                Right (_, qntStrNoSpc) ->
                                    case parseNumber qntStrNoSpc of
                                        Left errrrrr -> Left errrrrr
                                        Right (quantity, _) -> Right (Restock item quantity)
        Right (cmd, _) -> Left $ "Unrecognized command: " ++ cmd

parseSell :: String -> Either String Query
parseSell input =
    case parseWord input of
        Left err -> Left err
        Right ("Sell", rest) ->
            case parseWhitespace rest of
                Left errr -> Left errr
                Right (_, rest1) -> 
                    case parseWord rest1 of
                        Left errrr -> Left errrr
                        Right (item, qntStr) ->
                            case parseWhitespace qntStr of
                                Left e -> Left e
                                Right (_, qntStrNoSpc) ->
                                    case parseNumber qntStrNoSpc of
                                        Left errrrrr -> Left errrrrr
                                        Right (quantity, _) -> Right (Sell item quantity)
        Right (cmd, _) -> Left $ "Unrecognized command: " ++ cmd

parseAdd :: String -> Either String Query
parseAdd input = 
    case parseWord input of                         --"Add", " smth 5"
        Left er -> Left er
        Right ("Add", rest1) -> case parseWhitespace rest1 of
                Left err -> Left err
                Right (_, rest2) -> 
                    case parseWord rest2 of                     --"Add" "smth" " 5"?
                        Left e -> Left e
                        Right (item, rest3) -> case parseWhitespace rest3 of
                            Left err -> Left err
                            Right (_, rest4) -> case parseNumber rest4 of
                                Left asd -> Left asd
                                Right (quantity, _) -> Right(Add item quantity)
        Right (cmd, _) -> Left $ "Unrecognized command: " ++ cmd


-- commandCheck :: String -> String -> Either String Query
-- commandCheck cmd rest =  
--     case cmd of
--         "Check" -> parseCheckItems rest        --
--         "Delete" -> deleteParser rest                                -- Right(Delete rest2)
--         e -> Left e
parseDelete :: String -> Either String Query
parseDelete input = 
    case parseWord input of
        Right ("Delete", rest)  ->        --Delete and item
            case parseWhitespace rest of
            Left e -> Left e
            Right (_, rest1) -> 
                case parseWord rest1 of
                    Right (item, _) -> Right (Delete item)
                    Left err -> Left err     --if more written than item
        Right (cmd, _) -> Left $ "Unrecognized command: " ++ cmd
        Left er -> Left er

--string parser palyginimas kuris patikrina ar prasideda delete add etc
parseCheck :: String -> Either String Query
parseCheck input =
    case parseWord input of
        Left e -> Left e
        Right ("Check", rest) -> case parseWhitespace rest of
            Left e -> Left e
            Right (_, rest1) ->
                let item = parseWords' rest1  
                in if null item
                then Left "expected at least one item for Check"
                else Right (Check item)
        Right (cmd, _) -> Left $ "Unrecognized command: " ++ cmd

----------------------------------------------------------------------------------------------
-- Parse a single word 
parseWord :: Parser String
parseWord input =
    let letters = L.takeWhile C.isLetter input
        rest = L.drop (length letters) input
    in if not (null letters)
        then Right (letters, rest)
        else Left ("Expected a word")

parseNumber :: Parser Int
parseNumber [] = Left "empty input, cannot parse a number"
parseNumber str =
    let
        digits = L.takeWhile C.isDigit str
        rest = drop (length digits) str
    in
        case digits of
            [] -> Left "not a number"
            _ -> Right (read digits, rest)

parseWhitespace :: Parser String
parseWhitespace input = 
    let (spaces, rest) = span (== ' ') input
    in Right (spaces, rest) 

parseWords' :: String -> [String]
parseWords' [] = []
parseWords' s =
    let (word, rest) = span (/= ' ') s  
    in if null word
       then parseWords' (dropWhile (== ' ') rest)  
       else word : parseWords' (dropWhile (== ' ') rest)  

        --     case parseWhitespace rest1 of
        --         Left err -> Left err
        --         Right (_, rest2) -> 

        --            case command of
        --                "Check" -> parseCheckItems rest2        --
        --                "Delete" -> deleteParser rest2                                -- Right(Delete rest2)
        --                _ -> 
        --                     case parseWord rest2 of
        --                         Left err -> Left err
        --                         Right (item, rest3) -> 
        --                             case parseWhitespace rest3 of
        --                                 Left err -> Left err
        --                                 Right (_, rest4) -> 
        --                                     case parseNumber rest4 of
        --                                         Left _ -> Left "expected quantity after item"
        --                                         Right (quantity, _) -> 
        --                                             case command of
        --                                                 "Add" -> Right(Add item quantity) 
        --                                                 "Restock" -> Right (Restock item quantity)
        --                                                 "Sell"    -> Right (Sell item quantity)
        --                                                 _         -> Left "Unrecognized command"
--parseQuery input = 

--parseDelete input =
    --jei input prasidedea su delete case tai return right ("Delete", rest) if not retun nothign or left

-----------------------------------------------------------------------------------------------------------------------------------------------------------
--type Parser1 a = String -> Either String a

--add, checks if value already exists in other categories
add :: State -> String -> Int -> Either String State
add currentState itemStr quantityInt =
            if
                elem' itemStr (writingUtensils currentState) ||
                elem' itemStr (books currentState) ||
                elem' itemStr (artSupplies currentState) ||
                elem' itemStr (otherItems currentState)
            then 
                Left "Item already exists. Use restock to increase quantity."
            else 
                Right $ addToCategory currentState itemStr quantityInt


--Check item in list
elem' :: String -> [(String, Int)] -> Bool
elem' _ [] = False
elem' x ((h,_) : t) = if x == h then True else elem' x t

--adds to category, if used to be in category and added again - adds in old category
addToCategory :: State -> String -> Int -> State
addToCategory currentState itemStr quantityInt =
    if isWritingUtensil itemStr then currentState { writingUtensils = addItem (writingUtensils currentState) itemStr quantityInt }
    else if isBook itemStr then currentState { books = addItem (books currentState) itemStr quantityInt }
    else if isArtSupply itemStr then currentState { artSupplies = addItem (artSupplies currentState) itemStr quantityInt }
    else currentState { otherItems = addItem (otherItems currentState) itemStr quantityInt }

-- Check if an item belongs to a specific category
isWritingUtensil :: String -> Bool
isWritingUtensil itemStr = exists itemStr writingItems

isBook :: String -> Bool
isBook itemStr = exists itemStr bookItems

isArtSupply :: String -> Bool
isArtSupply itemStr =  exists itemStr artItems

exists :: String -> [String] -> Bool
exists _ [] = False
exists x (h : t) = if x == h then True else exists x t

-- Function to add an item to the specified list
addItem :: [(String, Int)] -> String -> Int -> [(String, Int)]
addItem currentItems itemStr quantityInt =
    let existingItem = find itemStr currentItems
        in case existingItem of
            Just _  -> currentItems  
            Nothing -> currentItems ++ [(itemStr, quantityInt)] 

find :: String -> [(String, Int)] -> Maybe Int
find _ [] = Nothing  
find itemStr ((name, qty) : xs) =
    if itemStr == name then Just qty 
        else find itemStr xs 
--


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


deleteFromList :: [(String, Int)] -> String -> [(String, Int)]
deleteFromList items itemName = filter (\(name, _) -> name /= itemName) items


-------------------------------------------------------------------SELL-----------------------------------------------------------
-- Sell function
sell :: State -> String -> Int -> Either String State
sell state itemName quantity =
    let writingQty = find itemName (writingUtensils state) 
        bookQty = find itemName (books state)             
        artQty = find itemName (artSupplies state)       
        otherQty = find itemName (otherItems state)       
        --find quantity
        currentQuantity = case writingQty of
            Just qty -> qty  
            Nothing -> case bookQty of
                Just qty -> qty  
                Nothing -> case artQty of
                    Just qty -> qty 
                    Nothing -> case otherQty of
                        Just qty -> qty  
                        Nothing -> 0 

-- delete quanitity
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
            itemInWritingUtensils = find itemName (writingUtensils state)
            itemInBooks = find itemName (books state)
            itemInArtSupplies = find itemName (artSupplies state)
            itemInOtherItems = find itemName (otherItems state)

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

            updateItem :: String -> Int -> [(String, Int)] -> [(String, Int)]
            updateItem itm qty lst = 
                case find itm lst of
                    Just currentQty -> (itm, currentQty + qty) : filter (\(i, _) -> i /= itm) lst
                    Nothing         -> lst  


            updatedWritingUtensils = updateItem items quantities (writingUtensils state)
            updatedBooks           = updateItem items quantities (books state)
            updatedArtSupplies     = updateItem items quantities (artSupplies state)
            updatedOtherItems      = updateItem items quantities (otherItems state)

 
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
stateTransition currentState (Add items quantity) =
     case add currentState items quantity of
            Left errorMsg -> Left errorMsg
            Right newState -> Right (["Added " ++ show quantity ++ " of " ++ items], newState)

stateTransition currentState (Delete itemName) =
    let newState = delete currentState itemName
    in Right (["Deleted " ++ itemName], newState) 

stateTransition state (Restock itemStr quantityInt) =
    case restock itemStr quantityInt state of  
        Left err -> Left err
        Right (_, newState) -> Right ([], newState)

----------------------------------------------------------------------------------------

stateTransition currentState (Sell item quantity) = 
    case sell currentState item quantity of
            Left errorMsg -> Left errorMsg
            Right newState -> Right (["Sold " ++ show quantity ++ " of " ++ item], newState)
        

------------------------------------------------------------------------------------------------

stateTransition currentState (Check itemNames) = 
    let checkResults = map (check currentState) itemNames  
    in Right (checkResults, currentState) 




-- instance Eq State where
--          (State wu1 b1 a1 o1) == (State wu2 b2 a2 o2) =
--              wu1 == wu2 && b1 == b2 && a1 == a2 && o1 == o2

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
