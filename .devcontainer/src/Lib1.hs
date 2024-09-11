module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = ["Add" <Storage> <Item>, "Delete" <Storage> <Item>, "Restock" <Storage> <Item>, "Sell" <Item> | "Exit"]

