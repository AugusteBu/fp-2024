module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's REPL.
completions :: [String]
completions = ["Add", "Delete", "Restock", "Sell"]
--completions = ["Add" <Storage> <Item>, "Delete" <Storage> <Item>, "Restock" <Storage> <Item>, "Sell" <Storage> <Item> ]
