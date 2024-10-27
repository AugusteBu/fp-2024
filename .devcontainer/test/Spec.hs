{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Lib1 qualified
import Lib2 qualified 

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
            [ testCase "parseQuery - valid Add command" $ 
            let input = "Add Pencil 10"
                expected = Right (Lib2.Add "Pencil" 10)
            in Lib2.parseQuery input @?= expected

        , testCase "parseQuery - valid Delete command" $ 
            let input = "Delete Paint"
                expected = Right (Lib2.Delete "Paint")
            in Lib2.parseQuery input @?= expected

        , testCase "parseQuery - valid Restock command" $ 
            let input = "Restock Eraser 20"
                expected = Right (Lib2.Restock "Eraser" 20)
            in Lib2.parseQuery input @?= expected

        , testCase "parseQuery - valid Sell command" $ 
            let input = "Sell Marker 2"
                expected = Right (Lib2.Sell "Marker" 2)
            in Lib2.parseQuery input @?= expected

        , testCase "parseQuery - valid Check command" $ 
            let input = "Check Paper"
                expected = Right (Lib2.Check "Paper")
            in Lib2.parseQuery input @?= expected

        , testCase "parseQuery - unrecognized command" $ 
            let input = "Buy Pencil 10"
                expected = Left "Unrecognized command"
            in Lib2.parseQuery input @?= expected

        , testCase "parseQuery - invalid format for Add" $ 
            let input = "Add 10"
                expected = Left "Expected a word"
            in Lib2.parseQuery input @?= expected

        , testCase "stateTransition - valid Add command" $ do
            let initialState = Lib2.emptyState
                command = Lib2.Add "Pencil" 10
                result = Lib2.stateTransition initialState command
                expectedState = Lib2.State
                    [("graphite", 0), ("mechanical", 0), ("ballpoint", 0), ("fountain", 0), ("gel", 0)]  
                    [("fiction", 0), ("mystery", 0), ("poetry", 0)] 
                    [("brush", 0), ("canvases", 0), ("oil", 0), ("watercolors", 0), ("acrylics", 0), ("sketchpads", 0), ("notebooks", 0)] 
                    [("Pencil", 10)]  
                expected = Right ([], expectedState)
            result @?= expected  
--------------------------------------------------------------
        , testCase "stateTransition - Restock command for art supply item" $ do
            let initialState = Lib2.emptyState
                command = Lib2.Restock "brush" 5
                result = Lib2.stateTransition initialState command
                expectedState = initialState { Lib2.artSupplies = [("brush", 5), ("canvases", 0), ("oil", 0), ("watercolors", 0), ("acrylics", 0), ("sketchpads", 0), ("notebooks", 0)] }
                expected = Right ([], expectedState)
            result @?= expected

        , testCase "stateTransition - Delete command for writing utensil" $ do
            let initialState = Lib2.emptyState { Lib2.writingUtensils = [("graphite", 15), ("mechanical", 0), ("ballpoint", 0), ("fountain", 0), ("gel", 0)] }
                command = Lib2.Delete "graphite" 
                result = Lib2.stateTransition initialState command
                expectedState = initialState { Lib2.writingUtensils = [("mechanical", 0), ("ballpoint", 0), ("fountain", 0), ("gel", 0)] }
                expected = Right ([], expectedState)
            result @?= expected

        , testCase "stateTransition - Sell command with exact stock for book item" $ do
            let initialState = Lib2.emptyState { Lib2.books = [("fiction", 10), ("mystery", 0), ("poetry", 0)] }
                command = Lib2.Sell "fiction" 10
                result = Lib2.stateTransition initialState command
                expectedState = initialState { Lib2.books = [("fiction", 0), ("mystery", 0), ("poetry", 0)] }
                expected = Right ([], expectedState)
            result @?= expected

        , testCase "stateTransition - Check command for existing art supply item" $ do
            let initialState = Lib2.State
                    [("graphite", 0)]
                    [("fiction", 0)]
                    [("brush", 10)]
                    []
                command = Lib2.Check "brush"
                result = Lib2.stateTransition initialState command
                expected = Right (["brush has quantity 10"], initialState)
            result @?= expected

        , testCase "stateTransition - Check command for nonexistent item" $ 
            let initialState = Lib2.emptyState
                command = Lib2.Check "NonExistentItem"
                expected = Left "Error: Item NonExistentItem not found in storage"
            in Lib2.stateTransition initialState command @?= expected

        , testCase "stateTransition - Sell command with insufficient stock for art supply" $ do
            let initialState = Lib2.State
                    [("graphite", 0)]
                    [("fiction", 0)]
                    [("brush", 1)]
                    [("Marker", 1)]
                command = Lib2.Sell "brush" 2
                result = Lib2.stateTransition initialState command
                expected = Left "Error: Item brush not found"
            result @?= expected
        ]