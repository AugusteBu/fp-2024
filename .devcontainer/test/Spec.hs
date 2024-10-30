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
           [ testCase "Valid Add command" $ 
                let input = "Add Pencil 10"
                    expected = Right (Lib2.Add "Pencil" 10)
                in Lib2.parseQuery input @?= expected

            , testCase "Valid Delete command" $ 
                let input = "Delete Paint"
                    expected = Right (Lib2.Delete "Paint")
                in Lib2.parseQuery input @?= expected

            , testCase "Valid Restock command" $ 
                let input = "Restock Eraser 20"
                    expected = Right (Lib2.Restock "Eraser" 20)
                in Lib2.parseQuery input @?= expected

            , testCase "Valid Sell command" $ 
                let input = "Sell Marker 2"
                    expected = Right (Lib2.Sell "Marker" 2)
                in Lib2.parseQuery input @?= expected

            , testCase "Valid Check command" $ 
                let input = "Check Paper"
                    expected = Right (Lib2.Check ["Paper"])
                in Lib2.parseQuery input @?= expected

            , testCase "Unrecognized command" $ 
                let input = "Buy Pencil 10"
                    expected = Left "Unrecognized command"
                in Lib2.parseQuery input @?= expected

            ]