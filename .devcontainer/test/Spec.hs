{-# LANGUAGE ImportQualifiedPost #-}

import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

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
                    expected = Left "Unrecognized command: Buy"
                in Lib2.parseQuery input @?= expected

            ]
propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
[ QC.testProperty "Adding an item and restocking results in the correct quantity" $ 
      \(item, quantity, restockQuantity) -> 
        let initialState = Lib2.parseAdd [] item quantity  
            updatedState = Lib2.parseAdd initialState item restockQuantity  
            expectedQuantity = quantity + restockQuantity  
            finalQuantity = fromMaybe 0 (lookup item updatedState) 
        in finalQuantity == expectedQuantity  
    ]
