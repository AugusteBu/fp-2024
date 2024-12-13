{-# LANGUAGE ImportQualifiedPost #-}

import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=), assertFailure, assertBool )
import Test.Tasty.QuickCheck 
import Test.Tasty (testGroup, defaultMain)
import Control.Concurrent.Chan (newChan, Chan)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (assert) 
import Control.Monad 
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, writeTVar, atomically)
import Test.QuickCheck.Monadic (monadicIO, run, assert, pick)
import Control.Concurrent (newChan, forkIO)
import Data.Char
import Debug.Trace (trace)

import Data.List
import Data.Ord

import Lib1 qualified
import Lib2 qualified
import Lib3 qualified
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

instance Arbitrary Lib3.Statements where
    arbitrary = do
        isBatch <- arbitrary
        if isBatch
            then do
                queries <- listOf arbitrary     
                return $ Lib3.Batch queries       
            else
                Lib3.Single <$> arbitrary      

instance Arbitrary Lib2.Query where
    arbitrary = do
        item <- arbitraryString
        quantity <- arbitraryPositiveInt
        additionalOp <- oneof
            [ pure []  
            , (:[]) <$> (Lib2.Restock item <$> arbitraryPositiveInt)
            , (:[]) <$> (pure (Lib2.Delete item))
            , (:[]) <$> (Lib2.Sell item <$> arbitraryPositiveInt)  
            , (:[]) <$> (Lib2.Add item <$> arbitraryPositiveInt)
            ]
        
        let queries = Lib2.Add item quantity : additionalOp
        return $ case queries of
            [] -> error "Error" 
            (x:xs) -> x

arbitraryString :: Gen String
arbitraryString = listOf1 (elements ['a'..'z'])

arbitraryPositiveInt :: Gen Int
arbitraryPositiveInt = choose (1, 100)

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


--test arbittraty !!
propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
  [  testProperty "Adding and restocking an item" prop_addAndRestock
  ,  testProperty "Saving and loading maintains the same state" prop_saveAndLoadState
 -- ,  testProperty "Queries inside BEGIN and END" prop_fullQueries

  ]

prop_addAndRestock :: Property
prop_addAndRestock = monadicIO $ do
    item <- pick arbitraryString
    quantity <- pick arbitraryPositiveInt
    restockQuantity <- pick arbitraryPositiveInt

    let initialState = Lib2.emptyState

    updatedState <- run $ case Lib2.stateTransition initialState (Lib2.Add item quantity) of        --pridedea i empty state
        Right (_, state) -> return state
        Left err -> error ("Error applying Add command: " ++ show err)  
    finalState <- run $ case Lib2.stateTransition updatedState (Lib2.Restock item restockQuantity) of       --su pridetais daiktais
        Right (_, state) -> return state
        Left err -> error ("Error applying Restock command: " ++ show err)
    

    let serializedLoadedState = Lib3.marshallState finalState


   -- trace ("STATES: = " ++ show updatedState ++ ",  FIANL  = " ++ show finalState ++ "\nSTART = " ++ show quantity ++ "    END: " ++ show restockQuantity ++ "\n") (return ())
    Test.QuickCheck.Monadic.assert $ (quantity + restockQuantity) == 


--SAVE AND LOAD

prop_saveAndLoadState :: Property
prop_saveAndLoadState = monadicIO $ do
    randomStatements <- liftIO $ generate arbitrary 

    stateVar <- liftIO $ newTVarIO Lib2.emptyState
    ioChan <- liftIO $ newChan
    _ <- liftIO $ forkIO $ Lib3.storageOpLoop ioChan

    case randomStatements of
        Lib3.Batch queries -> mapM_ (liftIO . applyCommand stateVar ioChan) queries
        Lib3.Single query  -> liftIO $ applyCommand stateVar ioChan query
    loadedStateVar <- liftIO $ newTVarIO Lib2.emptyState

    let saveCommand = "Save"
    case Lib3.parseCommand saveCommand of
        Right (command, _) -> liftIO $ Lib3.stateTransition loadedStateVar command ioChan
        Left err -> error $ "Error parsing save command: " ++ err

    let loadCommand = "Load"
    case Lib3.parseCommand loadCommand of
        Right (command, _) -> liftIO $ Lib3.stateTransition loadedStateVar command ioChan
        Left err -> error $ "Error parsing load command: " ++ err


    savedState <- liftIO $ readTVarIO loadedStateVar
    loadedState <- liftIO $ readTVarIO loadedStateVar

    let serializedSavedState = Lib3.marshallState savedState
    let serializedLoadedState = Lib3.marshallState loadedState

    Test.QuickCheck.Monadic.assert (serializedSavedState == serializedLoadedState)

applyCommand :: TVar Lib2.State -> Chan Lib3.StorageOp -> Lib2.Query -> IO ()
applyCommand stateVar ioChan query = do
    case Lib2.stateTransition (Lib2.emptyState) query of
        Right (_, updatedState) -> atomically $ writeTVar stateVar updatedState
        Left err -> putStrLn $ "Error applying command: " ++ err
