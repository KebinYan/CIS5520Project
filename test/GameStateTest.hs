module GameStateTest where

import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Monadic
import TestUtils
import GameState
import Phd
import Control.Monad (liftM2)
import Control.Monad.State

testInitializeGrid :: Test
testInitializeGrid = TestCase $ do
    grid <- initializeGrid hard
    let dim = dimension hard
    assertEqual "Grid dimensions match" dim (length (board grid))
    assertEqual "Grid initialized with correct normal candies" 
        (dim * dim) (length (concat (board grid)))

testSplitIntoRows :: Test
testSplitIntoRows = TestCase $ do
    let input = [1..9]
        result = splitIntoRows 3 input
    assertEqual "Split list into rows of 3" [[1,2,3],[4,5,6],[7,8,9]] result

testGetCurrentGrid :: Test
testGetCurrentGrid = TestCase $ do
    let grid = initialGrid
        initialState = GameState grid easy Nothing 50 0
    currentGrid <- evalStateT getCurrentGrid initialState
    assertEqual "Get current grid" grid currentGrid

testUpdateGridState :: Test
testUpdateGridState = TestCase $ do
    let grid = initialGrid
        initialState = GameState grid easy Nothing 50 0
        newGrid = crushableGrid
    nextState <- execStateT (updateGridState newGrid) initialState
    assertEqual "Grid updated" newGrid (currentGrid nextState)
    assertEqual "Remaining steps decremented" 49 (remainingSteps nextState)
    assertEqual "Last grid saved" (Just grid) (lastGrid nextState)

testUndoStep :: Test
testUndoStep = TestCase $ do
    let grid = initialGrid
        newGrid = crushableGrid
        initialState = GameState grid easy (Just newGrid) 50 0
    updatedState <- execStateT (updateGridState newGrid) initialState
    prevState <- execStateT undoStep updatedState
    assertEqual "Grid reverted" grid (currentGrid prevState)
    assertEqual "Remaining steps restored" 50 (remainingSteps prevState)
    assertEqual "Last grid cleared" Nothing (lastGrid prevState)

testGetRemainingSteps :: Test
testGetRemainingSteps = TestCase $ do
    let grid = initialGrid
        initialState = GameState grid easy Nothing 50 0
    remainingSteps <- evalStateT getRemainingSteps initialState
    assertEqual "Get remaining steps" 50 remainingSteps

testAddScore :: Test
testAddScore = TestCase $ do
    let grid = initialGrid
        initialState = GameState grid easy Nothing 50 0
    nextState <- execStateT (addScore 100) initialState
    assertEqual "Score updated" 100 (score nextState)

testResetScoreChange :: Test
testResetScoreChange = TestCase $ do
    let initialState = GameState crushableGrid easy Nothing 50 100
    updatedState <- execStateT resetScoreChange initialState
    let updatedScoreChange = scoreChange (currentGrid updatedState)
    assertEqual "Score change should be reset to 0" 0 updatedScoreChange

testUndoable :: Test
testUndoable = TestCase $ do
    let grid = initialGrid
        initialState = GameState grid easy Nothing 50 0
    assertEqual "InitialState not undoable" False =<< 
        evalStateT undoable initialState
    nextState <- execStateT (updateGridState grid) initialState
    isUndoable <- evalStateT undoable nextState
    assertEqual "UpdatedState undoable" True isUndoable

testAddScoreAccumulation :: Test
testAddScoreAccumulation = TestCase $ do
    let initialState = GameState initialGrid easy Nothing 50 0
    finalState <- execStateT (addScore 50 >> addScore 150) initialState
    assertEqual "Score after accumulation" 200 (score finalState)

-- QuickCheck properties
-- Property: Grid dimensions match difficulty
prop_gridDimensionsMatch :: GameConst -> Property
prop_gridDimensionsMatch d = monadicIO $ do
    grid <- run $ initializeGrid d
    let dim = dimension d
    Test.QuickCheck.Monadic.assert $ length (board grid) == dim

-- Property: All candies in a randomly generated game grid are valid
prop_allCandiesValid :: GameConst -> Property
prop_allCandiesValid d = monadicIO $ do
    grid <- run $ initializeGrid d
    let validShapes = map (shapeName . candyDef) (normalCandies grid)
        allCandies = concat (board grid)
        allShapesValid = 
            all (\c -> shapeName (candyDef c) `elem` validShapes) allCandies
        allEffectsNormal = 
            all (\c -> effectNameRef (candyDef c) == "Normal") allCandies
    Test.QuickCheck.Monadic.assert $ allShapesValid && allEffectsNormal

-- Property: splitIntoRows should reconstruct the original list
prop_splitIntoRows :: [Int] -> Property
prop_splitIntoRows xs = not (null xs) ==> monadicIO $ do
    n <- run $ generate (choose (1, length xs))
    Test.QuickCheck.Monadic.assert $ concat (splitIntoRows n xs) == xs

-- Property: update grid and then undo should revert to original state
prop_updateUndo :: GameConst -> Property
prop_updateUndo d = monadicIO $ do
    gameState <- run $ generate (genGameState d)
    updatedState <- run $ 
        execStateT (updateGridState (currentGrid gameState)) gameState
    prevState <- run $ execStateT undoStep updatedState
    Test.QuickCheck.Monadic.assert $ 
        currentGrid prevState == currentGrid gameState

-- Property: undo step can only consecutively undo once
prop_undoOnce :: GameConst -> Property
prop_undoOnce d = monadicIO $ do
    gameState <- run $ generate (genGameState d)
    updatedState <- run $ 
        execStateT (updateGridState (currentGrid gameState)) gameState
    prevState <- run $ execStateT undoStep updatedState
    prevState2 <- run $ execStateT undoStep prevState
    Test.QuickCheck.Monadic.assert $ prevState == prevState2

-- Update the runUnitTests function
runUnitTests :: IO Counts
runUnitTests = runTestTT $
    TestList [
        TestLabel "testInitializeGrid" testInitializeGrid,
        TestLabel "testSplitIntoRows" testSplitIntoRows,
        TestLabel "testGetCurrentGrid" testGetCurrentGrid,
        TestLabel "testUpdateGridState" testUpdateGridState,
        TestLabel "testUndoStep" testUndoStep,
        TestLabel "testGetRemainingSteps" testGetRemainingSteps,
        TestLabel "testAddScore" testAddScore,
        TestLabel "testResetScoreChange" testResetScoreChange,
        TestLabel "testUndoable" testUndoable,
        TestLabel "testAddScoreAccumulation" testAddScoreAccumulation
    ]

-- Update the runQuickCheckTests function
runQuickCheckTests :: IO ()
runQuickCheckTests = do
    putStrLn "prop_gridDimensionsMatch:"
    quickCheck (prop_gridDimensionsMatch easy)
    quickCheck (prop_gridDimensionsMatch medium)
    quickCheck (prop_gridDimensionsMatch hard)
    putStrLn "prop_allCandiesValid:"
    quickCheck (prop_allCandiesValid easy)
    quickCheck (prop_allCandiesValid medium)
    quickCheck (prop_allCandiesValid hard)
    putStrLn "prop_splitIntoRows:"
    quickCheck prop_splitIntoRows
    putStrLn "prop_updateUndo:"
    quickCheck prop_updateUndo
    putStrLn "prop_undoOnce:"
    quickCheck prop_undoOnce

-- main :: IO ()
-- main = do
--     runUnitTests >>= print
--     runQuickCheckTests