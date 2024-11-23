module GameGridTest where

import Test.HUnit
import Test.QuickCheck
import GameGrid
import Candy
import Test.QuickCheck.Monadic
import Control.Monad (liftM2)
import Control.Monad.State

-- Sample candies for testing
candy1 :: Candy
candy1 = Candy Circle Normal
candy2 :: Candy
candy2 = Candy Square Normal
candy3 :: Candy
candy3 = Candy Triangle Normal
candy4 :: Candy
candy4 = Candy Heart Normal
candy5 :: Candy
candy5 = Candy Star Normal
candySpecial :: Candy
candySpecial = bombCandy

sampleBoard :: [[Candy]]
sampleBoard = [[candy1, candy2, candy3], [candy4, candy5, candySpecial]]

testInitializeGrid :: Test
testInitializeGrid = TestCase $ do
    grid <- initializeGrid easy
    let dim = dimension easy
    assertEqual "Grid dimensions match" dim (length (board grid))
    assertEqual "Empty candy coordinates initially empty" [] (emptyCandyCoords grid)

testSplitIntoRows :: Test
testSplitIntoRows = TestCase $ do
    let input = [1..9]
        result = splitIntoRows 3 input
    assertEqual "Split list into rows of 3" [[1,2,3],[4,5,6],[7,8,9]] result

testGetCurrentGrid :: Test
testGetCurrentGrid = TestCase $ do
    let grid = GameGrid { board = sampleBoard, emptyCandyCoords = [] }
        initialState = GameState grid easy Nothing 50 0
    currentGrid <- evalStateT getCurrentGrid initialState
    assertEqual "Get current grid" grid currentGrid

testUpdateGridState :: Test
testUpdateGridState = TestCase $ do
    let grid = GameGrid { board = [], emptyCandyCoords = [] }
        initialState = GameState grid easy Nothing 50 0
        newGrid = GameGrid { board = sampleBoard, emptyCandyCoords = [(2,2)] }
    nextState <- execStateT (updateGridState newGrid) initialState
    assertEqual "Grid updated" newGrid (currentGrid nextState)
    assertEqual "Remaining steps decremented" 49 (remainingSteps nextState)
    assertEqual "Last grid saved" (Just grid) (lastGrid nextState)

testUndoStep :: Test
testUndoStep = TestCase $ do
    let grid = GameGrid { board = [], emptyCandyCoords = [] }
        newGrid = GameGrid { board = sampleBoard, emptyCandyCoords = [(1,1)] }
        initialState = GameState grid easy (Just newGrid) 50 0
    updatedState <- execStateT (updateGridState newGrid) initialState
    prevState <- execStateT undoStep updatedState
    assertEqual "Grid reverted" grid (currentGrid prevState)
    assertEqual "Remaining steps restored" 50 (remainingSteps prevState)
    assertEqual "Last grid cleared" Nothing (lastGrid prevState)

testGetRemainingSteps :: Test
testGetRemainingSteps = TestCase $ do
    let grid = GameGrid { board = sampleBoard, emptyCandyCoords = [] }
        initialState = GameState grid easy Nothing 50 0
    remainingSteps <- evalStateT getRemainingSteps initialState
    assertEqual "Get remaining steps" 50 remainingSteps

testGetEmptyCandyCoords :: Test
testGetEmptyCandyCoords = TestCase $ do
    let grid = GameGrid { board = sampleBoard, emptyCandyCoords = [(1,1), (2,2)] }
    assertEqual "Get empty candy coordinates" [(1,1), (2,2)] (getEmptyCandyCoords grid)

testAddScore :: Test
testAddScore = TestCase $ do
    let grid = GameGrid { board = sampleBoard, emptyCandyCoords = [] }
        initialState = GameState grid easy Nothing 50 0
    nextState <- execStateT (addScore 100) initialState
    assertEqual "Score updated" 100 (score nextState)

testUndoable :: Test
testUndoable = TestCase $ do
    let grid = GameGrid { board = sampleBoard, emptyCandyCoords = [] }
        initialState = GameState grid easy Nothing 50 0
    assertEqual "InitialState not undoable" False =<< evalStateT undoable initialState
    nextState <- execStateT (updateGridState grid) initialState
    isUndoable <- evalStateT undoable nextState
    assertEqual "UpdatedState undoable" True isUndoable

testCloneGrid :: Test
testCloneGrid = TestCase $ do
    let grid = GameGrid { board = sampleBoard, emptyCandyCoords = [] }
        clonedGrid = cloneGrid grid
    assertEqual "Grid cloned" grid clonedGrid

testGetCandyAt :: Test
testGetCandyAt = TestCase $ do
    assertEqual "Get valid candy" (Just candy2) (getCandyAt sampleBoard (0,1))
    assertEqual "Get invalid candy" Nothing (getCandyAt sampleBoard (3,3))

testSetCandyAt :: Test
testSetCandyAt = TestCase $ do
    let updatedBoard = setCandyAt sampleBoard (0,1) stripedCrossCandy
    case getCandyAt updatedBoard (0,1) of
        Just candy -> assertEqual "Set valid candy" stripedCrossCandy candy
        Nothing -> assertFailure "Candy not found at (0,1)"
    case getCandyAt sampleBoard (0,1) of
        Just candy -> assertEqual "Original board unchanged" candy2 candy
        Nothing -> assertFailure "Candy not found at (0,0)"

-- Arbitrary instance for Difficulty
instance Arbitrary Difficulty where
    arbitrary :: Gen Difficulty
    arbitrary = elements [easy, medium, hard]

-- Arbitrary instance for GameState
instance Arbitrary GameState where
    arbitrary :: Gen GameState
    arbitrary = do
        difficulty <- arbitrary
        grid <- arbitraryGameGrid difficulty
        lastGrid <- oneof [Just <$> arbitraryGameGrid difficulty, pure Nothing]
        remainingSteps <- arbitrary
        score <- arbitrary
        return GameState { currentGrid = grid, difficulty = difficulty, lastGrid = lastGrid, remainingSteps = remainingSteps, score = score }

-- Arbitrary instance for GameGrid
instance Arbitrary GameGrid where
    arbitrary :: Gen GameGrid
    arbitrary = do
        difficulty <- arbitrary
        arbitraryGameGrid difficulty

-- Generate an arbitrary GameGrid with a given difficulty
arbitraryGameGrid :: Difficulty -> Gen GameGrid
arbitraryGameGrid difficulty = do
        let dim = dimension difficulty
        candies <- vectorOf (dim * dim) (arbitraryCandy difficulty)
        return GameGrid {
            board = splitIntoRows dim candies,
            emptyCandyCoords = []  -- Assume no empty candies for simplicity
        }

-- Arbitrary instance for Candy
instance Arbitrary Candy where
    arbitrary :: Gen Candy
    arbitrary = do
        difficulty <- arbitrary
        arbitraryCandy difficulty

arbitraryCandy :: Difficulty -> Gen Candy
arbitraryCandy difficulty = do
    shape <- elements (candyShapes difficulty)
    let effect = Normal
    return Candy { candyShape = shape, candyEffect = effect }

-- Property: Grid dimensions match difficulty
prop_gridDimensionsMatch :: Difficulty -> Property
prop_gridDimensionsMatch diff = 
    let grid = arbitraryGameGrid diff
    in forAll grid $ \g -> 
        length (board g) == dimension diff && 
        all (\row -> length row == dimension diff) (board g)

-- Property: All candies in a randomly generated game grid are valid
prop_allCandiesValid :: Difficulty -> Property
prop_allCandiesValid diff =
    let grid = arbitraryGameGrid diff
    in forAll grid $ \g -> all (all (isValidCandy diff)) (board g)
    where
        isValidCandy :: Difficulty -> Candy -> Bool
        isValidCandy diff EmptyCandy = True
        isValidCandy diff (Candy shape effect) =
            let validShapes = candyShapes diff
            in shape `elem` validShapes && effect == Normal

-- Property: splitIntoRows should reconstruct the original list
prop_splitIntoRows :: Int -> [Int] -> Property
prop_splitIntoRows n xs =
    n > 0 && n <= length xs ==> concat (splitIntoRows n xs) == xs

-- Property: update grid and then undo should revert to original state
prop_updateUndo :: GameState -> Property
prop_updateUndo initialState =
    let grid = currentGrid initialState
    in monadicIO $ do
        updatedState <- run $ execStateT (updateGridState grid) initialState
        nextState <- run $ execStateT undoStep updatedState
        Test.QuickCheck.Monadic.assert $ currentGrid nextState == grid

-- Property: undo step can only consecutively undo once
prop_undoOnce :: GameState -> Property
prop_undoOnce initialState =
    let grid = currentGrid initialState
    in monadicIO $ do
        updatedState <- run $ execStateT (updateGridState grid) initialState
        nextState <- run $ execStateT undoStep updatedState
        nextState2 <- run $ execStateT undoStep nextState
        Test.QuickCheck.Monadic.assert $ nextState2 == nextState

runUnitTests :: IO Counts
runUnitTests = runTestTT $
    TestList [
        TestLabel "testInitializeGrid" testInitializeGrid,
        TestLabel "testSplitIntoRows" testSplitIntoRows,
        TestLabel "testGetCurrentGrid" testGetCurrentGrid,
        TestLabel "testUpdateGridState" testUpdateGridState,
        TestLabel "testUndoStep" testUndoStep,
        TestLabel "testGetRemainingSteps" testGetRemainingSteps,
        TestLabel "testGetEmptyCandyCoords" testGetEmptyCandyCoords,
        TestLabel "testAddScore" testAddScore,
        TestLabel "testUndoable" testUndoable,
        TestLabel "testCloneGrid" testCloneGrid,
        TestLabel "testGetCandyAt" testGetCandyAt,
        TestLabel "testSetCandyAt" testSetCandyAt
    ]

runQuickCheckTests :: IO ()
runQuickCheckTests = do
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