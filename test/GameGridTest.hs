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
sampleGrid :: GameGrid
sampleGrid = GameGrid{ 
    board = [[candy1, candy2, candy3], [candy4, candy5, candySpecial], [candy3, candy2, candy1]]
    , emptyCandyCoords = []
    }

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
testSetCandyAtBoundary :: Test
testSetCandyAtBoundary = TestCase $ do
    let updatedBoard = setCandyAt sampleBoard (-1, 0) stripedCrossCandy
    assertEqual "Set candy at negative index should not change board" sampleBoard updatedBoard
    let updatedBoard2 = setCandyAt sampleBoard (0, 100) stripedCrossCandy
    assertEqual "Set candy at out-of-bounds index should not change board" sampleBoard updatedBoard2

testGetCandyAtBoundary :: Test
testGetCandyAtBoundary = TestCase $ do
    let result = getCandyAt sampleBoard (-1, 0)
    assertEqual "Get candy at negative index should return Nothing" Nothing result
    let result2 = getCandyAt sampleBoard (0, 100)
    assertEqual "Get candy at out-of-bounds index should return Nothing" Nothing result2

testAddScoreAccumulation :: Test
testAddScoreAccumulation = TestCase $ do
    let initialState = GameState sampleGrid easy Nothing 50 0
    finalState <- execStateT (addScore 50 >> addScore 150) initialState
    assertEqual "Score after accumulation" 200 (score finalState)

testCreateSpecialCandies :: Test
testCreateSpecialCandies = TestCase $ do
    let bomb = bombCandy
    let stripedRow = stripedRowCandy 
    let stripedCross = stripedCrossCandy
    assertEqual "Bomb candy should have effect Bomb" Bomb (candyEffect bomb)
    assertEqual "Striped row candy should have effect StripedRow" StripedRow (candyEffect stripedRow)
    assertEqual "Striped cross candy should have effect StripedCross" StripedCross (candyEffect stripedCross)
testSetGetSpecialCandy :: Test
testSetGetSpecialCandy = TestCase $ do
    let grid = GameGrid { board = sampleBoard, emptyCandyCoords = [] }
        specialCandy = bombCandy
        updatedBoard = setCandyAt (board grid) (1,1) specialCandy
        retrievedCandy = getCandyAt updatedBoard (1,1)
    case retrievedCandy of
        Just candy -> assertEqual "Set and get special candy" specialCandy candy
        Nothing -> assertFailure "Special candy not found at (1,1)"

-- test corner cases
testEmptyGridOperations :: Test
testEmptyGridOperations = TestCase $ do
    let emptyGrid = GameGrid { board = [], emptyCandyCoords = [] }
        result = getCandyAt (board emptyGrid) (0,0)
    assertEqual "Get candy from empty grid should return Nothing" Nothing result
    let updatedBoard = setCandyAt (board emptyGrid) (0,0) candy1
    assertEqual "Set candy on empty grid should not change the grid" [] updatedBoard
testInvalidCoordinate :: Test
testInvalidCoordinate = TestCase $ do
    let grid = GameGrid { board = sampleBoard, emptyCandyCoords = [] }
        invalidCoordinates = [(-1,-1), (100,100), (0,-1), (-1,0)]
    mapM_ (\coord -> do
        let result = getCandyAt (board grid) coord
        assertEqual ("Get candy at invalid coordinate " ++ show coord) Nothing result
        let updatedBoard = setCandyAt (board grid) coord candy1
        assertEqual ("Set candy at invalid coordinate " ++ show coord) (board grid) updatedBoard
        ) invalidCoordinates


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

-- CandyShape Arbitrary instance
instance Arbitrary CandyShape where
    arbitrary = elements [Triangle, Circle, Square, Star, Heart, Diamond, Asterisk]

-- CandyEffect Arbitrary instance
instance Arbitrary CandyEffect where
    arbitrary = elements [Normal, Bomb, StripedRow, StripedCross]

-- Arbitrary instance for Candy
instance Arbitrary Candy where
    arbitrary :: Gen Candy
    arbitrary = do
        difficulty <- arbitrary
        arbitraryCandy difficulty

-- Generate an arbitrary GameGrid with a given difficulty
arbitraryGameGrid :: Difficulty -> Gen GameGrid
arbitraryGameGrid difficulty = do
        let dim = dimension difficulty
        candies <- vectorOf (dim * dim) (arbitraryCandy difficulty)
        return GameGrid {
            board = splitIntoRows dim candies,
            emptyCandyCoords = []  -- Assume no empty candies for simplicity
        }

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

arbitraryCandyWithDifficulty :: Difficulty -> Gen Candy
arbitraryCandyWithDifficulty difficulty = do
    shape <- elements (candyShapes difficulty)
    let effect = Normal
    return Candy { candyShape = shape, candyEffect = effect }

-- Property: Cloning a grid results in an equal but not identical grid
prop_cloneGrid :: GameGrid -> Bool
prop_cloneGrid grid =
    let clonedGrid = cloneGrid grid
    in grid == clonedGrid && board grid == board clonedGrid

-- Property: Setting and then getting a candy at a valid coordinate retrieves the same candy
prop_setGetCandyAt :: GameState -> Property
prop_setGetCandyAt gameState =
    let grid = currentGrid gameState
        difficultyLevel = difficulty gameState
        dimX = length (board grid)
        dimY = length (head (board grid))
    in not (null (board grid)) ==>
        forAll (choose (0, dimX - 1)) $ \x ->
        forAll (choose (0, dimY - 1)) $ \y ->
        forAll (arbitraryCandyWithDifficulty difficultyLevel) $ \candy ->
            let newBoard = setCandyAt (board grid) (x, y) candy
                retrievedCandy = getCandyAt newBoard (x, y)
            in retrievedCandy == Just candy

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
        TestLabel "testGetEmptyCandyCoords" testGetEmptyCandyCoords,
        TestLabel "testAddScore" testAddScore,
        TestLabel "testUndoable" testUndoable,
        TestLabel "testCloneGrid" testCloneGrid,
        TestLabel "testGetCandyAt" testGetCandyAt,
        TestLabel "testSetCandyAt" testSetCandyAt,
        TestLabel "testSetCandyAtBoundary" testSetCandyAtBoundary,
        TestLabel "testGetCandyAtBoundary" testGetCandyAtBoundary,
        TestLabel "testAddScoreAccumulation" testAddScoreAccumulation,
        TestLabel "testCreateSpecialCandies" testCreateSpecialCandies,
        TestLabel "testSetGetSpecialCandy" testSetGetSpecialCandy,
        TestLabel "testEmptyGridOperations" testEmptyGridOperations,
        TestLabel "testInvalidCoordinate" testInvalidCoordinate
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
    putStrLn "prop_cloneGrid:"
    quickCheck prop_cloneGrid
    putStrLn "prop_setGetCandyAt:"
    quickCheck prop_setGetCandyAt

main :: IO ()
main = do
    runUnitTests >>= print
    runQuickCheckTests