module GameControllerTest where
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.HUnit
import TestUtils

import GameController
import Phd
import qualified Data.Set as Set
import Data.List (sort)
import Data.Sequence (Seq(Empty))

-- Test contructDisappear: coordinates should be sorted
testConstructDisappear :: Test
testConstructDisappear = TestCase $ do
    let disappear = constructDisappear
            [(Coordinate 1, Coordinate 3),
             (Coordinate 1, Coordinate 0),
             (Coordinate 1, Coordinate 2)]
    let expectedDisappear = Disappear
            [(Coordinate 1, Coordinate 0),
             (Coordinate 1, Coordinate 2),
             (Coordinate 1, Coordinate 3)]
    assertEqual "Disappear should be equal" expectedDisappear disappear

testConstructors :: Test
testConstructors = TestList [
    TestLabel "constructDisappear" testConstructDisappear
  ]

-- Test helper functions for swap action
testSwapCandies :: Test
testSwapCandies = TestCase $ do
    let grid = swapCandies initialGrid
            (Coordinate 0, Coordinate 0) (Coordinate 1, Coordinate 0)
    let expectedBoard =
            [ [candy4, candy2, candy3]
            , [candy2, candy1, candy2]
            , [candy3, candy5, candy4]
            ]
    assertEqual "Grids should be equal" expectedBoard (board grid)

testFindNormalCandyCrushables1 :: Test
testFindNormalCandyCrushables1 = TestCase $ do
    let crushables = findNormalCandyCrushables crushableGrid (Coordinate 0, Coordinate 0)
    let expectedCrushables = Just (Disappear
            [(Coordinate 0, Coordinate 0),
             (Coordinate 0, Coordinate 1),
             (Coordinate 0, Coordinate 2)])
    assertEqual "Crushables should be equal" expectedCrushables crushables

testFindNormalCandyCrushables2 :: Test
testFindNormalCandyCrushables2 = TestCase $ do
    let crushables = findNormalCandyCrushables crushableGrid (Coordinate 1, Coordinate 1)
    let expectedCrushables = Just (Disappear
            [(Coordinate 1, Coordinate 0),
             (Coordinate 1, Coordinate 1),
             (Coordinate 1, Coordinate 2),
             (Coordinate 1, Coordinate 3)])
    assertEqual "Crushables should be equal" expectedCrushables crushables

testFindNormalCandyCrushables3 :: Test
testFindNormalCandyCrushables3 = TestCase $ do
    let crushables = findNormalCandyCrushables initialGrid (Coordinate 1, Coordinate 1)
    let expectedCrushables = Nothing
    assertEqual "Crushables should be equal" expectedCrushables crushables

testFindCrushables1 :: Test
testFindCrushables1 = TestCase $ do
    let crushables = findCrushables crushableGrid
            [(Coordinate 0, Coordinate 0),
             (Coordinate 1, Coordinate 1),
             (Coordinate 2, Coordinate 2)]
    let expectedCrushables =
            [Disappear [(Coordinate 0, Coordinate 0),
                        (Coordinate 0, Coordinate 1),
                        (Coordinate 0, Coordinate 2)],
             Disappear [(Coordinate 1, Coordinate 0),
                        (Coordinate 1, Coordinate 1),
                        (Coordinate 1, Coordinate 2),
                        (Coordinate 1, Coordinate 3)],
             Trigger ((Coordinate 2, Coordinate 2), candy5)]
    assertEqual "Crushables should be equal" expectedCrushables crushables

testFindCrushables2 :: Test
testFindCrushables2 = TestCase $ do
    let crushables = findCrushables initialGrid 
            [(Coordinate 0, Coordinate 0),
             (Coordinate 0, Coordinate 1),
             (Coordinate 1, Coordinate 2)]
    let expectedCrushables = []
    assertEqual "Crushables should be equal" expectedCrushables crushables

testSwapHelpers :: Test
testSwapHelpers = TestList [
    TestLabel "swapCandies" testSwapCandies,
    TestLabel "findNormalCandyCrushables1" testFindNormalCandyCrushables1,
    TestLabel "findNormalCandyCrushables2" testFindNormalCandyCrushables2,
    TestLabel "findNormalCandyCrushables3" testFindNormalCandyCrushables3,
    TestLabel "findCrushables1" testFindCrushables1,
    TestLabel "findCrushables2" testFindCrushables2
  ]

-- Test helper functions for apply action
testApplyDisappear :: Test
testApplyDisappear = TestCase $ do
    grid <- applyDisappear crushableGrid
        [(Coordinate 0, Coordinate 0),
        (Coordinate 0, Coordinate 1),
        (Coordinate 0, Coordinate 2)]
    let expectedBoard =
            [ [EmptyCandy, EmptyCandy, EmptyCandy, candy3]
            , [candy2, candy2, candy2, candy2]
            , [candy2, candy6, candy5, candy3]
            , [candy3, candy2, candy1, candy1]
            ]
    assertEqual "Grids should be equal" expectedBoard (board grid)

testApplyTrigger :: Test
testApplyTrigger = TestCase $ do
    grid <- applyTrigger crushableGrid (Coordinate 2, Coordinate 2) candy5
    let expectedBoard =
            [ [candy1, EmptyCandy, candy1, candy3]
            , [candy2, EmptyCandy, EmptyCandy, EmptyCandy]
            , [EmptyCandy, EmptyCandy, EmptyCandy, EmptyCandy]
            , [candy3, EmptyCandy, EmptyCandy, EmptyCandy]
            ]
    assertEqual "Grids should be equal" expectedBoard (board grid)

testSwapNoCrush :: Test
testSwapNoCrush = TestCase $ do
    grid <- applySwap initialGrid
        (Coordinate 0, Coordinate 0) (Coordinate 0, Coordinate 1)
    assertEqual "Grids should be equal" initialGrid grid

testSwapCrush :: Test
testSwapCrush = TestCase $ do
    grid <- applySwap initialGrid
        (Coordinate 0, Coordinate 2) (Coordinate 1, Coordinate 2)
    let expectedBoard =
            [ [EmptyCandy, EmptyCandy, EmptyCandy]
            , [candy4, candy1, candy3]
            , [candy3, candy5, candy4]
            ] -- After crushing (0,0), (0,1), (0,2)
    assertEqual "Grids should be equal" expectedBoard (board grid)

testApplyClick1 :: Test
testApplyClick1 = TestCase $ do
    grid <- applyClick crushableGrid (Coordinate 2, Coordinate 1)
    let expectedBoard =
            [ [candy1, EmptyCandy, candy1, candy3]
            , [candy2, EmptyCandy, EmptyCandy, EmptyCandy]
            , [EmptyCandy, EmptyCandy, EmptyCandy, EmptyCandy]
            , [candy3, EmptyCandy, EmptyCandy, EmptyCandy]
            ]
    assertEqual "Grids should be equal" expectedBoard (board grid)

testApplyClick2 :: Test
testApplyClick2 = TestCase $ do
    grid <- applyClick crushableGrid (Coordinate 0, Coordinate 0)
    assertEqual "Grids should be equal" (board crushableGrid) (board grid)

testActions :: Test
testActions = TestList [
    TestLabel "applyDisappear" testApplyDisappear,
    TestLabel "applyTrigger" testApplyTrigger,
    TestLabel "applySwapNoCrush" testSwapNoCrush,
    TestLabel "applySwapCrush" testSwapCrush,
    TestLabel "applyClick1" testApplyClick1,
    TestLabel "applyClick2" testApplyClick2
  ]

-- Test applyAction input
testApplyAction1 :: Test
testApplyAction1 = TestCase $ do
    grid <- applyAction initialGrid 
        (Swap (Coordinate 0, Coordinate 0) (Coordinate 0, Coordinate 1))
    assertEqual "Grids should be equal" initialGrid grid

testApplyAction2 :: Test
testApplyAction2 = TestCase $ do
    grid <- applyAction initialGrid (Swap 
        (Coordinate 0, Coordinate 2) (Coordinate 1, Coordinate 2))
    let expectedBoard =
            [ [EmptyCandy, EmptyCandy, EmptyCandy]
            , [candy4, candy1, candy3]
            , [candy3, candy5, candy4]
            ]  -- After crushing (0,0), (0,1), (0,2)
    assertEqual "Grids should be equal" expectedBoard (board grid)

testApplyAction3 :: Test
testApplyAction3 = TestCase $ do
    grid <- applyAction crushableGrid (Click (Coordinate 2, Coordinate 2))
    let expectedBoard =
            [ [candy1, EmptyCandy, candy1, candy3]
            , [candy2, EmptyCandy, EmptyCandy, EmptyCandy]
            , [EmptyCandy, EmptyCandy, EmptyCandy, EmptyCandy]
            , [candy3, EmptyCandy, EmptyCandy, EmptyCandy]
            ]
    assertEqual "Grids should be equal" expectedBoard (board grid)

testApplyAction4 :: Test
testApplyAction4 = TestCase $ do
    grid <- applyAction crushableGrid (Click (Coordinate 2, Coordinate 0))
    assertEqual "Grids should be equal" (board crushableGrid) (board grid)

testApplyAction5 :: Test
testApplyAction5 = TestCase $ do
    grid <- applyAction crushableGrid 
        (Trigger ((Coordinate 2, Coordinate 1), candy6))
    let expectedBoard =
            [ [candy1, EmptyCandy, candy1, candy3]
            , [candy2, EmptyCandy, EmptyCandy, EmptyCandy]
            , [EmptyCandy, EmptyCandy, EmptyCandy, EmptyCandy]
            , [candy3, EmptyCandy, EmptyCandy, EmptyCandy]
            ]
    assertEqual "Grids should be equal" expectedBoard (board grid)

testApplyAction6 :: Test
testApplyAction6 = TestCase $ do
    grid <- applyAction crushableGrid (Disappear 
            [(Coordinate 0, Coordinate 0),
            (Coordinate 0, Coordinate 1),
            (Coordinate 0, Coordinate 2)])
    let expectedBoard =
            [ [EmptyCandy, EmptyCandy, EmptyCandy, candy3]
            , [candy2, candy2, candy2, candy2]
            , [candy2, candy6, candy5, candy3]
            , [candy3, candy2, candy1, candy1]
            ]
    assertEqual "Grids should be equal" expectedBoard (board grid)

testApplyAction :: Test
testApplyAction = TestList [
    TestLabel "applyAction1" testApplyAction1,
    TestLabel "applyAction2" testApplyAction2,
    TestLabel "applyAction3" testApplyAction3,
    TestLabel "applyAction4" testApplyAction4,
    TestLabel "applyAction5" testApplyAction5,
    TestLabel "applyAction6" testApplyAction6
  ]

-- Test autoCrush related functions
expectedCrushables :: [Action]
expectedCrushables
    = [Disappear [(Coordinate 0, Coordinate 0),
                (Coordinate 0, Coordinate 1),
                (Coordinate 0, Coordinate 2)],
       Disappear [(Coordinate 1, Coordinate 0),
                (Coordinate 1, Coordinate 1),
                (Coordinate 1, Coordinate 2),
                (Coordinate 1, Coordinate 3)]]

testFinalAllCrushables :: Test
testFinalAllCrushables = TestCase $ do
    let allCrushables = sort $ findAllNormalCrushables crushableGrid
    assertEqual "Crushables should be equal" (sort expectedCrushables) allCrushables

testApplyActions :: Test
testApplyActions = TestCase $ do
    grid <- applyActions crushableGrid expectedCrushables
    let expectedBoard =
            [ [EmptyCandy, EmptyCandy, EmptyCandy, candy3]
            , [EmptyCandy, EmptyCandy, candy7, EmptyCandy]
            , [candy2, candy6, candy5, candy3]
            , [candy3, candy2, candy1, candy1]
            ]
    assertEqual "Grids should be equal" expectedBoard (board grid)

testAutoCrush :: Test
testAutoCrush = TestCase $ do
    crushedGrid <- autoCrush crushableGrid
    let expectedBoard =
            [ [EmptyCandy, EmptyCandy, EmptyCandy, candy3]
            , [EmptyCandy, EmptyCandy, candy7, EmptyCandy]
            , [candy2, candy6, candy5, candy3]
            , [candy3, candy2, candy1, candy1]
            ]
    assertEqual "Grids should be equal" expectedBoard (board crushedGrid)

testAutoCrushRelated :: Test
testAutoCrushRelated = TestList [
    TestLabel "finalAllCrushables" testFinalAllCrushables,
    TestLabel "applyActions" testApplyActions,
    TestLabel "autoCrush" testAutoCrush
  ]

-- QuickCheck properties
-- Property: Swapping candies twice results in the original grid
prop_swapCandiesIdempotent :: Difficulty -> Property
prop_swapCandiesIdempotent d = monadicIO $ do
    coord1 <- run $ generate $ genArbIntCoordPair d
    coord2 <- run $ generate $ genArbIntCoordPair d
    grid <- run $ generate $ genGameGrid d
    let swappedOnce = swapCandies grid coord1 coord2
    let swappedTwice = swapCandies swappedOnce coord1 coord2
    return $ swappedTwice == grid

-- Property: Applying an action preserves the grid size
prop_applyActionPreservesGridSize :: Difficulty -> Property
prop_applyActionPreservesGridSize d = monadicIO $ do
    grid <- run $ generate $ genGameGrid d
    action <- run $ generate $ genArbAction d
    newGrid <- run $ applyAction grid action
    return $ length (board grid) == length (board newGrid) &&
             all (\(row1, row2) -> length row1 == length row2)
                (zip (board grid) (board newGrid))

-- -- Property: all candies in a crushable group are the same
prop_findNormalCandyCrushablesMatch :: Difficulty -> Property
prop_findNormalCandyCrushablesMatch d = monadicIO $ do
    coord <- run $ generate $ genArbIntCoordPair d
    grid <- run $ generate $ genGameGrid d
    let crushables = findNormalCandyCrushables grid coord
    return $ all (allSameCandy grid) crushables
    where
        allSameCandy :: GameGrid -> Action -> Bool
        allSameCandy grid (Disappear coords) =
            let candies = map (\(Coordinate x, Coordinate y) ->
                    (board grid !! x) !! y) coords
            in all (== head candies) candies
        allSameCandy _ _ = False

-- Property: fillAndCrushUntilStable should return a stable grid with no immediate crushables
prop_fillAndCrushUntilStable :: Difficulty -> Property
prop_fillAndCrushUntilStable d = monadicIO $ do
    grid <- run $ generate $ genGameGrid d
    stableGrid <- run $ fillAndCrushUntilStable grid (normalCandies grid)
    -- there should be no immediate crushables in the stable grid
    crushAgain <- run $ autoCrush stableGrid
    Test.QuickCheck.Monadic.assert $ board crushAgain == board stableGrid

-- Property: Applying an action modifies step size correctly
prop_actionStepSize :: Difficulty -> Property
prop_actionStepSize d = monadicIO $ do
    initalState <- run $ generate $ genGameState d
    initalAction <- run $ generate $ genArbUserAction d
    let initalStepSize = remainingSteps initalState
    newState <- run $ handleAction False initalState initalAction
    let newStepSize = remainingSteps newState
    case initalAction of
        Undo ->
            case lastGrid initalState of
                Just _ ->
                    Test.QuickCheck.Monadic.assert $
                        newStepSize == initalStepSize + 1
                -- already undoed once or initial state: no last grid to restore
                Nothing ->
                    Test.QuickCheck.Monadic.assert $
                        newStepSize == initalStepSize
        Quit ->
            Test.QuickCheck.Monadic.assert $
                newStepSize == initalStepSize
        _ ->
            Test.QuickCheck.Monadic.assert $
                newStepSize == initalStepSize - 1

-- Property: undoing an action restores the grid to the previous state
prop_undoRestoresGrid :: Difficulty -> Property
prop_undoRestoresGrid d = monadicIO $ do
    initialState <- run $ generate $ genGameState d
    action <- run $ generate $ genArbReversibleAction d
    newState <- run $ handleAction False initialState action
    undoState <- run $ handleAction False newState Undo
    return $ currentGrid initialState == currentGrid undoState

-- Unit tests
runUnitTests :: IO Counts
runUnitTests = runTestTT $ TestList
    [
        TestLabel "testConstructors" testConstructors,
        TestLabel "testSwapHelpers" testSwapHelpers,
        TestLabel "testActions" testActions,
        TestLabel "testApplyAction" testApplyAction,
        TestLabel "testAutoCrushRelated" testAutoCrushRelated
    ]

-- QuickCheck tests
runQuickCheckTests :: IO ()
runQuickCheckTests = do
    putStrLn "prop_swapCandiesIdempotent:"
    quickCheck prop_swapCandiesIdempotent
    putStrLn "prop_applyActionPreservesGridSize:"
    quickCheck prop_applyActionPreservesGridSize
    putStrLn "prop_findNormalCandyCrushablesMatch:"
    quickCheck prop_findNormalCandyCrushablesMatch
    putStrLn "prop_fillAndCrushUntilStable:"
    quickCheck prop_fillAndCrushUntilStable
    putStrLn "prop_actionStepSize:"
    quickCheck prop_actionStepSize
    putStrLn "prop_undoRestoresGrid:"
    quickCheck prop_undoRestoresGrid

-- main :: IO ()
-- main = do
--     runUnitTests >>= print
--     runQuickCheckTests
