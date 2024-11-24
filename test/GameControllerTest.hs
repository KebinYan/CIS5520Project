module GameControllerTest where
import Test.QuickCheck 
import Test.QuickCheck.Monadic
import Test.HUnit

import GameController
import GameGrid
import Candy
import qualified Data.Set as Set

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

-- Test grids
initialGrid :: GameGrid
initialGrid = GameGrid
    [ [candy2, candy2, candy3]
    , [candy4, candy1, candy2]
    , [candy3, candy5, candy4]
    ] []

crushableGrid :: GameGrid
crushableGrid = GameGrid
    [ [candy1, candy1, candy1]
    , [candy2, candy2, candy2]
    , [candy2, candy1, bombCandy]
    ] []

gridWithEmptyCandy :: GameGrid
gridWithEmptyCandy = GameGrid
    [ [candy1, candy1, candy3]
    , [candy2, EmptyCandy, EmptyCandy]
    , [candy2, EmptyCandy, EmptyCandy]
    ] [(1, 1), (1, 2), (2, 1), (2, 2)]

-- Test contructDisappear: coordinates should be sorted
testConstructDisappear :: Test
testConstructDisappear = TestCase $ do
    let disappear = constructDisappear [(1, 2), (1, 0), (1, 3)]
    let expectedDisappear = Disappear [(1, 0), (1, 2), (1, 3)]
    assertEqual "Disappear should be equal" expectedDisappear disappear

-- Test constructActionResult: Actions should be sorted
testConstructActionResult :: Test
testConstructActionResult = TestCase $ do
    let actionResults = constructActionResult crushableGrid
            [Quit, Click (2, 1), Disappear [(0, 0), (0, 1), (0, 2)], Trigger ((2, 2), Bomb)]
        expectedActions = [Quit, Click (2, 1), Disappear [(0, 0), (0, 1), (0, 2)], Trigger ((2, 2), Bomb)]
    assertEqual "ActionResults should be equal" expectedActions (actions actionResults)

-- Test Action Parser
testParseAction :: Test
testParseAction = TestList [
    "Parse swap action" ~:
        parseAction "swap 1 2 3 4" ~?= Right (Swap (1, 2) (3, 4)),
    "Parse click action" ~:
        parseAction "click 1 2" ~?= Right (Click (1, 2)),
    "Parse undo action" ~:
        parseAction "undo" ~?= Right Undo,
    "Parse quit action" ~:
        parseAction "quit" ~?= Right Quit,
    "Parse invalid action" ~:
        parseAction "invalid" ~?= Left "Invalid action"
  ]

testSwapCandies :: Test
testSwapCandies = TestCase $ do
    let grid = swapCandies initialGrid (0, 0) (1, 0)
    let expectedGrid = GameGrid
            [ [candy4, candy2, candy3]
            , [candy2, candy1, candy2]
            , [candy3, candy5, candy4]
            ] []
    assertEqual "Grids should be equal" expectedGrid grid

testFindNormalCandyCrushables1 :: Test
testFindNormalCandyCrushables1 = TestCase $ do
    let crushables = findNormalCandyCrushables crushableGrid (0, 0)
    let expectedCrushables = [Disappear [(0, 0), (0, 1), (0, 2)]]
    assertEqual "Crushables should be equal" expectedCrushables crushables

testFindNormalCandyCrushables2 :: Test
testFindNormalCandyCrushables2 = TestCase $ do
    let crushables = findNormalCandyCrushables crushableGrid (1, 0)
    let expectedCrushables = [Disappear [(1, 0), (1, 1), (1, 2), (2, 0)]]
    assertEqual "Crushables should be equal" expectedCrushables crushables

testFindNormalCandyCrushables3 :: Test
testFindNormalCandyCrushables3 = TestCase $ do
    let crushables = findNormalCandyCrushables initialGrid (1, 1)
    let expectedCrushables = []
    assertEqual "Crushables should be equal" expectedCrushables crushables

testFindCrushables1 :: Test
testFindCrushables1 = TestCase $ do
    let crushables = findCrushables crushableGrid [(0, 0), (1, 0), (2, 2)]
    let expectedCrushables = [Disappear [(0, 0), (0, 1), (0, 2)],
                              Disappear [(1, 0), (1, 1), (1, 2), (2, 0)],
                              Trigger ((2, 2), Bomb)]
    assertEqual "Crushables should be equal" expectedCrushables crushables

testFindCrushables2 :: Test
testFindCrushables2 = TestCase $ do
    let crushables = findCrushables initialGrid [(0, 0), (0, 1), (1, 2)]
    let expectedCrushables = []
    assertEqual "Crushables should be equal" expectedCrushables crushables

testApplyDisappear :: Test
testApplyDisappear = TestCase $ do
    let grid = applyDisappear crushableGrid [(0, 0), (0, 1), (0, 2)]
    let expectedGrid = GameGrid
            [ [EmptyCandy, EmptyCandy, EmptyCandy]
            , [candy2, candy2, candy2]
            , [candy2, candy1, bombCandy]
            ] []
    assertEqual "Grids should be equal" expectedGrid grid

testApplyTrigger :: Test
testApplyTrigger = TestCase $ do
    let grid = applyTrigger crushableGrid (2, 2) Bomb
    let expectedGrid = GameGrid
            [ [candy1, candy1, candy1]
            , [candy2, EmptyCandy, EmptyCandy]
            , [candy2, EmptyCandy, EmptyCandy]
            ] []
    assertEqual "Grids should be equal" expectedGrid grid

testSwapNoCrush :: Test
testSwapNoCrush = TestCase $ do
    let grid = applySwap initialGrid (0, 0) (0, 1)
    assertEqual "Grids should be equal" initialGrid grid

testSwapCrush :: Test
testSwapCrush = TestCase $ do
    let grid = applySwap initialGrid (0, 2) (1, 2)
        expectedCrushedGrid = GameGrid
            [ [EmptyCandy, EmptyCandy, EmptyCandy]
            , [candy4, candy1, candy3]
            , [candy3, candy5, candy4]
            ] []  -- After crushing (0,0), (0,1), (0,2)
    assertEqual "Grids should be equal" expectedCrushedGrid grid

testApplyClick1 :: Test
testApplyClick1 = TestCase $ do
    let grid = applyClick crushableGrid (2, 2)
    let expectedGrid = GameGrid
            [ [candy1, candy1, candy1]
            , [candy2, EmptyCandy, EmptyCandy]
            , [candy2, EmptyCandy, EmptyCandy]
            ] []
    assertEqual "Grids should be equal" expectedGrid grid

testApplyClick2 :: Test
testApplyClick2 = TestCase $ do
    let grid = applyClick crushableGrid (0, 0)
    assertEqual "Grids should be equal" crushableGrid grid

-- Test applyAction input
testApplyAction1 :: Test
testApplyAction1 = TestCase $ do
    let grid = applyAction initialGrid (Swap (0, 0) (0, 1))
    assertEqual "Grids should be equal" initialGrid grid

testApplyAction2 :: Test
testApplyAction2 = TestCase $ do
    let grid = applyAction initialGrid (Swap (0, 2) (1, 2))
    let expectedCrushedGrid = GameGrid
            [ [EmptyCandy, EmptyCandy, EmptyCandy]
            , [candy4, candy1, candy3]
            , [candy3, candy5, candy4]
            ] []  -- After crushing (0,0), (0,1), (0,2)
    assertEqual "Grids should be equal" expectedCrushedGrid grid

testApplyAction3 :: Test
testApplyAction3 = TestCase $ do
    let grid = applyAction crushableGrid (Click (2, 2))
    let expectedCrushedGrid = GameGrid
            [ [candy1, candy1, candy1]
            , [candy2, EmptyCandy, EmptyCandy]
            , [candy2, EmptyCandy, EmptyCandy]
            ] []
    assertEqual "Grids should be equal" expectedCrushedGrid grid

testApplyAction4 :: Test
testApplyAction4 = TestCase $ do
    let grid = applyAction crushableGrid (Click (2, 1))
    assertEqual "Grids should be equal" crushableGrid grid

testApplyAction5 :: Test
testApplyAction5 = TestCase $ do
    let grid = applyAction crushableGrid (Trigger ((2, 2), Bomb))
    let expectedCrushedGrid = GameGrid
            [ [candy1, candy1, candy1]
            , [candy2, EmptyCandy, EmptyCandy]
            , [candy2, EmptyCandy, EmptyCandy]
            ] []
    assertEqual "Grids should be equal" expectedCrushedGrid grid

testApplyAction6 :: Test
testApplyAction6 = TestCase $ do
    let grid = applyAction crushableGrid (Disappear [(0, 0), (0, 1), (0, 2)])
    let expectedCrushedGrid = GameGrid
            [ [EmptyCandy, EmptyCandy, EmptyCandy]
            , [candy2, candy2, candy2]
            , [candy2, candy1, bombCandy]
            ] []
    assertEqual "Grids should be equal" expectedCrushedGrid grid

testRedeemSpecialCandy :: Test
testRedeemSpecialCandy = TestList [
    "Create a striped row candy with 4 candies" ~:
        redeemSpecialCandy (Disappear [(0, 0), (0, 1), (0, 2), (0, 3)]) ~?=
            Just stripedRowCandy,
    "Create a bomb candy with 5 candies" ~:
        redeemSpecialCandy (Disappear [(0, 0), (0, 1), (0, 2), (0, 3), (0, 4)]) ~?=
            Just bombCandy,
    "No special candy with fewer than 4 candies" ~:
        redeemSpecialCandy (Disappear [(0, 0), (0, 1), (0, 2)]) ~?= Nothing
  ]

testFillBoard :: Test
testFillBoard = TestCase $ do
    newBoard <- fillBoard (board gridWithEmptyCandy) [Circle, Square, Triangle, Heart, Star]
    assertBool "No empty candies" (EmptyCandy `notElem` concat newBoard)

instance Arbitrary Candy where
    arbitrary = oneof [Candy <$> arbitrary <*> arbitrary, pure EmptyCandy]

instance Arbitrary CandyShape where
    arbitrary = elements [Triangle .. Asterisk]

instance Arbitrary CandyEffect where
    arbitrary = elements [Normal .. StripedCross]

instance Arbitrary GameGrid where
    arbitrary = do
        dim <- chooseInt (3, 9)
        candies <- vectorOf (dim * dim) arbitrary
        let board = splitIntoRows dim candies
        return $ GameGrid board []

instance Arbitrary Action where
    arbitrary = oneof [
        Swap <$> arbitrary <*> arbitrary,
        Click <$> arbitrary,
        pure Undo,
        pure Quit,
        Trigger <$> arbitrary,
        Disappear <$> listOf arbitrary
        ]
-- Property: Swapping candies twice results in the original grid
prop_swapCandiesIdempotent :: GameGrid -> Coordinate -> Coordinate -> Property
prop_swapCandiesIdempotent grid coord1 coord2 =
    validCoords ==> 
    let swappedOnce = swapCandies grid coord1 coord2
        swappedTwice = swapCandies swappedOnce coord1 coord2
    in swappedTwice == grid
  where
    validCoords = validCoordinate grid coord1 && validCoordinate grid coord2
                  && coord1 /= coord2

-- Property: Applying an action preserves the grid size
prop_applyActionPreservesGridSize :: GameGrid -> Action -> Bool
prop_applyActionPreservesGridSize grid action =
    let newGrid = applyAction grid action
    in length (board grid) == length (board newGrid) &&
       all (\(row1, row2) -> length row1 == length row2) (zip (board grid) (board newGrid))

-- Property: all candies in a crushable group are the same
prop_findNormalCandyCrushablesMatch :: GameGrid -> Coordinate -> Property
prop_findNormalCandyCrushablesMatch grid coord =
    validCoordinate grid coord ==> 
    let crushables = findNormalCandyCrushables grid coord
    in all (allSameCandy grid) crushables
  where
    allSameCandy g (Disappear coords) =
        let candies = map (getCandyAt (board g)) coords
        in all (== head candies) (tail candies)
    allSameCandy _ _ = True

-- Property: fillAndCrushUntilStable should return a stable grid with no immediate crushables
prop_fillAndCrushUntilStable :: GameGrid -> [CandyShape] -> Property
prop_fillAndCrushUntilStable grid shapes =
    not (null (board grid)) ==> monadicIO $ do
        stableGrid <- run $ fillAndCrushUntilStable grid shapes
        let crushables = findAllCrushables stableGrid
        Test.QuickCheck.Monadic.assert $ null crushables

-- Unit tests
runUnitTests :: Test
runUnitTests = TestList
    [
        TestLabel "testParseAction" testParseAction,
        TestLabel "testSwapCandies" testSwapCandies,
        TestLabel "testFindNormalCandyCrushables1" testFindNormalCandyCrushables1,
        TestLabel "testFindNormalCandyCrushables2" testFindNormalCandyCrushables2,
        TestLabel "testFindNormalCandyCrushables3" testFindNormalCandyCrushables3,
        TestLabel "testFindCrushables1" testFindCrushables1,
        TestLabel "testFindCrushables2" testFindCrushables2,
        TestLabel "testApplyDisappear" testApplyDisappear,
        TestLabel "testApplyTrigger" testApplyTrigger,
        TestLabel "testSwapNoCrush" testSwapNoCrush,
        TestLabel "testSwapCrush" testSwapCrush,
        TestLabel "testApplyClick1" testApplyClick1,
        TestLabel "testApplyClick2" testApplyClick2,
        TestLabel "testApplyAction1" testApplyAction1,
        TestLabel "testApplyAction2" testApplyAction2,
        TestLabel "testApplyAction3" testApplyAction3,
        TestLabel "testApplyAction4" testApplyAction4,
        TestLabel "testApplyAction5" testApplyAction5,
        TestLabel "testApplyAction6" testApplyAction6
    ]
runQuickCheckTests :: IO ()
runQuickCheckTests = do
    quickCheck prop_swapCandiesIdempotent
    quickCheck prop_applyActionPreservesGridSize
    quickCheck prop_findNormalCandyCrushablesMatch
    quickCheck prop_fillAndCrushUntilStable

-- Run the tests
main :: IO Counts
main = do
    counts <- runTestTT runUnitTests
    print counts 
    runQuickCheckTests  
    return counts
