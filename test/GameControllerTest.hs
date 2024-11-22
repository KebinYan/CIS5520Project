import Test.QuickCheck
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

-- Test Action Parser
test_parseActionSwap :: Test
test_parseActionSwap = TestCase $ do
    let action = parseAction "swap 1 2 3 4"
    case action of
        Right a -> assertEqual "Action should be Swap" a (Swap (1, 2) (3, 4))
        Left err -> assertFailure $ "Parsing failed with error: " ++ show err

test_parseActionInvalid :: Test
test_parseActionInvalid = TestCase $ do
    let action = parseAction "invalid"
    case action of
        Right a -> assertFailure $ "Parsing should have failed, but got: " ++ show a
        Left _ -> return ()

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

test_swapCandies :: Test
test_swapCandies = TestCase $ do
    let grid = swapCandies initialGrid (0, 0) (1, 0)
    let expectedGrid = GameGrid
            [ [candy4, candy2, candy3]
            , [candy2, candy1, candy2]
            , [candy3, candy5, candy4]
            ] []
    assertEqual "Grids should be equal" expectedGrid grid

test_findNormalCandyCrushables1 :: Test
test_findNormalCandyCrushables1 = TestCase $ do
    let crushables = findNormalCandyCrushables crushableGrid (0, 0)
    let expectedCrushables = [Disappear [(0, 0), (0, 1), (0, 2)]]
    assertEqual "Crushables should be equal" expectedCrushables crushables

test_findNormalCandyCrushables2 :: Test
test_findNormalCandyCrushables2 = TestCase $ do
    let crushables = findNormalCandyCrushables crushableGrid (1, 0)
    let expectedCrushables = [Disappear [(1, 0), (1, 1), (1, 2), (2, 0)]]
    assertEqual "Crushables should be equal" expectedCrushables crushables

test_findNormalCandyCrushables3 :: Test
test_findNormalCandyCrushables3 = TestCase $ do
    let crushables = findNormalCandyCrushables initialGrid (1, 1)
    let expectedCrushables = []
    assertEqual "Crushables should be equal" expectedCrushables crushables

test_findCrushables1 :: Test
test_findCrushables1 = TestCase $ do
    let crushables = findCrushables crushableGrid [(0, 0), (1, 0), (2, 2)]
    let expectedCrushables = [Disappear [(0, 0), (0, 1), (0, 2)],
                              Disappear [(1, 0), (1, 1), (1, 2), (2, 0)],
                              Trigger ((2, 2), Bomb)]
    assertEqual "Crushables should be equal" expectedCrushables crushables

test_findCrushables2 :: Test
test_findCrushables2 = TestCase $ do
    let crushables = findCrushables initialGrid [(0, 0), (0, 1), (1, 2)]
    let expectedCrushables = []
    assertEqual "Crushables should be equal" expectedCrushables crushables

test_applyDisappear :: Test
test_applyDisappear = TestCase $ do
    let grid = applyDisappear crushableGrid [(0, 0), (0, 1), (0, 2)]
    let expectedGrid = GameGrid
            [ [EmptyCandy, EmptyCandy, EmptyCandy]
            , [candy2, candy2, candy2]
            , [candy2, candy1, bombCandy]
            ] []
    assertEqual "Grids should be equal" expectedGrid grid

test_applyTrigger :: Test
test_applyTrigger = TestCase $ do
    let grid = applyTrigger crushableGrid (2, 2) Bomb
    let expectedGrid = GameGrid
            [ [candy1, candy1, candy1]
            , [candy2, EmptyCandy, EmptyCandy]
            , [candy2, EmptyCandy, EmptyCandy]
            ] []
    assertEqual "Grids should be equal" expectedGrid grid

test_swapNoCrush :: Test
test_swapNoCrush = TestCase $ do
    let grid = applySwap initialGrid (0, 0) (0, 1)
    assertEqual "Grids should be equal" initialGrid grid

test_swapCrush :: Test
test_swapCrush = TestCase $ do
    let grid = applySwap initialGrid (0, 2) (1, 2)
        expectedCrushedGrid = GameGrid
            [ [EmptyCandy, EmptyCandy, EmptyCandy]
            , [candy4, candy1, candy3]
            , [candy3, candy5, candy4]
            ] []  -- After crushing (0,0), (0,1), (0,2)
    assertEqual "Grids should be equal" expectedCrushedGrid grid

test_applyClick1 :: Test
test_applyClick1 = TestCase $ do
    let grid = applyClick crushableGrid (2, 2)
    let expectedGrid = GameGrid
            [ [candy1, candy1, candy1]
            , [candy2, EmptyCandy, EmptyCandy]
            , [candy2, EmptyCandy, EmptyCandy]
            ] []
    assertEqual "Grids should be equal" expectedGrid grid

test_applyClick2 :: Test
test_applyClick2 = TestCase $ do
    let grid = applyClick crushableGrid (0, 0)
    assertEqual "Grids should be equal" crushableGrid grid

-- Test applyAction input
test_applyAction1 :: Test
test_applyAction1 = TestCase $ do
    let grid = applyAction initialGrid (Swap (0, 0) (0, 1))
    assertEqual "Grids should be equal" initialGrid grid

test_applyAction2 :: Test
test_applyAction2 = TestCase $ do
    let grid = applyAction initialGrid (Swap (0, 2) (1, 2))
    let expectedCrushedGrid = GameGrid
            [ [EmptyCandy, EmptyCandy, EmptyCandy]
            , [candy4, candy1, candy3]
            , [candy3, candy5, candy4]
            ] []  -- After crushing (0,0), (0,1), (0,2)
    assertEqual "Grids should be equal" expectedCrushedGrid grid

test_applyAction3 :: Test
test_applyAction3 = TestCase $ do
    let grid = applyAction crushableGrid (Click (2, 2))
    let expectedCrushedGrid = GameGrid
            [ [candy1, candy1, candy1]
            , [candy2, EmptyCandy, EmptyCandy]
            , [candy2, EmptyCandy, EmptyCandy]
            ] []
    assertEqual "Grids should be equal" expectedCrushedGrid grid

test_applyAction4 :: Test
test_applyAction4 = TestCase $ do
    let grid = applyAction crushableGrid (Click (2, 1))
    assertEqual "Grids should be equal" crushableGrid grid

test_applyAction5 :: Test
test_applyAction5 = TestCase $ do
    let grid = applyAction crushableGrid (Trigger ((2, 2), Bomb))
    let expectedCrushedGrid = GameGrid
            [ [candy1, candy1, candy1]
            , [candy2, EmptyCandy, EmptyCandy]
            , [candy2, EmptyCandy, EmptyCandy]
            ] []
    assertEqual "Grids should be equal" expectedCrushedGrid grid

test_applyAction6 :: Test
test_applyAction6 = TestCase $ do
    let grid = applyAction crushableGrid (Disappear [(0, 0), (0, 1), (0, 2)])
    let expectedCrushedGrid = GameGrid
            [ [EmptyCandy, EmptyCandy, EmptyCandy]
            , [candy2, candy2, candy2]
            , [candy2, candy1, bombCandy]
            ] []
    assertEqual "Grids should be equal" expectedCrushedGrid grid


-- Unit tests
tests :: Test
tests = TestList
    [
        TestLabel "test_parseActionSwap" test_parseActionSwap,
        TestLabel "test_parseActionInvalid" test_parseActionInvalid,
        TestLabel "test_swapCandies" test_swapCandies,
        TestLabel "test_findNormalCandyCrushables1" test_findNormalCandyCrushables1,
        TestLabel "test_findNormalCandyCrushables2" test_findNormalCandyCrushables2,
        TestLabel "test_findNormalCandyCrushables3" test_findNormalCandyCrushables3,
        TestLabel "test_findCrushables1" test_findCrushables1,
        TestLabel "test_findCrushables2" test_findCrushables2,
        TestLabel "test_applyDisappear" test_applyDisappear,
        TestLabel "test_applyTrigger" test_applyTrigger,
        TestLabel "test_swapNoCrush" test_swapNoCrush, 
        TestLabel "test_swapCrush" test_swapCrush,
        TestLabel "test_applyClick1" test_applyClick1,
        TestLabel "test_applyClick2" test_applyClick2,
        TestLabel "test_applyAction1" test_applyAction1,
        TestLabel "test_applyAction2" test_applyAction2,
        TestLabel "test_applyAction3" test_applyAction3,
        TestLabel "test_applyAction4" test_applyAction4,
        TestLabel "test_applyAction5" test_applyAction5,
        TestLabel "test_applyAction6" test_applyAction6
    ]

-- Run the tests
main :: IO Counts
main = runTestTT tests
