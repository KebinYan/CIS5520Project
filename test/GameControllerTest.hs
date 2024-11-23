module GameControllerTest where
import Test.QuickCheck ()
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
testParseActionSwap :: Test
testParseActionSwap = TestCase $ do
    let action = parseAction "swap 1 2 3 4"
    case action of
        Right a -> assertEqual "Action should be Swap" a (Swap (1, 2) (3, 4))
        Left err -> assertFailure $ "Parsing failed with error: " ++ show err

testParseActionInvalid :: Test
testParseActionInvalid = TestCase $ do
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

-- Unit tests
tests :: Test
tests = TestList
    [
        TestLabel "testParseActionSwap" testParseActionSwap,
        TestLabel "testParseActionInvalid" testParseActionInvalid,
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

-- Run the tests
main :: IO Counts
main = runTestTT tests
