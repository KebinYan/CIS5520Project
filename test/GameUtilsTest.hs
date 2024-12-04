module GameUtilsTest where
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Monadic
import TestUtils
import Data.List
import Data.Maybe

import GameUtils
import Phd

-- Unit test for generateRandomCandy
testGenerateRandomCandy :: Test
testGenerateRandomCandy = TestCase $ do
    let candies = [candy1, candy2, candy3, candy4]
    candy <- generateRandomCandy candies
    let candyShapes = map (shapeName . candyDef) candies
    assertBool "Generated candy shape is within the provided list"
        (shapeName (candyDef candy) `elem` candyShapes)
    assertEqual "Generated candy effect is Normal"
        "Normal" (effectName (candyEffect candy))

-- Unit test for generateRandomCandyList
testGenerateRandomCandyList :: Test
testGenerateRandomCandyList = TestCase $ do
    let candies = [candy1, candy2, candy3, candy4]
    generatedCandies <- generateRandomCandyList 10 candies
    let candyShapes = map (shapeName . candyDef) candies
    assertEqual "Correct number of candies generated" 10 (length generatedCandies)
    assertBool "All candy shapes are within the provided list"
        (all ((`elem` candyShapes) . shapeName . candyDef) generatedCandies)
    assertBool "All candy effects are Normal"
        (all ((== "Normal") . effectName . candyEffect) generatedCandies)

-- Unit test for generateSpecialEffect
testComputeCirclePositions :: Test
testComputeCirclePositions = TestCase $ do
    let positions = computeCirclePositions (2, 2) 1
        expected = sort [
            (Coordinate 1, Coordinate 2),
            (Coordinate 2, Coordinate 1),
            (Coordinate 2, Coordinate 2),
            (Coordinate 2, Coordinate 3),
            (Coordinate 3, Coordinate 2)]
    assertEqual "computeCirclePositions" (sort positions) expected

testComputeRectanglePositions :: Test
testComputeRectanglePositions = TestCase $ do
    let positions = computeRectanglePositions (2, 2) 3 2
        expected = sort [
            (Coordinate 1, Coordinate 1),
            (Coordinate 1, Coordinate 2),
            (Coordinate 2, Coordinate 1),
            (Coordinate 2, Coordinate 2),
            (Coordinate 3, Coordinate 1),
            (Coordinate 3, Coordinate 2)]
    assertEqual "computeRectanglePositions" (sort positions) expected

testComputeDiamondPositions :: Test
testComputeDiamondPositions = TestCase $ do
    let positions = computeDiamondPositions (2, 2) 1
        expected = sort [
            (Coordinate 2, Coordinate 2),
            (Coordinate 1, Coordinate 2),
            (Coordinate 2, Coordinate 1),
            (Coordinate 2, Coordinate 3),
            (Coordinate 3, Coordinate 2)]
    assertEqual "computeDiamondPositions" (sort positions) expected

testComputeArbitraryPositions :: Test
testComputeArbitraryPositions = TestCase $ do
    let boardSize = (3, 3)
        positions = computeArbitraryPositions boardSize (2, 1) [(Coordinate 0, All), (All, Coordinate 0)]
        expected = sort [
            (Coordinate 0, Coordinate 1),
            (Coordinate 1, Coordinate 1),
            (Coordinate 2, Coordinate 0),
            (Coordinate 2, Coordinate 1),
            (Coordinate 2, Coordinate 2)]
    assertEqual "computeArbitraryPositions" (sort positions) expected

testGenerateSpecialEffect1 :: Test
testGenerateSpecialEffect1 = TestCase $ do
    let testBoard = board crushableGrid
        coord = (Coordinate 2, Coordinate 2)
        newBoard = generateSpecialEffect candy5 coord testBoard
    let positionsCleared = computeRectanglePositions (2, 2) 3 3
        candiesAtClearedPositions = map (getCandyAt newBoard) positionsCleared
    assertBool "generateSpecialEffect - bomb" $
        all (== Just EmptyCandy) candiesAtClearedPositions

testGenerateSpecialEffect2 :: Test
testGenerateSpecialEffect2 = TestCase $ do
    let testBoard = board crushableGrid
        coord = (Coordinate 2, Coordinate 1)
        newBoard = generateSpecialEffect candy6 coord testBoard
    let positionsCleared = computeArbitraryPositions (3, 3) (2, 1) [(Coordinate 0, All), (All, Coordinate 0)]
        candiesAtClearedPositions = map (getCandyAt newBoard) positionsCleared
    assertBool "generateSpecialEffect - cross" $
        all (== Just EmptyCandy) candiesAtClearedPositions

testGetCandyAt :: Test
testGetCandyAt = TestCase $ do
    assertEqual "Get valid candy" (Just candy2) (getCandyAt (board initialGrid) (Coordinate 0, Coordinate 1))
    assertEqual "Get invalid candy" Nothing (getCandyAt (board initialGrid) (Coordinate 3, Coordinate 3))

testSetCandyAt :: Test
testSetCandyAt = TestCase $ do
    let updatedBoard = setCandyAt (board initialGrid) (Coordinate 0, Coordinate 1) candy6
    case getCandyAt updatedBoard (Coordinate 0, Coordinate 1) of
        Just candy -> assertEqual "Set valid candy" candy6 candy
        Nothing -> assertFailure "Candy not found at (0,1)"
    case getCandyAt (board initialGrid) (Coordinate 0, Coordinate 1) of
        Just candy -> assertEqual "Original board unchanged" candy2 candy
        Nothing -> assertFailure "Candy not found at (0,0)"

testSetCandyAtBoundary :: Test
testSetCandyAtBoundary = TestCase $ do
    let updatedBoard = setCandyAt (board initialGrid) (Coordinate (-1), Coordinate 0) candy6
    assertEqual "Set candy at negative index should not change board" (board initialGrid) updatedBoard
    let updatedBoard2 = setCandyAt (board initialGrid) (Coordinate 0, Coordinate 100) candy6
    assertEqual "Set candy at out-of-bounds index should not change board" (board initialGrid) updatedBoard2

testGetCandyAtBoundary :: Test
testGetCandyAtBoundary = TestCase $ do
    let result = getCandyAt (board initialGrid) (Coordinate (-1), Coordinate 0)
    assertEqual "Get candy at negative index should return Nothing" Nothing result
    let result2 = getCandyAt (board initialGrid) (Coordinate 0, Coordinate 100)
    assertEqual "Get candy at out-of-bounds index should return Nothing" Nothing result2

testClearPosition :: Test
testClearPosition = TestCase $ do
    let grid = clearPosition (board initialGrid) (Coordinate 1, Coordinate 1)
    let expectedBoard = 
            [ [candy2, candy2, candy3]
            , [candy4, EmptyCandy, candy2]
            , [candy3, candy5, candy4]
            ]
    assertEqual "Grids should be equal" expectedBoard grid

testClearPositionWithSpecialEffect :: Test
testClearPositionWithSpecialEffect = TestCase $ do
    let grid = clearPosition (board crushableGrid) (Coordinate 2, Coordinate 2)
    let expectedBoard = 
            [ [candy1, EmptyCandy, candy1, candy3]
            , [candy2, EmptyCandy, EmptyCandy, EmptyCandy]
            , [EmptyCandy, EmptyCandy, EmptyCandy, EmptyCandy]
            , [candy3, EmptyCandy, EmptyCandy, EmptyCandy]
            ]
    assertEqual "Grids should be equal" expectedBoard grid

testValidCoordinate :: Test
testValidCoordinate = TestCase $ do
    assertBool "Valid coordinate" (validCoordinate (board initialGrid) (Coordinate 0, Coordinate 0))
    assertBool "Invalid coordinate" (not $ validCoordinate (board initialGrid) (Coordinate 3, Coordinate 0))
    assertBool "Invalid coordinate" (not $ validCoordinate (board initialGrid) (Coordinate 0, Coordinate (-1)))
    assertBool "Invalid coordinate" (not $ validCoordinate (board initialGrid) (Coordinate 0, All))

testRedeemSpecialCandy :: Test
testRedeemSpecialCandy = TestList [
    "Create a striped row candy with 4 candies" ~:
        do result <- redeemSpecialCandy 4 (specialCandies crushableGrid)
           assertEqual "Redeem special candy with 4 candies" (Just candy7) result,
    "Create a bomb candy or a striped cross candy with 5 candies" ~:
        do result <- redeemSpecialCandy 5 (specialCandies crushableGrid)
           assertBool "Redeem special candy with 5 candies" 
            (result == Just candy5 || result == Just candy6),
    "Create a striped cross candy with 6 candies" ~:
        do result <- redeemSpecialCandy 6 (specialCandies crushableGrid)
           assertEqual "Redeem special candy with 6 candies" (Just candy6) result,
    "No special candy with fewer than 4 candies" ~:
        do result <- redeemSpecialCandy 3 (specialCandies crushableGrid)
           assertEqual "No special candy with fewer than 4 candies" Nothing result
  ]

testFillRow :: Test
testFillRow = TestCase $ do
    let row = [EmptyCandy, EmptyCandy, EmptyCandy, EmptyCandy]
    newRow <- fillRow row (normalCandies crushableGrid)
    assertBool "No empty candies" (EmptyCandy `notElem` newRow)

testFillBoard :: Test
testFillBoard = TestCase $ do
    newBoard <- fillBoard (board gridWithEmptyCandy) (normalCandies gridWithEmptyCandy)
    assertBool "No empty candies" (EmptyCandy `notElem` concat newBoard)

testAllCoordinates :: Test
testAllCoordinates = TestCase $ do
    let coords = allCoordinates (board initialGrid)
    let expectedCoords = 
            [ (Coordinate 0, Coordinate 0),
              (Coordinate 0, Coordinate 1),
              (Coordinate 0, Coordinate 2),
              (Coordinate 1, Coordinate 0),
              (Coordinate 1, Coordinate 1),
              (Coordinate 1, Coordinate 2),
              (Coordinate 2, Coordinate 0),
              (Coordinate 2, Coordinate 1),
              (Coordinate 2, Coordinate 2) ]
    assertEqual "Coordinates should be equal" expectedCoords coords

-- QuickCheck properties
-- Property: Only valid coordinates are cleared by generateSpecialEffect
prop_validCoordinatesCleared :: Difficulty -> Property
prop_validCoordinatesCleared d = monadicIO $ do
    normalCandies <- run $ generate (listOf1 (genArbNormalCandy d))
    board <- run $ generate (genGameBoard d (return normalCandies))
    coord <- run $ generate (genArbIntCoordPair d)
    specialCandy <- run $ generate (genArbSpecialCandy d)
    boardAfterEffect <- run $ return (generateSpecialEffect specialCandy coord board)
    let positionsCleared = case effectRange (candyEffect specialCandy) of
            Circle r -> computeCirclePositions (getCoord coord) r
            Rectangle w h -> computeRectanglePositions (getCoord coord) w h
            Diamond r -> computeDiamondPositions (getCoord coord) r
            Arbitrary coords -> computeArbitraryPositions (length board, length (head board)) (getCoord coord) coords
    let clearedPositions = filter (validCoordinate board) positionsCleared
    Test.QuickCheck.Monadic.assert $
        all ((Just EmptyCandy ==) . getCandyAt boardAfterEffect) clearedPositions

-- Property: Positions outside the effect range remain unchanged
prop_noUnintendedModifications :: Difficulty -> Property
prop_noUnintendedModifications d = monadicIO $ do
    normalCandies <- run $ generate (listOf1 (genArbNormalCandy d))
    board <- run $ generate (genGameBoard d (return normalCandies))
    coord <- run $ generate (genArbIntCoordPair d)
    specialCandy <- run $ generate (genArbSpecialCandy d)
    let positionsToClear = case effectRange (candyEffect specialCandy) of
            Circle r -> computeCirclePositions (getCoord coord) r
            Rectangle w h -> computeRectanglePositions (getCoord coord) w h
            Diamond r -> computeDiamondPositions (getCoord coord) r
            Arbitrary coords -> 
                computeArbitraryPositions (length board, length (head board)) 
                (getCoord coord) coords
        unaffectedPositions =
            filter (`notElem` (coord : positionsToClear)) (allCoordinates board)
    boardAfterEffect <-
        run $ return (generateSpecialEffect specialCandy coord board)
    let unaffectedCandies = map (getCandyAt board) unaffectedPositions
        affectedCandies = map (getCandyAt boardAfterEffect) unaffectedPositions
    Test.QuickCheck.Monadic.assert $ unaffectedCandies == affectedCandies

-- Property: Filling a board twice has the same effect as filling it once
prop_fillBoardIdempotency :: Difficulty -> Property
prop_fillBoardIdempotency d = monadicIO $ do
    normalCandies <- run $ generate (listOf1 (genArbNormalCandy d))
    board <- run $ generate (genGameBoardWithEmpty (return normalCandies))
    filledOnce <- run $ fillBoard board normalCandies
    filledTwice <- run $ fillBoard filledOnce normalCandies
    Test.QuickCheck.Monadic.assert $ filledOnce == filledTwice

-- Property: Applying the same special effect twice is equivalent to applying it once
prop_specialEffectIdempotency :: Difficulty -> Property
prop_specialEffectIdempotency d = monadicIO $ do
    normalCandies <- run $ generate (listOf1 (genArbNormalCandy d))
    board <- run $ generate (genGameBoard d (return normalCandies))
    coord <- run $ generate (genArbIntCoordPair d)
    specialCandy <- run $ generate (genArbSpecialCandy d)
    boardAfterFirst <- run $ return (generateSpecialEffect specialCandy coord board)
    boardAfterSecond <- run $ return (generateSpecialEffect specialCandy coord boardAfterFirst)
    Test.QuickCheck.Monadic.assert $ boardAfterFirst == boardAfterSecond

-- Property: Applying two special effects in any order is equivalent to applying them in the opposite order
prop_specialEffectAssociativity :: Difficulty -> Property
prop_specialEffectAssociativity d = monadicIO $ do
    normalCandies <- run $ generate (listOf1 (genArbNormalCandy d))
    board <- run $ generate (genGameBoard d (return normalCandies))
    coord1 <- run $ generate (genArbIntCoordPair d)
    coord2 <- run $ generate (genArbIntCoordPair d)
    candy1 <- run $ generate (genArbSpecialCandy d)
    candy2 <- run $ generate (genArbSpecialCandy d)
    let board1 = generateSpecialEffect candy1 coord1
            (generateSpecialEffect candy2 coord2 board)
        board2 = generateSpecialEffect candy2 coord2
            (generateSpecialEffect candy1 coord1 board)
    Test.QuickCheck.Monadic.assert $ board1 == board2

-- Property: Applying generateSpecialEffect is equivalent to applying clearPosition
-- if the candy at the coordinate is a special candy
prop_specialEffectEquivalent :: Difficulty -> Property
prop_specialEffectEquivalent d = monadicIO $ do
    grid <- run $ generate (genGameGrid d)
    coord <- run $ generate (genArbIntCoordPair d)
    specialCandy <- run $ generate (genArbSpecialCandy d)
    let updatedBoard = setCandyAt (board grid) coord specialCandy
        boardAfterEffect = generateSpecialEffect specialCandy coord updatedBoard
        boardAfterClear = clearPosition updatedBoard coord
    Test.QuickCheck.Monadic.assert $ boardAfterEffect == boardAfterClear

-- Property: No EmptyCandy remains after applying generateSpecialEffect and fillBoard
prop_noEmptyCandyAfterFill :: Difficulty -> Property
prop_noEmptyCandyAfterFill d = monadicIO $ do
    normalCandies <- run $ generate (listOf1 (genArbNormalCandy d))
    board <- run $ generate (genGameBoard d (return normalCandies))
    coord <- run $ generate (genArbIntCoordPair d)
    specialCandy <- run $ generate (genArbSpecialCandy d)
    boardAfterEffect <- run $ return (generateSpecialEffect specialCandy coord board)
    filledBoard <- run $ fillBoard boardAfterEffect normalCandies
    let allCandies = concat filledBoard
    Test.QuickCheck.Monadic.assert $ notElem EmptyCandy allCandies

-- Property: Setting and then getting a candy at a valid coordinate retrieves the same candy
prop_setGetCandyAt :: Difficulty -> Property
prop_setGetCandyAt d = monadicIO $ do
    normalCandies <- run $ generate (listOf1 (genArbNormalCandy d))
    board <- run $ generate (genGameBoard d (return normalCandies))
    -- make sure to generate a valid coordinate
    coord <- run $ generate (genArbIntCoordPair d)
    -- make sure to generate a different candy
    newCandy <- run $ generate (genArbSpecialCandy d)
    let newBoard = setCandyAt board coord newCandy
        retrievedCandy = getCandyAt newBoard coord
    Test.QuickCheck.Monadic.assert $ retrievedCandy == Just newCandy

-- Helper function
getCoord :: CoordinatePair -> (Int, Int)
getCoord (Coordinate x, Coordinate y) = (x, y)
getCoord (All, y) = error "Invalid coordinate: All"
getCoord (x, All) = error "Invalid coordinate: All"

-- Run tests
runUnitTests :: IO Counts
runUnitTests = runTestTT $ TestList [
    TestLabel "testGenerateRandomCandy" testGenerateRandomCandy,
    TestLabel "testGenerateRandomCandyList" testGenerateRandomCandyList,
    TestLabel "testComputeCirclePositions" testComputeCirclePositions,
    TestLabel "testComputeRectanglePositions" testComputeRectanglePositions,
    TestLabel "testComputeDiamondPositions" testComputeDiamondPositions,
    TestLabel "testComputeArbitraryPositions" testComputeArbitraryPositions,
    TestLabel "testGenerateSpecialEffect1" testGenerateSpecialEffect1,
    TestLabel "testGenerateSpecialEffect2" testGenerateSpecialEffect2,
    TestLabel "testGetCandyAt" testGetCandyAt,
    TestLabel "testSetCandyAt" testSetCandyAt,
    TestLabel "testSetCandyAtBoundary" testSetCandyAtBoundary,
    TestLabel "testGetCandyAtBoundary" testGetCandyAtBoundary,
    TestLabel "testClearPosition" testClearPosition,
    TestLabel "testClearPositionWithSpecialEffect" testClearPositionWithSpecialEffect,
    TestLabel "testValidCoordinate" testValidCoordinate,
    TestLabel "testRedeemSpecialCandy" testRedeemSpecialCandy,
    TestLabel "testFillRow" testFillRow,
    TestLabel "testFillBoard" testFillBoard,
    TestLabel "testAllCoordinates" testAllCoordinates
    ]

runQuickCheckTests :: IO ()
runQuickCheckTests = do
    putStrLn "prop_validCoordinatesCleared:"
    quickCheck prop_validCoordinatesCleared
    putStrLn "prop_noUnintendedModifications:"
    quickCheck prop_noUnintendedModifications
    putStrLn "prop_fillBoardIdempotency:"
    quickCheck prop_fillBoardIdempotency
    putStrLn "prop_specialEffectIdempotency:"
    quickCheck prop_specialEffectIdempotency
    putStrLn "prop_specialEffectAssociativity:"
    quickCheck prop_specialEffectAssociativity
    putStrLn "prop_specialEffectEquivalent:"
    quickCheck prop_specialEffectEquivalent
    putStrLn "prop_noEmptyCandyAfterFill:"
    quickCheck prop_noEmptyCandyAfterFill
    putStrLn "prop_setGetCandyAt:"
    quickCheck prop_setGetCandyAt

-- main :: IO ()
-- main = do
--     runUnitTests >>= print
--     runQuickCheckTests