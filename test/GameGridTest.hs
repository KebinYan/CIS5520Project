{-# LANGUAGE FlexibleInstances #-}

module GameGridTest where

import Test.QuickCheck
import GameGrid
import Candy
import Control.Monad (liftM2)

-- Arbitrary instance for Difficulty
instance Arbitrary Difficulty where
    arbitrary = elements [easy, medium, hard]

-- Generate a random GameGrid for a given Difficulty
arbitraryGameGrid :: Difficulty -> Gen GameGrid
arbitraryGameGrid diff = do
    let dim = dimension diff
    candies <- vectorOf (dim * dim) (arbitraryCandy (candyShapes diff))
    return GameGrid {
        board = splitIntoRows dim candies,
        emptyCandyCoords = []  -- Assume no empty candies for simplicity
    }

-- Arbitrary instance for Candy
arbitraryCandy :: [CandyShape] -> Gen Candy
arbitraryCandy shapes = do
    shape <- elements shapes
    return Candy { candyShape = shape, candyEffect = Normal }

-- Property: Grid dimensions match difficulty
prop_gridDimensionsMatch :: Difficulty -> Property
prop_gridDimensionsMatch diff = forAll (arbitraryGameGrid diff) $ \grid ->
    let dim = dimension diff
        b = board grid
    in length b == dim && all (\row -> length row == dim) b

-- Property: All candies are valid
prop_allCandiesValid :: Difficulty -> Property
prop_allCandiesValid diff = forAll (arbitraryGameGrid diff) $ \grid ->
    all (all (\candy -> candyShape candy `elem` candyShapes diff)) (board grid)

-- Property: Empty candy coordinates are consistent
prop_emptyCoordsConsistent :: Difficulty -> Property
prop_emptyCoordsConsistent diff = forAll (arbitraryGameGrid diff) $ \grid ->
    let emptyCoords = [(x, y) | (x, row) <- zip [0..] (board grid),
                                (y, candy) <- zip [0..] row,
                                candy == EmptyCandy]
    in emptyCandyCoords grid == emptyCoords


runTests :: IO Bool
runTests = $quickCheckAll