module GameUtils where

import Candy
import GHC.Base (error)
import System.Random
import Control.Monad (replicateM)

-- Generate a random Candy based on a provided list of CandyShape
-- | candyShapes: the list of available CandyShape
generateRandomCandy :: [Candy] -> IO Candy
generateRandomCandy candys = do
    candyShapes <- mapM (return . (shapeName . candy)) candys
    candySymbols <- mapM (return . (shapeIcon . candy)) candys
    randomIndex <- randomRIO (0, length candyShapes - 1)
    return Candy {
        candyDef = candyDef candys !! randomIndex,
        candyEffect = normalEffect
    }

-- Generate a list of random candies based on a provided list of CandyShape
-- | len: the number of candies to generate
-- | candyShapes: the list of available CandyShape
generateRandomCandyList :: Int -> [Candy] -> IO [Candy]
generateRandomCandyList len candys =
    replicateM len (generateRandomCandy candys)

-- Generate the special effect for a special candy based on the special effect range
generateSpecialEffect :: Candy -> (CoordinatePair -> [[Candy]] -> [[Candy]])
generateSpecialEffect candy
    | effectName (candyEffect candy) == "Circle" = generateCircleEffect candy
    | effectName (candyEffect candy) == "Rectangle" = generateRectangleEffect candy
    | effectName (candyEffect candy) == "Diamond" = generateDiamondEffect candy
    | effectName (candyEffect candy) == "Arbitrary" = generateArbitraryEffect candy
    | otherwise = error "Invalid special effect"

-- Clear a row of candies
clearRow :: Int -> [[Candy]] -> [[Candy]]
clearRow x grid =
    [if rowIdx == x then replicate (length row) emptyCandy else row
        | (rowIdx, row) <- zip [0..] grid]

-- Clear a column of candies
clearColumn :: Int -> [[Candy]] -> [[Candy]]
clearColumn y grid =
    [ [if colIdx == y then emptyCandy else candy
        | (colIdx, candy) <- zip [0..] row] | row <- grid ]

-- Clear candies in a 3x3 grid around a given coordinate
clearSurrounding :: [[Candy]] -> CoordinatePair -> [[Candy]]
clearSurrounding grid (x, y) =
    let positionsToClear = [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1]]
    in foldl clearPosition grid positionsToClear

-- Clear a candy at a given coordinate
clearPosition :: [[Candy]] -> CoordinatePair -> [[Candy]]
clearPosition board (x, y) = setCandyAt board (x, y) EmptyCandy

-- Check if a coordinate is valid
validCoordinate :: GameGrid -> CoordinatePair -> Bool
validCoordinate (GameGrid board _ _) (x, y) =
    not (null board)
    && x >= 0 && x < length board
    && y >= 0 && y < length (head board)