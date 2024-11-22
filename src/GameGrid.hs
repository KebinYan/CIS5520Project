module GameGrid where

import Control.Monad.State
import Candy
import GameUtils (generateRandomCandyList)
import qualified Data.List as List
import System.Random as Random
import Data.Maybe

type Coordinate = (Int, Int)

data Difficulty = Difficulty {
    dimension :: Int,
    candyShapes :: [CandyShape],
    maxSteps :: Int
} deriving (Eq, Show)

easy, medium, hard :: Difficulty
easy = Difficulty {
    dimension = 5,
    candyShapes = [Triangle, Circle, Square, Star],
    maxSteps = 50
}
medium = Difficulty {
    dimension = 7,
    candyShapes = [Triangle, Circle, Square, Star, Heart],
    maxSteps = 40
}
hard = Difficulty {
    dimension = 9,
    candyShapes = [Triangle, Circle, Square, Star, Heart , Diamond],
    maxSteps = 30
}

-- A GameGrid consists of a 2D grid of candies
data GameGrid = GameGrid {
        board :: [[Candy]],
        emptyCandyCoords :: [Coordinate]
    }
  deriving (Show)

-- check if two boards are equal
instance Eq GameGrid where
    (==) :: GameGrid -> GameGrid -> Bool
    (GameGrid board1 _) == (GameGrid board2 _) = board1 == board2

-- The game state includes the current grid, difficulty level, history for undo, and remaining steps
data GameState = GameState {
    currentGrid :: GameGrid,
    difficulty :: Difficulty,
    lastGrid :: Maybe GameGrid,  -- previous grids for undo
    remainingSteps :: Int,  -- number of steps left
    score :: Int            -- current score
} deriving (Eq, Show)

type GameMonad = StateT GameState IO

-- Initialize the grid based on difficulty
initializeGrid :: Difficulty -> IO GameGrid
initializeGrid d = do
    let dim = dimension d
    candies <- generateRandomCandyList (dim * dim) (candyShapes d)
    return $ GameGrid {
        board = splitIntoRows dim candies,
        emptyCandyCoords = []
    }

-- Split a list into rows of a given length
splitIntoRows :: Int -> [a] -> [[a]]
splitIntoRows n = go
  where
    go [] = []
    go ys = let (row, rest) = splitAt n ys in row : go rest

-- Initialize the game state
initializeGameState :: Difficulty -> IO GameState
initializeGameState d = do
    grid <- initializeGrid d
    return $ GameState {
        currentGrid = grid,
        difficulty = d,
        lastGrid = Nothing,
        remainingSteps = maxSteps d,
        score = 0
    }

-- Get the current grid
getCurrentGrid :: GameMonad GameGrid
getCurrentGrid = gets currentGrid

-- Update the grid (save the current grid to `lastGrid` for undo)
updateGridState :: GameGrid -> GameMonad ()
updateGridState newGrid = modify $ \s -> s {
    lastGrid = Just (currentGrid s), -- save the current grid
    currentGrid = newGrid,
    remainingSteps = remainingSteps s - 1  -- update the remaining steps
}

-- Undo the last step
undoStep :: GameMonad ()
undoStep = modify $ \s -> case lastGrid s of
    Nothing -> s  -- no last grid to restore
    Just prevGrid -> s {
        currentGrid = prevGrid,
        lastGrid = Nothing, -- undo only once
        remainingSteps = remainingSteps s + 1  -- restore the step
    }

-- Get the remaining steps
getRemainingSteps :: GameMonad Int
getRemainingSteps = gets remainingSteps

-- Get the emptyCandyCoords
getEmptyCandyCoords :: GameGrid -> [Coordinate]
getEmptyCandyCoords (GameGrid _ emptyCandyCoords) = emptyCandyCoords

-- Add to the score
addScore :: Int -> GameMonad ()
addScore points = modify $ \s -> s { score = score s + points }

-- Check if undo is possible
undoable :: GameMonad Bool
undoable = gets (isJust . lastGrid)

-- TODO: beautilful print
-- printGrid :: GameGrid -> IO ()
-- printGrid (GameGrid board _) = mapM_ print board

-- clone current grid
cloneGrid :: GameGrid -> GameGrid
cloneGrid (GameGrid board emptyCandyCoords) = GameGrid board emptyCandyCoords

-- get candy at a specific position
getCandyAt :: [[Candy]] -> (Int, Int) -> Maybe Candy
getCandyAt board (x, y)
    | x < 0 || x >= length board = Nothing
    | y < 0 || y >= length (head board) = Nothing
    | otherwise = Just $ (board !! x) !! y

-- set candy at a specific position
setCandyAt :: [[Candy]] -> (Int, Int) -> Candy -> [[Candy]]
setCandyAt board (x, y) newCandy
    | x < 0 || x >= length board = board
    | y < 0 || y >= length (head board) = board
    | otherwise =
        let (before, row:after) = splitAt x board
            (left, _:right) = splitAt y row
            newRow = left ++ [newCandy] ++ right
            newBoard = before ++ [newRow] ++ after
        in newBoard
