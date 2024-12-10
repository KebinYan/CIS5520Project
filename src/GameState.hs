module GameState where
import Phd
import Control.Monad
import Control.Monad.State
import GameUtils
import qualified Data.List as List
import Data.Maybe
import Data.Map (Map, update, elems)
import Text.Printf (printf)

-- Initialize the grid based on game constants
initializeGrid :: GameConst -> IO GameGrid
initializeGrid gc = do
    let dim = dimension gc
    let normalCandies = extractNormalCandies (elems (candyMap gc))
    candiesInBoard <- generateRandomCandyList (dim * dim) normalCandies
    return $ GameGrid {
        board = splitIntoRows dim candiesInBoard,
        normalCandies = normalCandies,
        specialCandies = extractSpecialCandies dim (elems (candyMap gc)),
        crushScore = scorePerCandy gc,
        effectScore = scorePerEffect gc,
        scoreChange = 0
    }

-- Split a list into rows of a given length
splitIntoRows :: Int -> [a] -> [[a]]
splitIntoRows n = go
  where
    go [] = []
    go ys = let (row, rest) = splitAt n ys in row : go rest

-- Initialize the game state
initializeGameState :: GameConst -> IO GameState
initializeGameState gc = do
    grid <- initializeGrid gc
    return $ GameState {
        currentGrid = grid,
        gameConst = gc,
        lastGrid = Nothing,
        remainingSteps = maxSteps gc,
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

-- Add to the score
addScore :: Int -> GameMonad ()
addScore points = modify $ \s -> s { score = score s + points }

-- Reset changed score to 0 for each step
resetScoreChange :: GameMonad ()
resetScoreChange = modify $ \s -> s {
    currentGrid = (currentGrid s) { scoreChange = 0 }
}

-- Check if undo is possible
undoable :: GameMonad Bool
undoable = gets (isJust . lastGrid)

-- Function to format and print the board beautifully
printGrid :: GameGrid -> IO ()
printGrid (GameGrid board _ _ _ _ _) = do
    let dim = length board
    -- Print column numbers, formatted to match cell widths
    putStrLn $ "  " ++ unwords (map (printf "%2d") [0 .. (dim - 1)])
    -- Print rows with row numbers
    zipWithM_ printRow [0 ..] board
  where
    -- Print a single row with row number and candies
    printRow :: Int -> [Candy] -> IO ()
    printRow rowNum row = do
        let rowStr = unwords (map (printf "%2s" . candyToSymbol) row)
        putStrLn $ printf "%2d %s" rowNum rowStr


printStateGrid :: GameState -> IO ()
printStateGrid(GameState { currentGrid = grid }) = printGrid grid

-- Function to update the board in the game state
updateBoard :: [[Candy]] -> GameGrid -> GameGrid
updateBoard newBoard g = g { board = newBoard }

updateScore :: Int -> GameGrid -> GameGrid
updateScore newScore g = g {
    scoreChange = scoreChange g + newScore
}