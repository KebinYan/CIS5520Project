module GameState where

import Control.Monad
import Control.Monad.State
import Candy
import GameUtils
import qualified Data.List as List
import System.Random as Random
import Data.Maybe
import Data.Map (Map, update)

data Difficulty = Difficulty {
    dimension :: Int,
    candies :: [Candy],
    maxSteps :: Int
} deriving (Eq, Show)

easy, medium, hard :: Difficulty
easy = Difficulty {
    dimension = 5,
    candies = [
        Candy {
            candyDef = CandyDefinition {
                shapeName = "Circle",
                shapeIcon = "O",
                effectNameRef = "Normal"
            },
            candyEffect = normalEffect
        },
        Candy { candyDef = CandyDefinition {
          shapeName = "Minus",
          shapeIcon = "-",
          effectNameRef = "StripedRow"
        }, candyEffect = Effect {
          effectName = "StripedRow",
          effectRange = Arbitrary [(Coordinate 0, All)],
          effectRequirement = Requirement Eq 4,
          effectDescription = "placeholder"
        } }
    ],
    maxSteps = 50
}
medium = Difficulty {
    dimension = 7,
    candies = [
        Candy {
            candyDef = CandyDefinition {
                shapeName = "Circle",
                shapeIcon = "O",
                effectNameRef = "Normal"
            },
            candyEffect = normalEffect
        },
        Candy { candyDef = CandyDefinition {
          shapeName = "Minus",
          shapeIcon = "-",
          effectNameRef = "StripedRow"
        }, candyEffect = Effect {
          effectName = "StripedRow",
          effectRange = Arbitrary [(Coordinate 0, All)],
          effectRequirement = Requirement Eq 4,
          effectDescription = "placeholder"
        } }
    ],
    maxSteps = 40
}
hard = Difficulty {
    dimension = 9,
    candies = [Candy { candyDef = CandyDefinition {
                shapeName = "Triangle",
                shapeIcon = "▲",
                effectNameRef = "Normal"
                },
              candyEffect = normalEffect },
              Candy { candyDef = CandyDefinition {
                shapeName = "Square",
                shapeIcon = "■",
                effectNameRef = "Normal"
                },
              candyEffect = normalEffect },
              Candy { candyDef = CandyDefinition {
                shapeName = "Spade",
                shapeIcon = "♠",
                effectNameRef = "Normal"
                },
              candyEffect = normalEffect },
              Candy { candyDef = CandyDefinition {
                shapeName = "Heart",
                shapeIcon = "♥",
                effectNameRef = "Normal"
                },
                candyEffect = normalEffect },
              Candy {candyDef = CandyDefinition {
                shapeName = "Club",
                shapeIcon = "♣",
                effectNameRef = "Normal"
                },
                candyEffect = normalEffect },
              Candy { candyDef = CandyDefinition {
                shapeName = "Star",
                shapeIcon = "★",
                effectNameRef = "Normal"
                },
                candyEffect = normalEffect },
              Candy { candyDef = CandyDefinition {
                shapeName = "Circle",
                shapeIcon = "●",
                effectNameRef = "CircleBomb"
                },
              candyEffect = Effect {
                effectName = "CircleBomb",
                effectRange = Circle 2,
                effectRequirement = Requirement Eq 5,
                effectDescription = "placeholder"
              } },
              Candy { candyDef = CandyDefinition {
                shapeName = "Diamond",
                shapeIcon = "♦",
                effectNameRef = "DiamondBomb"
                },
              candyEffect = Effect {
                effectName = "DiamondBomb",
                effectRange = Diamond 2,
                effectRequirement = Requirement Eq 5,
                effectDescription = "placeholder"
              } },
              Candy { candyDef = CandyDefinition {
                shapeName = "Asterisk",
                shapeIcon = "*",
                effectNameRef = "Bomb"
                },
              candyEffect = Effect {
                effectName = "Bomb",
                effectRange = Rectangle 3 3,
                effectRequirement = Requirement Eq 5,
                effectDescription = "placeholder"
              } },
              Candy { candyDef = CandyDefinition {
                shapeName = "Cross",
                shapeIcon = "✚",
                effectNameRef = "StripedCross"
                },
              candyEffect = Effect {
                effectName = "StripedCross",
                effectRange = Arbitrary [(Coordinate 0, All), (All, Coordinate 0)],
                effectRequirement = Requirement Ge 5,
                effectDescription = "placeholder"
              } },
              Candy { candyDef = CandyDefinition {
                shapeName = "Minus",
                shapeIcon = "-",
                effectNameRef = "StripedRow"
              }, candyEffect = Effect {
                effectName = "StripedRow",
                effectRange = Arbitrary [(Coordinate 0, All)],
                effectRequirement = Requirement Eq 4,
                effectDescription = "placeholder"
              } }],
    maxSteps = 30
}

-- A GameGrid consists of a 2D grid of candies
data GameGrid = GameGrid {
        board :: [[Candy]],
        normalCandies :: [Candy],
        specialCandies :: Map Int [Candy],
        emptyCandyCoords :: [CoordinatePair]
    }
  deriving (Show)

-- check if two boards are equal
instance Eq GameGrid where
    (==) :: GameGrid -> GameGrid -> Bool
    (GameGrid board1 normalCandies1 specialCandies1 emptyCandyCoords1) ==
        (GameGrid board2 normalCandies2 specialCandies2 emptyCandyCoords2) =
        board1 == board2
        && normalCandies1 == normalCandies2
        && specialCandies1 == specialCandies2
        && emptyCandyCoords1 == emptyCandyCoords2

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
    let normalCandies = extractNormalCandies (candies d)
    candiesInBoard <- generateRandomCandyList (dim * dim) normalCandies
    return $ GameGrid {
        board = splitIntoRows dim candiesInBoard,
        normalCandies = normalCandies,
        specialCandies = extractSpecialCandies dim (candies d),
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
getEmptyCandyCoords :: GameGrid -> [CoordinatePair]
getEmptyCandyCoords (GameGrid _ _ _ emptyCandyCoords) = emptyCandyCoords

updateEmptyCandyCoords :: [CoordinatePair] -> GameGrid -> GameGrid
updateEmptyCandyCoords coords g = g { emptyCandyCoords = coords }

-- Add to the score
addScore :: Int -> GameMonad ()
addScore points = modify $ \s -> s { score = score s + points }

-- Check if undo is possible
undoable :: GameMonad Bool
undoable = gets (isJust . lastGrid)

-- Function to format and print the board beautifully
printGrid :: GameGrid -> IO ()
printGrid (GameGrid board _ _ _) = do
    let dim = length board
    putStrLn $ "  " ++ unwords (map show [0..(dim - 1)]) -- print column numbers
    zipWithM_ printRow [0..] board  -- print each row with row numbers
  where
    printRow :: Int -> [Candy] -> IO ()
    printRow rowNum row = putStrLn $ show rowNum ++ " " ++ unwords (map candyToSymbol row)

printStateGrid :: GameState -> IO ()
printStateGrid(GameState { currentGrid = grid }) = printGrid grid

-- Function to update the board in the game state
updateBoard :: [[Candy]] -> GameGrid -> GameGrid
updateBoard newBoard g = g { board = newBoard }

updateIOBoard :: IO [[Candy]] -> IO GameGrid -> IO GameGrid
updateIOBoard ioBoard ioGrid = do
  newBoard <- ioBoard
  updateBoard newBoard <$> ioGrid