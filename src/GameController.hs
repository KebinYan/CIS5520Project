-- GameController.hs

module GameController where
import Test.QuickCheck

import Control.Monad
import Control.Monad.State
import System.IO (hFlush, stdout)
import Data.Maybe
import Data.List
import qualified Data.Set as Set
import Control.Applicative
import Control.Monad.IO.Class

import GameGrid
import Candy
import GameUtils
import Parser hiding (intP)
import Prelude

-- Define actions
data Action = Swap CoordinatePair CoordinatePair
            | Click CoordinatePair
            | Undo
            | Quit
            | Trigger (CoordinatePair, CandyEffect)
            | Disappear [CoordinatePair]
    deriving (Eq, Show)

-- Constructor for Disappear action that sorts the coordinates
constructDisappear :: [CoordinatePair] -> Action
constructDisappear = Disappear . sortCoordinates
    where
        sortCoordinates =
            sortBy (\(x1, y1) (x2, y2) -> compare (x1, y1) (x2, y2))

-- define the precedence of actions
instance Ord Action where
    compare :: Action -> Action -> Ordering
    compare Quit _ = LT
    compare Undo _ = LT
    compare (Swap _ _) _ = LT
    compare (Click _) _ = LT
    compare (Disappear _) _ = LT
    compare (Trigger _) _ = LT

-- Define action result
data ActionResult = ActionResult {
    grid :: GameGrid,
    actions :: [Action]
} deriving (Eq, Show)

-- Constructor for ActionResult with sorted actions
constructActionResult :: GameGrid -> [Action] -> ActionResult
constructActionResult g actions = ActionResult g (sort actions)

-- The main game loop
gameLoop :: Difficulty -> IO ()
gameLoop d = do
    initialState <- initializeGameState d
    state <- fillAndCrushStateIO initialState
    printGridState state
    gameStep state

gameStep :: GameState -> IO ()
gameStep state = do
    let grid = currentGrid state
        stepsRemaining = remainingSteps state
    putStrLn $ "Remaining Steps: " ++ show stepsRemaining
    putStr "Enter your action (swap x1 y1 x2 y2 / click x y / undo / quit): "
    hFlush stdout

    input <- getLine
    case parseAction input of
        Right action ->
            case action of
                Quit -> putStrLn "Quitting game"
                _ -> do
                    updatedState <- handleAction True state action
                    stableState <- fillAndCrushStateIO updatedState
                    printGridState stableState
                    gameStep stableState
        Left _       -> do
            putStrLn "Invalid action, please try again."
            gameStep state

parseAction :: String -> Either ParseError Action
parseAction = parse actionParser

actionParser :: Parser Action
actionParser = wsP $ choice
    [ Swap <$> (string "swap" *>
                ((,) <$> wsP intP <*> wsP intP)) <*>
                ((,) <$> wsP intP <*> wsP intP)
    , Click <$> (string "click" *> ((,) <$> wsP intP <*> wsP intP))
    , string "undo" *> pure Undo
    , string "quit" *> pure Quit
    ]

handleAction :: Bool -> GameState -> Action -> IO GameState
handleAction verbose state action = case action of
    Swap (x1, y1) (x2, y2) -> do
        when verbose $
            putStrLn $
                "Swapping (" ++ show x1 ++ "," ++ show y1 ++ ") with ("
                ++ show x2 ++ "," ++ show y2 ++ ")"
        let newGrid = applySwap (currentGrid state) (x1, y1) (x2, y2)
        (_, newState) <- runStateT (updateGridState newGrid) state
        when verbose $ printGrid newGrid
        return newState
    Click (x, y) -> do
        when verbose $
            putStrLn $ "Clicking on (" ++ show x ++ "," ++ show y ++ ")"
        let newGrid = applyClick (currentGrid state) (x, y)
        (_, newState) <- runStateT (updateGridState newGrid) state
        when verbose $ printGrid newGrid
        return newState
    Undo -> do
        when verbose $ putStrLn "Undoing last action"
        (_, newState) <- runStateT undoStep state
        when verbose $ printGrid (currentGrid state)
        return newState
    _ -> do
        when verbose $ putStrLn "Invalid action"
        return state

applyAction :: GameGrid -> Action -> GameGrid
applyAction g (Disappear coords) = applyDisappear g coords
applyAction g (Trigger (coordinate, candyEffect)) =
    applyTrigger g coordinate candyEffect
applyAction g (Swap (x1, y1) (x2, y2)) = applySwap g (x1, y1) (x2, y2)
applyAction g (Click (x, y)) = applyClick g (x, y)
applyAction g Undo = g
applyAction g Quit = g

applySwap :: GameGrid -> CoordinatePair -> CoordinatePair -> GameGrid
applySwap g (x1, y1) (x2, y2)
    | not (validCoordinate g (x1 , y1)) || not (validCoordinate g (x2, y2)) = g
    | abs (x1 - x2) + abs (y1 - y2) /= 1 = g
    | otherwise =
        let newGrid = swapCandies g (x1, y1) (x2, y2)
            crushables = findCrushables newGrid [(x1, y1), (x2, y2)]
        in case crushables of
            [] -> swapCandies newGrid (x1, y1) (x2, y2)
            _ -> foldl applyAction newGrid crushables

swapCandies :: GameGrid -> CoordinatePair -> CoordinatePair -> GameGrid
swapCandies g (x1, y1) (x2, y2) =
    let c1 = getCandyAt (board g) (x1, y1)
        c2 = getCandyAt (board g) (x2, y2)
        newGrid = case (c1, c2) of
            (Just candy1, Just candy2) ->
                GameGrid (setCandyAt (setCandyAt (board g) (x1, y1) candy2) (x2, y2) candy1)
                         (getEmptyCandyCoords g)
            _ -> g
    in newGrid

findCrushables :: GameGrid -> [CoordinatePair] -> [Action]
findCrushables grid = concatMap (processCoord grid)
    where
        processCoord :: GameGrid -> CoordinatePair -> [Action]
        processCoord g coord =
            let candy = getCandyAt (board g) coord
            in case candy of
                Nothing -> []
                Just EmptyCandy -> []
                Just c -> case candyEffect c of
                    Normal -> findNormalCandyCrushables g coord
                    _ -> [Trigger (coord, candyEffect c)]

-- Find all normal candies that can be crushed starting from a given coordinate
-- Returns a list of Disappear actions
findNormalCandyCrushables :: GameGrid -> CoordinatePair -> [Action]
findNormalCandyCrushables grid coord =
    case getCandyAt (board grid) coord of
        Nothing -> []  -- no candy at the given coordinate, return no actions
        Just candy ->
            let visited = explore coord grid candy (Set.singleton coord)
            in [constructDisappear (Set.toList visited) | Set.size visited >= 3]
            -- create a Disappear action if at least 3 candies match
  where
    -- four cardinal directions: up, down, left, right
    directions :: [(Int, Int)]
    directions = [(1, 0), (-1, 0), (0, 1), (0, -1)]

    -- explore at most two steps in each direction
    explore :: CoordinatePair -> GameGrid -> Candy ->
        Set.Set CoordinatePair -> Set.Set CoordinatePair
    explore start grid candy visited =
        foldl (checkDirection start grid candy) visited directions

    -- check a single direction for matching candies
    checkDirection :: CoordinatePair -> GameGrid -> Candy ->
        Set.Set CoordinatePair -> (Int, Int) -> Set.Set CoordinatePair
    checkDirection (x, y) grid candy visited (dx, dy) =
        let step1 = (x + dx, y + dy)
            step2 = (x + 2 * dx, y + 2 * dy)
        in case addIfMatch step1 grid candy visited of
            -- first step doesn't match, stop exploration in this direction
            Nothing -> visited
            Just visited1 -> fromMaybe visited1
                (addIfMatch step2 grid candy visited1)

    -- check if a single step matches the given candy
    addIfMatch :: CoordinatePair -> GameGrid -> Candy ->
        Set.Set CoordinatePair -> Maybe (Set.Set CoordinatePair)
    addIfMatch coord grid candy visited
        | Set.member coord visited = Nothing
        | otherwise =
            case getCandyAt (board grid) coord of
                Just c | c == candy ->
                    Just (Set.insert coord visited)  -- match found
                _ -> Nothing  -- no match

applyClick :: GameGrid -> CoordinatePair -> GameGrid
applyClick g coord
    | not (validCoordinate g coord) = g
    | otherwise =
        let candy = getCandyAt (board g) coord
        in case candy of
            Nothing -> g
            Just EmptyCandy -> g
            Just c -> applyTrigger g coord (candyEffect c)

applyDisappear :: GameGrid -> [CoordinatePair] -> GameGrid
applyDisappear (GameGrid grid emptyCandyCoords) coords =
    let clearGrid = foldl clearPosition grid coords
        specialCandy = redeemSpecialCandy (Disappear coords)
    in case specialCandy of
        Just candy -> do
            let position = coords !! (length coords `div` 2)
                in GameGrid (setCandyAt clearGrid position candy)
                            emptyCandyCoords
        Nothing -> GameGrid clearGrid emptyCandyCoords

applyTrigger :: GameGrid -> CoordinatePair -> CandyEffect -> GameGrid
applyTrigger (GameGrid grid emptyCandyCoords) (x, y) StripedRow =
    GameGrid (clearRow x grid) emptyCandyCoords
applyTrigger (GameGrid grid emptyCandyCoords) (x, y) StripedCross =
    GameGrid (clearColumn y (clearRow x grid)) emptyCandyCoords
applyTrigger (GameGrid grid emptyCandyCoords) (x, y) Bomb =
    GameGrid (clearSurrounding grid (x, y)) emptyCandyCoords
applyTrigger grid _ _ = grid

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
validCoordinate (GameGrid board _) (x, y) =
    not (null board)
    && x >= 0 && x < length board
    && y >= 0 && y < length (head board)

-- | Reedem a special candy based on the number of candies in the list
redeemSpecialCandy :: Action -> Maybe Candy
redeemSpecialCandy (Disappear l)
    | len == 4 = Just stripedRowCandy
    | len == 5 = Just bombCandy
    | len >= 6 = Just stripedCrossCandy
    | otherwise = Nothing
    where len = length l
redeemSpecialCandy _ = Nothing

fillAndCrushStateIO :: GameState -> IO GameState
fillAndCrushStateIO state = do
    let board = currentGrid state
        shapes = candyShapes (difficulty state)
    filledGrid <- fillAndCrushUntilStable board shapes
    return $ state { currentGrid = filledGrid }

fillAndCrushUntilStable :: GameGrid -> [CandyShape] -> IO GameGrid
fillAndCrushUntilStable grid shapes = do
    -- Fill the grid with random candies
    newBoard <- fillBoard (board grid) shapes
    let filledGrid = GameGrid newBoard (getEmptyCandyCoords grid)
    -- Auto-crush the grid if there are crushables
    case autoCrush filledGrid of
        -- No crushables left; return the stable grid
        Nothing -> return filledGrid
        Just crushedGrid ->
            fillAndCrushUntilStable crushedGrid shapes  -- Continue until stable


-- Fill the entire board row by row
fillBoard :: [[Candy]] -> [CandyShape] -> IO [[Candy]]
fillBoard [] _ = return []
fillBoard (row:rows) shapes = do
    filledRow <- fillRow row shapes
    filledRows <- fillBoard rows shapes
    return (filledRow : filledRows)

-- Fill a single row, handling empty candies
fillRow :: [Candy] -> [CandyShape] -> IO [Candy]
fillRow [] _ = return []
fillRow (candy:rest) shapes
    | candy == EmptyCandy = do
        newCandy <- generateRandomCandy shapes
        filledRest <- fillRow rest shapes       -- Recursively fill the rest of the row
        return (newCandy : filledRest)          -- Combine the result
    | otherwise = do
        filledRest <- fillRow rest shapes       -- Recursively fill the rest of the row
        return (candy : filledRest)             -- Keep the existing candy and combine


-- Auto-crush all crushable candies except special candies
autoCrush :: GameGrid -> Maybe GameGrid
autoCrush board =
    let actions = Prelude.filter (isNormalDisappear board) (findAllCrushables board)
        newGrid = applyActions board actions
    in if null actions || newGrid == board
       then Nothing
       else Just newGrid


-- Helper function to filter out actions involving special candies
isNormalDisappear :: GameGrid -> Action -> Bool
isNormalDisappear grid (Disappear coords) =
    all (maybe True (isNormal . candyEffect) . getCandyAt (board grid)) coords
  where
    isNormal Normal = True
    isNormal _ = False
isNormalDisappear _ _ = False

-- Find all crushable candies in the board
findAllCrushables :: GameGrid -> [Action]
findAllCrushables board = go (allCoordinates board) Set.empty []
  where
    go :: [CoordinatePair] -> Set.Set CoordinatePair -> [Action] -> [Action]
    go [] _ results = results
    go (coord:coords) visited results
        | coord `Set.member` visited = go coords visited results
        | otherwise =
            let actions = findNormalCandyCrushables board coord
                newVisited = foldl (\acc (Disappear coords) -> 
                    acc `Set.union` Set.fromList coords) visited actions
            in go coords newVisited (results ++ actions)

-- Apply a list of actions to the board
applyActions :: GameGrid -> [Action] -> GameGrid
applyActions = foldl applyAction

-- List all coordinates in the board
allCoordinates :: GameGrid -> [CoordinatePair]
allCoordinates (GameGrid board _) =
    [(x, y) | x <- [0 .. length board - 1], y <- [0 .. length (head board) - 1]]

-- Function to format and print the board beautifully
printGrid :: GameGrid -> IO ()
printGrid (GameGrid board _) = do
    let dim = length board
    putStrLn $ "  " ++ unwords (map show [0..(dim - 1)])  --  print column numbers
    zipWithM_ printRow [0..] board  -- print each row with row numbers
  where
    printRow :: Int -> [Candy] -> IO ()
    printRow rowNum row = putStrLn $ show rowNum ++ " " ++ unwords (map candyToSymbol row)

printGridState :: GameState -> IO ()
printGridState(GameState { currentGrid = grid }) = printGrid grid