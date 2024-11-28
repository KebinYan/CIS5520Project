-- GameController.hs

module GameController where
import Test.QuickCheck

import Control.Monad
import Control.Monad.State
import System.IO (hFlush, stdout)
import Data.Maybe
import Data.List
import Data.Map (Map)
import qualified Data.Set as Set
import Control.Applicative
import Control.Monad.IO.Class

import GameState
import Candy
import GameUtils
import Parser
import Prelude

-- Define actions
data Action = Swap CoordinatePair CoordinatePair
            | Click CoordinatePair
            | Undo
            | Quit
            | Trigger (CoordinatePair, Candy)
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
    printStateGrid state
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
                    printStateGrid stableState
                    gameStep stableState
        Left _       -> do
            putStrLn "Invalid action, please try again."
            gameStep state

parseAction :: String -> Either ParseError Action
parseAction = parse actionParser

actionParser :: Parser Action
actionParser = wsP $ choice
    [ Swap <$> (string "swap" *>
                ((,) <$> wsP coordinateP <*> wsP coordinateP)) <*>
                ((,) <$> wsP coordinateP <*> wsP coordinateP)
    , Click <$> (string "click" *> ((,) <$> wsP coordinateP <*> wsP coordinateP))
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
        newGrid <- applySwap (currentGrid state) (x1, y1) (x2, y2)
        (_, newState) <- runStateT (updateGridState newGrid) state
        when verbose $ printGrid newGrid
        return newState
    Click (x, y) -> do
        when verbose $
            putStrLn $ "Clicking on (" ++ show x ++ "," ++ show y ++ ")"
        newGrid <- applyClick (currentGrid state) (x, y)
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

applyAction :: GameGrid -> Action -> IO GameGrid
applyAction g (Disappear coords) = applyDisappear g coords
applyAction g (Trigger (coordinate, candy)) =
    applyTrigger g coordinate candy
applyAction g (Swap (x1, y1) (x2, y2)) = applySwap g (x1, y1) (x2, y2)
applyAction g (Click (x, y)) = applyClick g (x, y)
applyAction g Undo = return g
applyAction g Quit = return g

applySwap :: GameGrid -> CoordinatePair -> CoordinatePair -> IO GameGrid
applySwap g@(GameGrid board _ _ _)
          coord1@(Coordinate x1, Coordinate y1)
          coord2@(Coordinate x2, Coordinate y2)
    | not (validCoordinate board coord1) || not (validCoordinate board coord2) = return g
    | abs (x1 - x2) + abs (y1 - y2) /= 1 = return g
    | otherwise =
        let newGrid = swapCandies g coord1 coord2
            crushables = findCrushables newGrid [coord1, coord2]
        in case crushables of
            [] -> return $ swapCandies newGrid coord1 coord2
            _ -> foldM applyAction newGrid crushables
applySwap g _ _ = return g

swapCandies :: GameGrid -> CoordinatePair -> CoordinatePair -> GameGrid
swapCandies g@(GameGrid board _ _ _) (x1, y1) (x2, y2) =
    let c1 = getCandyAt board (x1, y1)
        c2 = getCandyAt board (x2, y2)
        newGrid = case (c1, c2) of
            (Just candy1, Just candy2) ->
                let newBoard = setCandyAt (setCandyAt board (x1, y1) candy2) (x2, y2) candy1
                in updateBoard newBoard g
            _ -> g
    in newGrid

findCrushables :: GameGrid -> [CoordinatePair] -> [Action]
findCrushables grid = concatMap processCoord
  where
    processCoord :: CoordinatePair -> [Action]
    processCoord coord =
        case getCandyAt (board grid) coord of
            Nothing -> []  -- No candy at the coordinate
            Just EmptyCandy -> []  -- Ignore empty candies
            Just c -> case effectName (candyEffect c) of
                "Normal" -> maybeToList (findNormalCandyCrushables grid coord)
                _ -> [Trigger (coord, c)]

-- Find all normal candies that can be crushed starting from a given coordinate
-- Returns a list of Disappear actions
findNormalCandyCrushables :: GameGrid -> CoordinatePair -> Maybe Action
findNormalCandyCrushables grid coord@(Coordinate x, Coordinate y) =
    case getCandyAt (board grid) coord of
        Nothing -> Nothing
        Just candy ->
            let visited = explore coord grid candy (Set.singleton coord)
            -- Create a Disappear action if at least 3 candies match
            in if Set.size visited >= 3
               then Just (constructDisappear (Set.toList visited))
               else Nothing
  where
    -- Four cardinal directions: up, down, left, right
    directions :: [(Int, Int)]
    directions = [(1, 0), (-1, 0), (0, 1), (0, -1)]

    -- Explore at most two steps in each direction
    explore :: CoordinatePair -> GameGrid -> Candy ->
        Set.Set CoordinatePair -> Set.Set CoordinatePair
    explore start grid candy visited =
        foldl (checkDirection start grid candy) visited directions

    -- Check a single direction for matching candies
    checkDirection :: CoordinatePair -> GameGrid -> Candy ->
        Set.Set CoordinatePair -> (Int, Int) -> Set.Set CoordinatePair
    checkDirection (Coordinate x, Coordinate y) grid candy visited (dx, dy) =
        let step1 = (Coordinate (x + dx), Coordinate (y + dy))
            step2 = (Coordinate (x + 2 * dx), Coordinate (y + 2 * dy))
        in case addIfMatch step1 grid candy visited of
            -- First step doesn't match, stop exploration in this direction
            Nothing -> visited
            Just visited1 -> fromMaybe visited1
                (addIfMatch step2 grid candy visited1)
    checkDirection _ _ _ visited _ = visited

    -- Check if a single step matches the given candy
    addIfMatch :: CoordinatePair -> GameGrid -> Candy ->
        Set.Set CoordinatePair -> Maybe (Set.Set CoordinatePair)
    addIfMatch coord grid candy visited
        | Set.member coord visited = Nothing
        | otherwise =
            case getCandyAt (board grid) coord of
                Just c | c == candy ->
                    Just (Set.insert coord visited)
                _ -> Nothing
findNormalCandyCrushables _ _ = Nothing

applyClick :: GameGrid -> CoordinatePair -> IO GameGrid
applyClick g@(GameGrid board _ _ _) coord
    | not (validCoordinate board coord) = return g
    | otherwise =
        let candy = getCandyAt board coord
        in case candy of
            Nothing -> return g
            Just EmptyCandy -> return g
            Just c -> applyTrigger g coord c

applyDisappear :: GameGrid -> [CoordinatePair] -> IO GameGrid
applyDisappear g@(GameGrid board _ specialCandies _) coords = do
    let clearBoard = foldl clearPosition board coords
        disappearCoords = extractDisappearCoords (Disappear coords)
    specialCandy <- redeemSpecialCandy disappearCoords specialCandies
    case specialCandy of
        Just candy -> do
            let position = coords !! (length coords `div` 2)
                newBoard = setCandyAt clearBoard position candy
            return $ updateBoard newBoard g
        Nothing -> return g

extractDisappearCoords :: Action -> [CoordinatePair]
extractDisappearCoords (Disappear coords) = coords
extractDisappearCoords _ = []

applyTrigger :: GameGrid -> CoordinatePair -> Candy -> IO GameGrid
applyTrigger g@(GameGrid board _ _ _) coord candy = do
    let newBoard = generateSpecialEffect candy coord board
    return $ updateBoard newBoard g

fillAndCrushStateIO :: GameState -> IO GameState
fillAndCrushStateIO state = do
    let board = currentGrid state
        candies = normalCandies (currentGrid state)
    filledGrid <- fillAndCrushUntilStable (return board) candies
    return $ state { currentGrid = filledGrid }

fillAndCrushUntilStable :: IO GameGrid -> [Candy] -> IO GameGrid
fillAndCrushUntilStable grid candies = do
    -- Fill the grid with random candies
    newBoard <- fillBoard (board <$> grid) candies
    filledGrid <- updateBoard newBoard <$> grid
    -- Auto-crush all crushable candies
    newGrid <- autoCrush (return filledGrid)
    -- If there are still crushable candies, repeat the process
    if board newGrid == board filledGrid
    then return newGrid
    else fillAndCrushUntilStable (return newGrid) candies

-- Auto-crush all crushable candies except special candies
autoCrush :: IO GameGrid -> IO GameGrid
autoCrush ioGrid = do
    g@(GameGrid board _ _ _) <- ioGrid
    let actions = findAllNormalCrushables g
        newGrid = applyActions g actions
    if null actions
    then return g
    else newGrid

-- Helper function to filter out actions involving special candies
-- isNormalDisappear :: [[Candy]] -> Action -> Bool
-- isNormalDisappear grid (Disappear coords) =
--     all (maybe True (isNormal . effectName candyEffect) . getCandyAt (board grid)) coords
--   where
--     isNormal "Normal" = True
--     isNormal _ = False
-- isNormalDisappear _ _ = False

-- Find all crushable candies in the board
findAllNormalCrushables :: GameGrid -> [Action]
findAllNormalCrushables g@(GameGrid board _ _ _) =
    processCoords (allCoordinates board) Set.empty []
    where
        processCoords :: [CoordinatePair] -> Set.Set CoordinatePair -> [Action] -> [Action]
        processCoords [] _ res = res
        processCoords (coord:coords) visited res
            | Set.member coord visited = processCoords coords visited res
            | otherwise =
                case findNormalCandyCrushables g coord of
                    Just action ->
                        let newVisited = addDisappearCoords visited action
                        in processCoords coords newVisited (action : res)
                    Nothing -> processCoords coords visited res

        addDisappearCoords :: Set.Set CoordinatePair -> Action -> Set.Set CoordinatePair
        addDisappearCoords visited (Disappear coords) =
            visited `Set.union` Set.fromList coords
        addDisappearCoords visited _ = visited

-- Apply a list of actions to the board
applyActions :: GameGrid -> [Action] -> IO GameGrid
applyActions = foldM applyAction