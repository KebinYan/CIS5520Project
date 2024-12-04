-- GameController.hs

module GameController where

import Control.Monad
import Control.Monad.State
import System.IO (hFlush, stdout)
import Data.Maybe
import Data.List
import Data.Map (Map,lookup)
import qualified Data.Set as Set
import Control.Applicative
import Control.Monad.IO.Class

import GameState
import GameUtils
import Prelude
import Phd
import CandyCrushParser

-- The main game loop
gameLoop :: Difficulty -> IO ()
gameLoop d = do
    initialState <- initializeGameState d
    state <- fillAndCrushStateIO initialState
    gameStep state

gameStep :: GameState -> IO ()
gameStep state = do
    let possibleMove = findMovable (currentGrid state)
    case possibleMove of
        Just _ -> printStateGrid state
        Nothing -> do
            -- reinitialize the grid if there are no possible moves
            putStrLn "No possible moves left, reinitializing the grid"
            newGrid <- initializeGrid (difficulty state)
            let newState = state { currentGrid = newGrid }
            filledState <- fillAndCrushStateIO newState
            gameStep filledState
    let grid = currentGrid state
        stepsRemaining = remainingSteps state
    putStrLn $ "Remaining Steps: " ++ show stepsRemaining
    putStr "Enter your action (swap x1 y1 x2 y2 / click x y / hint / undo / quit): "
    hFlush stdout

    input <- candyGetLine
    let diffculty = difficulty state
    case parseAction diffculty input of
        Right action ->
            case action of
                Quit -> putStrLn "Quitting game"
                Hint -> case possibleMove of
                    Just ((Coordinate x1, Coordinate y1), (Coordinate x2, Coordinate y2)) -> do
                        putStrLn $ "Hint: Swap (" ++ show x1 ++ "," ++ show y1
                            ++ ") with (" ++ show x2 ++ "," ++ show y2 ++ ")"
                        gameStep state
                    -- This should never happen
                    _ -> putStrLn "No possible moves available"
                _ -> do
                    updatedState <- handleAction True state action
                    stableState <- fillAndCrushStateIO updatedState
                    gameStep stableState
        Left _      -> do
            putStrLn "Invalid action, please try again."
            gameStep state

handleAction :: Bool -> GameState -> Action -> IO GameState
handleAction verbose state action = case action of
    Swap (Coordinate x1, Coordinate y1) (Coordinate x2, Coordinate y2) -> do
        when verbose $
            putStrLn $
                "Swapping (" ++ show x1 ++ "," ++ show y1 ++ ") with ("
                ++ show x2 ++ "," ++ show y2 ++ ")"
        newGrid <- applySwap (currentGrid state)
            (Coordinate x1, Coordinate y1)
            (Coordinate x2, Coordinate y2)
        execStateT (do
            updateGridState newGrid
            when verbose $ liftIO $ printGrid newGrid
            ) state
    Cheat (Coordinate x, Coordinate y) candy -> do
        when verbose $
            putStrLn $
                "Cheating at (" ++ show x ++ "," ++ show y ++ ") with candy '"
                ++ shapeName (candyDef candy) ++ "'"
        newGrid <- applyCheat (currentGrid state)
            (Coordinate x, Coordinate y) candy
        execStateT (do updateGridState newGrid) state
    Click (x, y) -> do
        when verbose $
            putStrLn $ "Clicking on (" ++ show x ++ "," ++ show y ++ ")"
        newGrid <- applyClick (currentGrid state) (x, y)
        execStateT (do
            updateGridState newGrid
            when verbose $ liftIO $ printGrid newGrid
            ) state
    Undo -> do
        when verbose $ putStrLn "Undosing last action"
        execStateT undoStep state
    _ -> do
        when verbose $ putStrLn "Invalid action"
        return state

applyCheat :: GameGrid -> CoordinatePair -> Candy -> IO GameGrid
applyCheat grid coord candy = do
    let newBoard = setCandyAt (board grid) coord candy
    let newGrid = grid { board = newBoard }
    return newGrid
            
applyAction :: GameGrid -> Action -> IO GameGrid
applyAction g (Disappear coords) = applyDisappear g coords
applyAction g (Cheat coord candy) = applyCheat g coord candy
applyAction g (Trigger (coordinate, candy)) =
    applyTrigger g coordinate candy
applyAction g (Swap (x1, y1) (x2, y2)) = applySwap g (x1, y1) (x2, y2)
applyAction g (Click (x, y)) = applyClick g (x, y)
applyAction g _ = return g

applySwap :: GameGrid -> CoordinatePair -> CoordinatePair -> IO GameGrid
applySwap g@(GameGrid board _ _)
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
swapCandies g@(GameGrid board _ _) (x1, y1) (x2, y2) =
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
            in case Set.size visited of
                n | n >= 5  ->
                    Just $ constructDisappear (Set.toList visited)
                n | n >= 3 && n < 5 && isLinear (Set.toList visited) ->
                    Just $ constructDisappear (Set.toList visited)
                _ -> Nothing
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

    -- Check if all coordinates are in the same row or column
    isLinear :: [CoordinatePair] -> Bool
    isLinear coords = isSameRow coords || isSameColumn coords

    -- Check if all coordinates are in the same row
    isSameRow :: [CoordinatePair] -> Bool
    isSameRow = all (\(Coordinate x1, _) -> x1 == x)

    -- Check if all coordinates are in the same column
    isSameColumn :: [CoordinatePair] -> Bool
    isSameColumn = all (\(_, Coordinate y1) -> y1 == y)
findNormalCandyCrushables _ _ = Nothing

applyClick :: GameGrid -> CoordinatePair -> IO GameGrid
applyClick g@(GameGrid board _ _) coord
    | not (validCoordinate board coord) = return g
    | otherwise =
        let candy = getCandyAt board coord
        in case candy of
            Just c | effectName (candyEffect c) /= "Normal" ->
                applyTrigger g coord c
            _ -> return g

applyDisappear :: GameGrid -> [CoordinatePair] -> IO GameGrid
applyDisappear g@(GameGrid board _ specialCandies) coords = do
    let clearBoard = foldl clearPosition board coords
        disappearCoords = extractDisappearCoords (Disappear coords)
    specialCandy <- redeemSpecialCandy (length disappearCoords) specialCandies
    case specialCandy of
        Just candy -> do
            let position = coords !! (length coords `div` 2)
                newBoard = setCandyAt clearBoard position candy
            return $ updateBoard newBoard g
        Nothing -> return $ updateBoard clearBoard g

extractDisappearCoords :: Action -> [CoordinatePair]
extractDisappearCoords (Disappear coords) = coords
extractDisappearCoords _ = []

applyTrigger :: GameGrid -> CoordinatePair -> Candy -> IO GameGrid
applyTrigger g@(GameGrid board _ _) coord candy = do
    let newBoard = generateSpecialEffect candy coord board
    return $ updateBoard newBoard g

fillAndCrushStateIO :: GameState -> IO GameState
fillAndCrushStateIO state = do
    let board = currentGrid state
        candies = normalCandies (currentGrid state)
    filledGrid <- fillAndCrushUntilStable board candies
    return $ state { currentGrid = filledGrid }

fillAndCrushUntilStable :: GameGrid -> [Candy] -> IO GameGrid
fillAndCrushUntilStable grid candies = do
    -- Fill the grid with random candies
    newBoard <- fillBoard (board grid) candies
    let filledGrid = updateBoard newBoard grid
    -- Auto-crush all crushable candies
    newGrid <- autoCrush filledGrid
    -- If there are still crushable candies, repeat the process
    if board newGrid == board filledGrid
    then return newGrid
    else fillAndCrushUntilStable newGrid candies

-- Auto-crush all crushable candies except special candies
autoCrush :: GameGrid -> IO GameGrid
autoCrush g = do
    let actions = findAllNormalCrushables g
    if null actions
    then return g
    else applyActions g actions

-- Find all crushable candies in the board
findAllNormalCrushables :: GameGrid -> [Action]
findAllNormalCrushables g@(GameGrid board _ _) =
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

-- Find a valid swap in the board
findMovable :: GameGrid -> Maybe (CoordinatePair, CoordinatePair)
findMovable grid = findValidSwap grid (allCoordinates (board grid)) Set.empty

findValidSwap :: GameGrid -> [CoordinatePair] ->
    Set.Set (CoordinatePair, CoordinatePair) ->
    Maybe (CoordinatePair, CoordinatePair)
findValidSwap _ [] _ = Nothing
findValidSwap grid (coord:coords) checked =
    case findAdjacentSwap grid coord checked of
        Just swapPairs -> Just swapPairs
        Nothing -> findValidSwap grid coords checked

findAdjacentSwap :: GameGrid -> CoordinatePair ->
    Set.Set (CoordinatePair, CoordinatePair) ->
    Maybe (CoordinatePair, CoordinatePair)
findAdjacentSwap grid coord checked =
    let adjacentCoords = Prelude.filter (validCoordinate (board grid)) $ adjacent coord
        unprocessedCoords =
            Prelude.filter (\adj ->
                not (Set.member (coord, adj) checked
                    || Set.member (adj, coord) checked))
                adjacentCoords
    in case find (checkSwap grid coord) unprocessedCoords of
        Just adjCoord -> Just (coord, adjCoord)
        Nothing -> Nothing
  where
    adjacent :: CoordinatePair -> [CoordinatePair]
    adjacent (Coordinate x, Coordinate y) =
        [ (Coordinate (x - 1), Coordinate y),
          (Coordinate (x + 1), Coordinate y),
          (Coordinate x, Coordinate (y - 1)),
          (Coordinate x, Coordinate (y + 1)) ]
    adjacent _ = []

checkSwap :: GameGrid -> CoordinatePair -> CoordinatePair -> Bool
checkSwap grid coord1 coord2 =
    let swappedGrid = swapCandies grid coord1 coord2
    in not (null (findCrushables swappedGrid [coord1, coord2]))
