module GameUtils where

import Phd
import GeneralStateParser
import System.Random
import System.IO
import System.Directory (doesFileExist)
import Control.Monad (replicateM, when)
import Control.Exception (evaluate)
import qualified Data.List as List
import Data.Map (Map, insertWith, empty, lookup)
import qualified Data.Set as Set
-- Generate a random Candy based on a provided list of CandyShape
-- | candyShapes: the list of available CandyShape
generateRandomCandy :: [Candy] -> IO Candy
generateRandomCandy [] = error "No candies available"
generateRandomCandy candies = do
    randomIndex <- randomRIO (0, length candies - 1)
    return (candies !! randomIndex)

-- Generate a list of random candies based on a provided list of CandyShape
-- | len: the number of candies to generate
-- | candyShapes: the list of available CandyShape
generateRandomCandyList :: Int -> [Candy] -> IO [Candy]
generateRandomCandyList len candies =
    replicateM len (generateRandomCandy candies)

-- Generate the special effect for a special candy based on the special effect 
-- range
generateSpecialEffect :: Candy -> (CoordinatePair -> [[Candy]] -> [[Candy]])
generateSpecialEffect candy coord board =
    case coord of
        (Coordinate x, Coordinate y) ->
            let positionsToClear = case effectRange (candyEffect candy) of
                    Circle r ->
                        computeCirclePositions (x, y) r
                    Rectangle w h->
                        computeRectanglePositions (x, y) w h
                    Diamond r ->
                        computeDiamondPositions (x, y) r
                    Arbitrary coords ->
                        computeArbitraryPositions
                            (length board, length (head board)) (x, y) coords
            in foldl clearPosition board (coord : positionsToClear)
        _ -> error ("Invalid candy coordinate: " ++ show coord)

-- Compute the positions to clear for a circle effect
computeCirclePositions :: (Int, Int) -> Int -> [CoordinatePair]
computeCirclePositions (x, y) r =
    [ (Coordinate (x + dx), Coordinate (y + dy)) |
        dx <- [-r..r], dy <- [-r..r], dx * dx + dy * dy <= r * r]

-- Compute the positions to clear for a rectangle effect
computeRectanglePositions :: (Int, Int) -> Int -> Int -> [CoordinatePair]
computeRectanglePositions (x, y) w h =
    let halfW = w `div` 2
        halfH = h `div` 2
        widthRange = if even w then [-halfW..halfW - 1] else [-halfW..halfW]
        heightRange = if even h then [-halfH..halfH - 1] else [-halfH..halfH]
    in [ (Coordinate (x + dx), Coordinate (y + dy)) |
        dx <- widthRange, dy <- heightRange]

-- Compute the positions to clear for a diamond effect
computeDiamondPositions :: (Int, Int) -> Int -> [CoordinatePair]
computeDiamondPositions (x, y) r =
    [ (Coordinate (x + dx), Coordinate (y + dy)) |
        dx <- [-r..r], dy <- [-r..r], abs dx + abs dy <= r]

-- Compute the positions to clear for a arbitrary effect
-- Parameters:
-- | (nRows, nCols): the dimension of the board
-- | (x, y): the coordinate of the special candy
-- | coords: the list of coordinates to clear
computeArbitraryPositions :: (Int, Int) -> (Int, Int) ->
    [CoordinatePair] -> [CoordinatePair]
computeArbitraryPositions (nRows, nCols) (x, y) =
    Set.toList . foldl
        (\acc coord -> expandCoords (nRows, nCols) (x, y) coord `Set.union` acc)
                                    Set.empty
    where
        expandCoords :: (Int, Int) -> (Int, Int) -> CoordinatePair ->
            Set.Set CoordinatePair
        expandCoords (nRows, nCols) (x, y) (Coordinate dx, Coordinate dy) =
            Set.singleton (Coordinate (x + dx), Coordinate (y + dy))
        expandCoords (nRows, nCols) (x, y) (All, Coordinate dy) =
            Set.fromList
                [(Coordinate row , Coordinate (y + dy)) | row <- [0..nCols - 1]]
        expandCoords (nRows, nCols) (x, y) (Coordinate dx, All) =
            Set.fromList
                [(Coordinate (x + dx), Coordinate col) | col <- [0..nRows - 1]]
        expandCoords (nRows, nCols) (x, y) (All, All) =
            Set.fromList [(Coordinate row, Coordinate col)
                | row <- [0..nCols - 1], col <- [0..nRows - 1]]

-- Clear a candy at a given coordinate
clearPosition :: [[Candy]] -> CoordinatePair -> [[Candy]]
clearPosition board coord@(Coordinate x, Coordinate y)
    | not (validCoordinate board coord) = board
    | otherwise =
        case getCandyAt board coord of
            Nothing -> board
            Just EmptyCandy -> board
            Just candy ->
                let newBoard = setCandyAt board coord EmptyCandy
                in case effectName (candyEffect candy) of
                    "Normal" -> newBoard
                    _ -> generateSpecialEffect candy coord newBoard
clearPosition board _ = board

-- Check if a coordinate is valid
validCoordinate :: [[Candy]] -> CoordinatePair -> Bool
validCoordinate board (Coordinate x, Coordinate y) =
    not (null board)
    && x >= 0 && x < length board
    && y >= 0 && y < length (head board)
validCoordinate _ (_, _) = False

-- List all coordinates in the board
allCoordinates :: [[Candy]] -> [CoordinatePair]
allCoordinates board =
    [ (Coordinate x, Coordinate y)
        | x <- [0..length board - 1], y <- [0..length (head board) - 1]]

-- get candy at a specific position
getCandyAt :: [[Candy]] -> CoordinatePair -> Maybe Candy
getCandyAt board (Coordinate x, Coordinate y) =
    if validCoordinate board (Coordinate x, Coordinate y)
    then Just (board !! x !! y)
    else Nothing
getCandyAt _ (_, _) = Nothing

-- set candy at a specific position
setCandyAt :: [[Candy]] -> CoordinatePair -> Candy -> [[Candy]]
setCandyAt board (Coordinate x, Coordinate y) newCandy =
    if validCoordinate board (Coordinate x, Coordinate y)
    then
        let (before, row:after) = splitAt x board
            (left, _:right) = splitAt y row
            newRow = left ++ [newCandy] ++ right
        in before ++ [newRow] ++ after
    else board
setCandyAt board _ _ = board

-- Extract normal candies from a candies list
extractNormalCandies :: [Candy] -> [Candy]
extractNormalCandies = Prelude.filter (\candy ->
    effectName (candyEffect candy) == "Normal")

-- Extract special candies from a candies list
extractSpecialCandies :: Int -> [Candy] -> Map Int [Candy]
extractSpecialCandies dim = foldr extractSpecialCandy Data.Map.empty
    where
        extractSpecialCandy candy acc =
            let req = effectRequirement (candyEffect candy)
            in case req of
                EffectRequirement Eq 0 -> acc
                EffectRequirement Eq n ->
                    if n < dim * dim
                    then insertWith (++) n [candy] acc
                    else acc
                EffectRequirement Ge n ->
                    if n < dim * dim
                    then foldr (\i -> insertWith (++) i [candy]) acc
                                [n.. dim * dim]
                    else acc
                EffectRequirement Gt n ->
                    if n < dim * dim
                    then foldr (\i -> insertWith (++) i [candy]) acc
                                [n + 1.. dim * dim]
                    else acc
                _ -> acc

-- | Reedem a special candy based on the number of candies in the list
redeemSpecialCandy :: Int -> Map Int [Candy] -> IO (Maybe Candy)
redeemSpecialCandy n specialCandies = do
    if n == 0
        then return Nothing
        else case Data.Map.lookup n specialCandies of
            Just candies | not (null candies) ->
                generateRandomCandy candies >>= return . Just
            _ -> return Nothing

candyToSymbol :: Candy -> String
candyToSymbol EmptyCandy = " "
candyToSymbol candy =
    let color = case effectName (candyEffect candy) of
            "Normal" -> "\ESC[30m"
            _ -> "\ESC[91m"
    in color ++ shapeIcon (candyDef candy) ++ "\ESC[0m"

-- Fill the entire board row by row
fillBoard :: [[Candy]] -> [Candy] -> IO [[Candy]]
fillBoard rows candies
    | null candies = error "No candies available"
    | otherwise = mapM (`fillRow` candies) rows

-- Fill a single row, handling empty candies
fillRow :: [Candy] -> [Candy] -> IO [Candy]
fillRow row candies
    | null candies = error "No candies available"
    | otherwise = mapM (\candy ->
        if candy == EmptyCandy
        then generateRandomCandy candies
        else return candy) row

-- Persist the current game info
printGameInfo :: Bool -> GameState -> IO ()
printGameInfo verbose gameState = do
    let gameConstants = gameConst gameState
        dim = dimension gameConstants
        steps = maxSteps gameConstants
        scoreVal = score gameState
        block = formatBlock dim steps scoreVal
        fileName = "score.txt"
    fileExists <- doesFileExist fileName
    if not fileExists
    then do
        -- Create and write the new block
        writeFile fileName block
        when verbose $ putStrLn "Created score.txt and wrote the data."
    else do
        -- Read the contents of the file
        -- Use withFile to ensure the file is closed after reading
        contents <- withFile fileName ReadMode (\handle -> do
            contents <- hGetContents handle
            evaluate (length contents) -- Force load entire content into memory
            return contents)
        let blocks = splitBlocks contents
            matchingBlock = findBlock blocks dim steps
            updatedBlocks =
                case matchingBlock of
                    Just _  -> map (\b ->
                        if isMatchingBlock b dim steps
                        then lines $ updateScoreBlock dim steps scoreVal b
                        else lines b) blocks
                    Nothing -> blocks : [lines block]
        -- Write back the updated contents
        writeFile fileName (unlines $ List.intercalate [""] updatedBlocks)
        when verbose $ putStrLn "Updated score.txt with the data."

formatBlock :: Int -> Int -> Int -> String
formatBlock dim steps score =
    "dimension: " ++ show dim ++ "\n" ++
    "maxSteps: " ++ show steps ++ "\n" ++
    "score: " ++ show score

isMatchingBlock :: String -> Int -> Int -> Bool
isMatchingBlock block targetDim targetSteps =
    case parseBlock block of
        Just (dim, steps) -> dim == targetDim && steps == targetSteps
        Nothing -> False

parseBlock :: String -> Maybe (Int, Int)
parseBlock block = do
    let linesOfBlock = lines block
        dimLine = List.find ("dimension:" `List.isPrefixOf`) linesOfBlock
        stepsLine = List.find ("maxSteps:" `List.isPrefixOf`) linesOfBlock
    case (dimLine, stepsLine) of
        (Just d, Just s) -> do
            let dim = read (drop (length "dimension: ") d) :: Int
            let steps = read (drop (length "maxSteps: ") s) :: Int
            Just (dim, steps)
        _ -> Nothing

splitBlocks :: String -> [String]
splitBlocks = Prelude.filter (not . null) . splitOn "\n\n"

splitOn :: String -> String -> [String]
splitOn delim str = go str []
  where
    go [] current = [reverse current]
    go s current
      | delim `List.isPrefixOf` s =
        reverse current : go (drop (length delim) s) []
      | otherwise = case s of
          (c:cs) -> go cs (c : current)

findBlock :: [String] -> Int -> Int -> Maybe String
findBlock blocks dim steps =
    List.find (\block -> ("dimension: " ++ show dim) `List.isInfixOf` block &&
                    ("maxSteps: " ++ show steps) `List.isInfixOf` block) blocks

updateScoreBlock :: Int -> Int -> Int -> String -> String
updateScoreBlock dim steps newScore block
    | ("dimension: " ++ show dim) `List.isInfixOf` block &&
      ("maxSteps: " ++ show steps) `List.isInfixOf` block =
        unlines $ map (updateScoreLine newScore) (lines block)
    | otherwise = block

updateScoreLine :: Int -> String -> String
updateScoreLine newScore line
    | "score:" `List.isPrefixOf` line = "score: " ++ show newScore
    | otherwise = line
