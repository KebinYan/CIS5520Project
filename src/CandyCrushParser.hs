-- This is the parser specified for parsing data related to the Candy Crush game
module CandyCrushParser where
import Phd
import GeneralStateParser as GSP

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List (find, isPrefixOf)
import Text.Read (readMaybe)
import Test.QuickCheck qualified as QC
import Prelude hiding (filter)
import Data.Set (Set)
import Data.Map (Map, lookup)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Exception (IOException, try)
import System.IO

-- ---------------------------------------------------------------
--                    Candy Crush Parser Implementations
-- ---------------------------------------------------------------

-- | Get the current GameConst from the parser state
getGameConst :: Parser GameConst
getGameConst = GSP.SP $ \state -> Right (pGameConst state, state)

-- | Update the GameConst state with a modification function
updateGameConst :: (GameConst -> GameConst) -> Parser ()
updateGameConst f = GSP.SP $ \state -> 
  Right ((), state { pGameConst = f (pGameConst state) })
{------------------------ Effect Parsing ------------------------}
-- | Parse a coordinate, expecting a colon or an integer
coordinateP :: Parser Coordinate
coordinateP = constP ":" All <|> Coordinate <$> intP

-- | Parse a pair of coordinates within parentheses
coordinatePairP :: Parser CoordinatePair
coordinatePairP = 
  parens $ (,) <$> wsP coordinateP <* wsP (char ',') <*> wsP coordinateP

-- | Parse a list of coordinate pairs within brackets
coordinateListP :: Parser [CoordinatePair]
coordinateListP = brackets $ coordinatePairP `sepBy` wsP (char ',')

-- | Parse a Circle effect range
circleP :: Parser EffectRange
circleP = upgradeToFatalIfFirstSucceeds "Error while parsing Circle"
  (constIgnoreCaseP "Circle" ())
  (Circle <$> wsP intP)

-- | Parse a Rectangle effect range
rectangleP :: Parser EffectRange
rectangleP = upgradeToFatalIfFirstSucceeds "Error while parsing Rectangle"
  (constIgnoreCaseP "Rectangle" ())
  (Rectangle <$> wsP intP <*> wsP intP)

-- | Parse a Diamond effect range
diamondP :: Parser EffectRange
diamondP = upgradeToFatalIfFirstSucceeds "Error while parsing Diamond"
  (constIgnoreCaseP "Diamond" ())
  (Diamond <$> wsP intP)

-- | Parse an Arbitrary effect range with coordinate list
arbitraryP :: Parser EffectRange
arbitraryP = upgradeToFatalIfFirstSucceeds "Error while parsing Arbitrary"
  (constIgnoreCaseP "Arbitrary" ())
  (Arbitrary <$> wsP coordinateListP)

-- | Parse an effect range, trying all possible types
effectRangeP :: Parser EffectRange
effectRangeP = circleP
  <|> rectangleP
  <|> diamondP
  <|> arbitraryP
  <|> fatalError "Unrecognized effect range type"

-- | Parse an effect name line
effectNameP :: Parser String
effectNameP =
  constIgnoreCaseP "effect_name:" ()
  *> stripP (some (satisfy (/= '\n')))
  <* newline
  <|> fatalErrorWithExpectStr "effectNameP()" "effect_name: <name>" 16

-- | Parse the effect range line
effectRangeLineP :: Parser EffectRange
effectRangeLineP = 
  stringIgnoreCase "effect_range:" *> wsP effectRangeP <* newline

-- | Parse a comparison operator
operatorP :: Parser Operator
operatorP = foldr ((<|>) . makeOpParser) empty operatorMapping
  where
    makeOpParser (str, op) = constP str op

-- | Parse an effect requirement line
effectRequirementP :: Parser EffectRequirement
effectRequirementP = do
  constIgnoreCaseP "effect_requirement:" ()
  EffectRequirement <$> (wsP operatorP >>= checkOperator)
                   <*> (wsP intP >>= checkPositive)
                   <* newline
  <|> fatalErrorWithExpectStr "effectRequirementP()"
    "effect_requirement: [>, >=, =] <number>" 23
  where
    -- Check if the operator is allowed
    checkOperator op
      | op `elem` allowedEffectReqOperators = return op
      | otherwise = fatalError $ "operator " ++ show op ++ " is not allowed"
    -- Check if the number is positive
    checkPositive n
      | n < 0     = fatalErrorWithExpectStr "checkPositive()"
        "a positive number" 3
      | otherwise = return n

-- | Parse the effect description line
effectDescriptionP :: Parser String
effectDescriptionP = do
  constIgnoreCaseP "effect_description:" ()
    <|> fatalErrorWithExpectStr "effectDescriptionP()"
      "`effect_description:`" 23
  stripP (many (satisfy (/= '\n')))
  <* newline
  <|> fatalErrorWithExpectStr "effectDescriptionP()"
    "`effect_description: <some description>`" 23

-- | Parse an effect block
effectP :: Parser ()
effectP = do
  name <- updateFailToFatal "Error parsing effect name" effectNameP
  range <- updateFailToFatal "Error parsing effect range" effectRangeLineP
  requirement <- updateFailToFatal "Error parsing effect requirement"
    effectRequirementP
  description <- updateFailToFatal "Error parsing effect description"
    effectDescriptionP
  let effect = Effect name range requirement description
  updateGameConst (\d -> d { effectMap = Map.insert name effect
    (effectMap d) })

-- | Parse multiple effect blocks
effectsP :: Parser ()
effectsP = void $ some $ skipCommentOrEmptyLines
  -- If a block starts with "effect_", it is an effect block
  *> expectString "effect_" 
  *> effectP
  <* skipCommentOrEmptyLines

{------------------------ Constant Parsing ------------------------}
-- | Parse a game constant block
gameConstantP :: Parser ()
gameConstantP = do
  -- Check if the block is a game constant block
  skipCommentOrEmptyLines
  constIgnoreCaseP "game_constant" () <* newline

  -- Parse dimension
  constP "dimension:" ()
    <|> fatalErrorWithExpectStr "gameConstantP()" "`dimension:`" 13
  dimensionVal <- (wsP intP <* newline)
    <|> fatalError "Error parsing dimension integer"
  when (dimensionVal < 3) $
    fatalError "dimension must be >= 3"
    
  skipCommentOrEmptyLines

  -- Parse max_steps
  constP "max_steps:" ()
    <|> fatalErrorWithExpectStr "gameConstantP()" "`max_steps:`" 14
  maxStepsVal <- (wsP intP <* newline)
    <|> fatalError "Error parsing max_steps integer"
  when (maxStepsVal < 3) $
    fatalError "max_steps must be >= 3"

  skipCommentOrEmptyLines

  -- Parse score_per_candy
  constP "score_per_candy:" ()
    <|> fatalErrorWithExpectStr "gameConstantP()" "`score_per_candy:`" 19
  scorePerCandyVal <- (wsP intP <* newline)
    <|> fatalError "Error parsing score_per_candy integer"
  when (scorePerCandyVal < 0) $
    fatalError "score_per_candy must be >= 0"

  skipCommentOrEmptyLines

  -- Parse score_per_effect
  constP "score_per_effect:" ()
    <|> fatalErrorWithExpectStr "gameConstantP()" "`score_per_effect:`" 20
  scorePerEffectVal <- (wsP intP <* newline)
    <|> fatalError "Error parsing score_per_effect integer"
  when (scorePerEffectVal < 0) $
    fatalError "score_per_effect must be >= 0"

  updateGameConst (\d -> d { 
    dimension = dimensionVal, 
    maxSteps = maxStepsVal, 
    scorePerCandy = scorePerCandyVal, 
    scorePerEffect = scorePerEffectVal })

{------------------------ Candy Parsing ------------------------}
-- | Parse the shape_name line
shapeNameP :: Parser String
shapeNameP = do
  constIgnoreCaseP "shape_name:" ()
    <|> fatalError "expected `shape_name:`, but got something else."
  name <- stripP (some (satisfy (/= '\n')))
    <|> fatalError "shape_name cannot be empty"
  newline <|> fatalError "missing newline after shape_name"
  return name

-- | Parse the shape_icon line
shapeIconP :: Parser String
shapeIconP = do
  constIgnoreCaseP "shape_icon:" ()
    <|> fatalErrorWithExpectStr "shapeIconP()"
      "`shape_icon: <icon>`" 13
  icon <- stripP (some (satisfy (/= '\n')))
    <|> fatalError "shape_icon cannot be empty"
  newline <|> fatalError "Missing newline after shape_icon"
  return icon

-- | Parse the effect_name line within a candy block
effectNameRefP :: Parser String
effectNameRefP = do
  constIgnoreCaseP "effect_name:" ()
    <|> fatalErrorWithExpectStr "effectNameRefP()"
      "`effect_name: <name>`" 14
  effectName <- stripP (some (satisfy (/= '\n')))
    <|> fatalError "effect name cannot be empty"
  newline <|> fatalError "Missing newline after effect_name"
  return effectName

-- | Retrieve the Effect from the effectMap based on name
effectNameToEffect :: String -> Parser Effect
effectNameToEffect name = do
  gameConstVal <- getGameConst
  case Map.lookup name (effectMap gameConstVal) of
    Just effect -> return effect
    Nothing -> fatalError $ "Effect `" ++ name ++ "` not found in the effect map"

-- | Parse a candy block
candyP :: Parser ()
candyP = do
  skipCommentOrEmptyLines
  name <- shapeNameP
  icon <- shapeIconP
  effectName <- effectNameRefP
  effect <- effectNameToEffect effectName
  let candyDef = CandyDefinition name icon effectName
  updateGameConst (\d -> d { candyMap = Map.insert name
    (Candy candyDef effect) (candyMap d) })

-- | Parse multiple candy blocks
candiesP :: Parser ()
candiesP = void $ some $ skipCommentOrEmptyLines
  -- If a block starts with "shape_", it is a candy block
  *> expectString "shape_"  
  *> candyP
  <* skipCommentOrEmptyLines

-- | Main parsing loop that continues until EOF
parseLoop :: Parser ()
parseLoop = do
  skipCommentOrEmptyLines
  notEOF <- (eof *> pure False) <|> pure True
  when notEOF $ do
    effectsP
      <|> candiesP
      <|> gameConstantP
      <|> failErrorWithExpectStr "parseLoop():"
        "one of [`effect_`, `shape_`, `game_constant`]" 10
    parseLoop

{------------------------ Action Parsing ------------------------}
actionIntP :: Int -> String -> Parser Int
actionIntP dim errorMessage = do
    coord <- wsP intP
    if coord < 0 || coord >= dim
        then failError errorMessage
        else return coord

parseAction :: GameConst -> String -> Either ParseError Action
parseAction gameConst input = GSP.doParse (actionParser gameConst) 
                              (CandyFileParser input 1 defaultGameConst) >>=
                                 \(action, _) -> Right action

-- Modify actionParser to include gameConst for parsing Cheat
actionParser :: GameConst -> Parser Action
actionParser gameConst = wsP $
    parseSwap  (dimension gameConst) <|>
    parseClick (dimension gameConst)<|>
    parseCheat gameConst <|>
    parseConstantAction "undo" Undo <|>
    parseConstantAction "quit" Quit <|>
    parseConstantAction "hint" Hint

-- parse "swap" action
parseSwap :: Int -> Parser Action
parseSwap dim = do
    stringP "swap"
    x1 <- actionIntP dim "x1 must be within the grid and non-negative"
    y1 <- actionIntP dim "y1 must be within the grid and non-negative"
    x2 <- actionIntP dim "x2 must be within the grid and non-negative"
    y2 <- actionIntP dim "y2 must be within the grid and non-negative"
    return $ Swap (Coordinate x1, Coordinate y1) (Coordinate x2, Coordinate y2)

-- parse "click" action
parseClick :: Int -> Parser Action
parseClick dim = do
    stringP "click"
    x <- actionIntP dim "x must be within the grid and non-negative"
    y <- actionIntP dim "y must be within the grid and non-negative"
    return $ Click (Coordinate x, Coordinate y)

-- parse constant actions
parseConstantAction :: String -> Action -> Parser Action
parseConstantAction keyword action = stringP keyword *> pure action

-- parse "cheat" action
parseCheat :: GameConst -> Parser Action
parseCheat gameConst = do
    stringP "cheat"
    let dim = dimension gameConst
    x <- actionIntP dim "x must be within the grid and non-negative"
    y <- actionIntP dim "y must be within the grid and non-negative"
    candyName <- stripP (some (satisfy (/= '\n')))
    case Data.Map.lookup candyName (candyMap gameConst) of
        Just candy -> return $ Cheat (Coordinate x, Coordinate y) candy
        Nothing -> fatalError $ "Candy '" ++ candyName ++ "' does not exist."

{---------------------------- Input Parse ------------------------------}
-- Custom input function supporting backspace
candyGetLine :: IO String
candyGetLine = do
    hSetEcho stdin False        -- Disable echo
    hSetBuffering stdin NoBuffering -- Read input character by character
    inputLoop ""
  where
    inputLoop :: String -> IO String
    inputLoop acc = do
        char <- getChar
        case char of
            '\n' -> do                 -- Handle Enter key
                putChar '\n'
                return acc
            '\DEL' -> do               -- Handle Backspace key
                if null acc
                    then inputLoop acc -- Ignore if no characters to delete
                    else do
                        putStr "\b \b" -- Erase character on terminal
                        inputLoop (init acc) -- Remove last character from input
            _ | isPrint char -> do     -- Handle printable characters
                    putChar char       -- Display the character
                    inputLoop (acc ++ [char]) -- Add character to input
              | otherwise -> inputLoop acc -- Ignore other control characters
{---------------------------- File Parse ------------------------------}
-- | Parse the entire input file into a gameConst object
fileP :: String -> Either ParseError GameConst
fileP input = case doParse (parseLoop <* eof)
  (CandyFileParser input 1 defaultGameConst) of
    Right (_, state) -> Right (pGameConst state)  -- Return the parsed gameConst
    Left err -> Left err                           -- Return the parse error

-- | Take a filename and return the parsed GameConst object
parseFile :: String -> IO (Either ParseError GameConst)
parseFile filename = do
  contentResult <- try (readFile filename) :: IO (Either IOException String)
  return $ either
    (Left . (\e -> 
      FatalError ("Error reading `" ++ filename ++ "`: " ++ show e) 0))
    parseAndValidate contentResult

-- | Parse and validate the content of a file
parseAndValidate :: String -> Either ParseError GameConst
parseAndValidate content =
  case fileP content of
    Left err -> Left err
    Right gameConst -> case validateGameConst gameConst of
      Left errMsg -> Left (FatalError errMsg 0)
      Right validGameConst -> Right validGameConst

validateGameConst :: GameConst -> Either String GameConst
validateGameConst (GameConst dimension candyMap _ maxSteps 
  scorePerCandy scorePerEffect)
  | dimension <= 0 = Left "Dimension must be greater than 0"
  | maxSteps < 3 = Left "maxSteps must be at least 3"
  | Map.size candyMap < 3 = Left "candyMap must contain at least 3 candies"
  | otherwise = Right (GameConst dimension candyMap Map.empty maxSteps 
    scorePerCandy scorePerEffect)