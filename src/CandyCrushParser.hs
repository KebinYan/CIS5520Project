module CandyCrushParser where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List (find, isPrefixOf)
import Text.Read (readMaybe)
import Test.QuickCheck qualified as QC
import Prelude hiding (filter)
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Prelude as P
import Control.Exception (IOException, try)
import System.IO
import Phd

-- | Throw a fatal error with a message
fatalError :: String -> Parser a
fatalError msg = P $ \state ->
  Left (FatalError (msg ++ "\n" ++ show state) (lineNum state))

-- | Throw a fatal error with expected string information
fatalErrorWithExpectStr :: String -> String -> Int -> Parser a
fatalErrorWithExpectStr msg expectStr len = P $ \state ->
  Left (FatalError (msg ++ " expected: `" ++ expectStr ++ "`, but got `"
    ++ peekStr len state ++ "`\n" ++ show state) (lineNum state))

-- | Throw a recoverable failure error with a message
failError :: String -> Parser a
failError msg = P $ \state -> Left (FailError msg (lineNum state))

-- | Throw a recoverable failure error with expected string information
failErrorWithExpectStr :: String -> String -> Int -> Parser a
failErrorWithExpectStr msg expectStr len = P $ \state ->
  Left (FailError (msg ++ ", expected: `" ++ expectStr
    ++ "`, but got `" ++ peekStr len state ++ "`") (lineNum state))

-- | Upgrade a FailError to a FatalError with a custom message
updateFailToFatal :: String -> Parser a -> Parser a
updateFailToFatal errorMsg parser = P $ \state -> case doParse parser state of
  Left (FailError msg _) -> Left (FatalError (errorMsg ++ ": " ++ msg)
    (lineNum state))  -- Upgrade to FatalError
  result -> result          -- Return other results

-- | Combine two parsers: upgrade to FatalError if the first succeeds
upgradeToFatalIfFirstSucceeds :: String -> Parser () -> Parser a -> Parser a
upgradeToFatalIfFirstSucceeds errorMsg conditionParser mainParser = do
  _ <- conditionParser
  P $ \state -> case doParse mainParser state of
    Left (FailError msg _) -> Left (FatalError (errorMsg ++ ": " ++ msg)
      (lineNum state))  -- Upgrade to FatalError
    result -> result          -- Return other results

-- | Strip whitespace from both ends of a parser's result
wsP :: Parser a -> Parser a
wsP p = many space *> p <* many space

-- | Strip leading and trailing whitespace from a string
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Strip whitespace and ensure the result is not empty
stripP :: Parser String -> Parser String
stripP p = do
  result <- p
  let stripped = strip result
  if null stripped
    then failError "empty string after stripping"
    else return stripped

-- | Get the next character from the input
get :: Parser Char
get = P $ \s -> case input s of
  (c:cs) ->
    let newState = s { input = cs
                     , lineNum = lineNum s + if c == '\n' then 1 else 0 }
    in Right (c, newState)
  [] ->
    Left (FailError ("Unexpected EOF at line " ++ show (lineNum s))
      (lineNum s))

-- | Succeed only at the end of the input
eof :: Parser ()
eof = P $ \s -> case input s of
  [] -> Right ((), s)  -- Success if no input remains
  _  -> Left (FailError ("Expected EOF at line "
    ++ show (lineNum s) ++ " but got `" ++ peekStr 5 s ++ "`...")
    (lineNum s))

-- | Filter parsing results based on a predicate
filter :: Show a => (a -> Bool) -> Parser a -> Parser a
filter f p = P $ \s -> case doParse p s of
  Left err -> Left err  -- Error already includes line number
  Right (c, s') ->
    if f c
      then Right (c, s')
      else Left (FailError ("Value " ++ show c
        ++ " does not satisfy the predicate at line "
        ++ show (lineNum s')) (lineNum s'))

-- | Return the next character if it satisfies the predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = filter p get

isSpaceOrTab :: Char -> Bool
isSpaceOrTab = (`elem` " \t\r")

-- | Parsers for specific types of characters
alpha, digit, upper, lower, space :: Parser Char
alpha = satisfy isAlpha
digit = satisfy isDigit
upper = satisfy isUpper
lower = satisfy isLower
space = satisfy isSpaceOrTab

-- | Return the next character if it matches the given character
char :: Char -> Parser Char
char c = satisfy (== c)

-- | Delimiters used in parsing
delims :: Set Char
delims = Set.fromList ['(', ')', '[', ']', '{', '}', ' ', ',']

-- | Check if a character is a delimiter
isDelim :: Char -> Bool
isDelim c = Set.member c delims

-- | Peek the next n characters without consuming them
peek :: Int -> Parser String
peek n = P $ \s ->
  if null (input s)
    then Right ("", s)
    else Right (take n (input s), s)

-- | Peek the next n characters from the current parse state
peekStr :: Int -> ParseState -> String
peekStr n state = take n (input state)

-- | Parse an integer with optional negative sign
intP :: Parser Int
intP = do
  firstChar <- peek 1
  if null firstChar
    then fatalError "Unexpected EOF in intP"
    else do
      sign <- optional (char '-')
      digits <- some digit <|> fatalErrorWithExpectStr "intP()"
        "a digit" 1
      nextChar <- peek 1
      let numStr = maybe "" (:[]) sign ++ digits
      if null nextChar || isSpace (head nextChar) || isDelim (head nextChar)
        then case readMaybe numStr of
          Just x  -> return x
          Nothing -> fatalError $ "Invalid integer: " ++ numStr
        else fatalError $ "Invalid integer: " ++ numStr ++ nextChar

-- | Parse a string surrounded by specific open and close parsers
between :: Parser open -> Parser a -> Parser close -> Parser a
between open p close = open *> p <* close

-- | Parse a list of items separated by a separator
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | Parse one or more items separated by a separator
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

-- | Count occurrences of a character in a string
count :: Eq a => a -> [a] -> Int
count x = length . P.filter (== x)

-- | Parse a specific string and update the line number
string :: String -> Parser String
string str = P $ \s ->
  if str `isPrefixOf` input s
    then Right (str, s { input = drop (length str) (input s)
                       , lineNum = lineNum s + count '\n' str })
    else Left (FailError ("Expected string " ++ show str
      ++ " at line " ++ show (lineNum s)) (lineNum s))

-- | Parse a string with leading and trailing whitespace
stringP :: String -> Parser ()
stringP s = wsP (string s) *> pure ()

-- | Parse content within parentheses
parens :: Parser a -> Parser a
parens x = between (stringP "(") x (stringP ")")

-- | Parse content within brackets
brackets :: Parser a -> Parser a
brackets x = between (stringP "[") x (stringP "]")

-- | Parse a constant string and return a fixed value
constP :: String -> a -> Parser a
constP s x = wsP (string s) *> pure x
  <|> (do
        s' <- peek (length s)
        failErrorWithExpectStr "constP()" s (length s))

-- | Expect a string without consuming input if it fails
-- Similar to `string`, but does not consume input
-- Partial funtion that takes a string, a parser, and returns a parser
expect :: String -> Parser () -> Parser ()
expect str p = P $ \state -> case doParse p state of
  Right (_, _) -> Right ((), state)  -- Only check presence
  Left (FailError _ _) -> Left (FailError ("Expected " ++ str)
    (lineNum state))
  Left fatalError -> Left fatalError

-- | Expect a specific string
expectString :: String -> Parser ()
expectString str = expect str (void $ string str)

-- | Expect a specific string, ignoring case
expectStringIgnoreCase :: String -> Parser ()
expectStringIgnoreCase str = expect str (void $ stringIgnoreCase str)

-- | Parse a character, ignoring case
charIgnoreCase :: Char -> Parser Char
charIgnoreCase c = satisfy (\x -> toLower x == toLower c)

-- | Parse a string, ignoring case
stringIgnoreCase :: String -> Parser String
stringIgnoreCase = traverse charIgnoreCase

-- | Parse a constant string, ignoring case, and return a fixed value
constIgnoreCaseP :: String -> a -> Parser a
constIgnoreCaseP s x = wsP (stringIgnoreCase s) *> pure x

-- | Advance the line number based on the number of newlines in a string
advanceLine :: String -> Int -> Int
advanceLine s currentLine = currentLine + length (P.filter (== '\n') s)

-- | Parse a newline character (handles different newline formats)
newline :: Parser ()
newline = void (char '\n')

-- | Parse an empty line with optional whitespace
emptyLine :: Parser ()
emptyLine = wsP newline

-- | Parse a comment line starting with "//"
commentLine :: Parser ()
commentLine = wsP (string "//") *> many (satisfy (/= '\n')) *> newline

-- | Skip over empty lines and comment lines
skipCommentOrEmptyLines :: Parser ()
skipCommentOrEmptyLines = void $ many (emptyLine <|> commentLine)

-- | Parse any character
anyChar :: Parser Char
anyChar = get

-- | Parse many characters until a terminating parser succeeds
manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end = go
  where
    go = (end *> pure []) <|> (:) <$> p <*> go

-- ---------------------------------------------------------------
--                       Parser Implementations
-- ---------------------------------------------------------------
{------------------------ Effect Parsing ------------------------}
-- | Parse a coordinate, expecting a colon or an integer
coordinateP :: Parser Coordinate
coordinateP = constP ":" All <|> Coordinate <$> intP

-- | Parse a pair of coordinates within parentheses
coordinatePairP :: Parser CoordinatePair
coordinatePairP = parens $ (,) <$> wsP coordinateP <* wsP (char ',') <*> wsP coordinateP

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
effectRangeLineP = stringIgnoreCase "effect_range:" *> wsP effectRangeP <* newline

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

-- | Update the Difficulty state with a modification function
updateDifficulty :: (Difficulty -> Difficulty) -> Parser ()
updateDifficulty f = P $ \state -> Right ((), state { pDifficulty = f (pDifficulty state) })

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
  updateDifficulty (\d -> d { effectMap = Map.insert name effect
    (effectMap d) })

-- | Parse multiple effect blocks
effectsP :: Parser ()
effectsP = void $ some $ skipCommentOrEmptyLines
  *> expectString "effect_"  -- If a block starts with "effect_", it is an effect block
  *> effectP
  <* skipCommentOrEmptyLines

{------------------------ Constant Parsing ------------------------}
-- | Parse a difficulty constant block
difficultyConstantP :: Parser ()
difficultyConstantP = do
  -- Check if the block is a difficulty constant block
  skipCommentOrEmptyLines
  constIgnoreCaseP "difficulty_constant" () <* newline

  -- Parse dimension
  constP "dimension:" ()
    <|> fatalErrorWithExpectStr "difficultyConstantP()" "`dimension:`" 13
  dimensionVal <- (wsP intP <* newline)
    <|> fatalError "Error parsing dimension integer"
  when (dimensionVal < 3) $
    fatalError "dimension must be >= 3"

  -- Parse max_steps
  constP "max_steps:" ()
    <|> fatalErrorWithExpectStr "difficultyConstantP()" "`max_steps:`" 14
  maxStepsVal <- (wsP intP <* newline)
    <|> fatalError "Error parsing max_steps integer"
  when (maxStepsVal < 3) $
    fatalError "max_steps must be >= 3"

  updateDifficulty (\d -> d { dimension = dimensionVal
    , maxSteps = maxStepsVal })

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
  difficultyVal <- P $ \state -> Right (pDifficulty state, state)
  case Map.lookup name (effectMap difficultyVal) of
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
  updateDifficulty (\d -> d { candyMap = Map.insert name
    (Candy candyDef effect) (candyMap d) })

-- | Parse multiple candy blocks
candiesP :: Parser ()
candiesP = void $ some $ skipCommentOrEmptyLines
  *> expectString "shape_"  -- If a block starts with "shape_", it is a candy block
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
      <|> difficultyConstantP
      <|> failErrorWithExpectStr "parseLoop():"
        "one of [`effect_`, `shape_`, `difficulty_constant`]" 10
    parseLoop

{------------------------ Action Parsing ------------------------}
actionIntP :: Int -> String -> Parser Int
actionIntP dim errorMessage = do
    coord <- wsP intP
    if coord < 0 || coord >= dim
        then failError errorMessage
        else return coord

parseAction :: Int -> String -> Either ParseError Action
parseAction dim input = doParse (actionParser dim) (ParseState input 1 emptyDifficulty) >>= \(action, _) -> Right action

actionParser :: Int -> Parser Action
actionParser dim = wsP $
    parseSwap dim <|> 
    parseClick dim <|> 
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
-- | Parse the entire input file into a Difficulty object
fileP :: String -> Either ParseError Difficulty
fileP input = case doParse (parseLoop <* eof)
  (ParseState input 1 emptyDifficulty) of
    Right (_, state) -> Right (pDifficulty state)  -- Return the parsed Difficulty
    Left err -> Left err                           -- Return the parse error

-- | Take a filename and return the parsed Difficulty object
parseFile :: String -> IO (Either ParseError Difficulty)
parseFile filename = do
  contentResult <- try (readFile filename) :: IO (Either IOException String)
  return $ either
    (Left . (\e -> FatalError ("Error reading `" ++ filename ++ "`: " ++ show e) 0))
    parseAndValidate contentResult
    
-- | Parse and validate the content of a file
parseAndValidate :: String -> Either ParseError Difficulty
parseAndValidate content = 
  case fileP content of
    Left err -> Left err
    Right difficulty -> case validateDifficulty difficulty of
      Left errMsg -> Left (FatalError errMsg 0)
      Right validDifficulty -> Right validDifficulty

validateDifficulty :: Difficulty -> Either String Difficulty
validateDifficulty (Difficulty dimension candyMap _ maxSteps)
  | dimension <= 0 = Left "Dimension must be greater than 0" 
  | maxSteps < 3 = Left "maxSteps must be at least 3"
  | Map.size candyMap < 3 = Left "candyMap must contain at least 3 candies"
  | otherwise = Right (Difficulty dimension candyMap Map.empty maxSteps)