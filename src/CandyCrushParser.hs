module CandyCrushParser where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List (find, isPrefixOf)
import Text.Read (readMaybe)
import Test.HUnit (Assertion, Counts, Test (..), assert, assertFailure, runTestTT, (~:), (~?=), assertBool, Testable (test))
import Test.QuickCheck qualified as QC
import Prelude hiding (filter)
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Prelude as P
import Debug.Trace (trace)
import Constants
import Candy
import qualified Constants as Constant
import Control.Exception (IOException, try)

type Name = String
-- Difficulty level
data Difficulty = Difficulty
    { dimension :: Int
    , candyMap  :: Map Name Candy
    , effectMap :: Map Name Effect
    , maxSteps  :: Int
    } deriving (Show, Eq)

emptyDifficulty :: Difficulty
emptyDifficulty = Difficulty 0 Map.empty Map.empty 0

data ParseState = ParseState
  { input      :: String       -- 当前输入
  , lineNum    :: Int          -- 当前行号
  , difficulty :: Difficulty   -- 当前上下文中的 Difficulty
  } deriving (Show, Eq)

data ParseError = FatalError String Int | FailError String Int
  deriving (Eq)

instance Show ParseError where
  show (FatalError msg line) = "Fatal error at line " ++ show line ++ ": " ++ msg
  show (FailError msg line) = "Error at line " ++ show line ++ ": " ++ msg

newtype Parser a = P { doParse :: ParseState -> Either ParseError (a, ParseState) }
  deriving Functor

instance Applicative Parser where
  pure x = P $ \s -> Right (x, s)
  p1 <*> p2 = P $ \s -> case doParse p1 s of
    Left (FatalError msg line) -> Left (FatalError msg line)  -- 如果 p1 致命错误，停止解析
    Left err -> Left err                                      -- 如果 p1 非致命错误，传播错误
    Right (f, s') -> case doParse p2 s' of                    -- 提取 p1 的结果函数 f 和更新的状态 s'
      Left err -> Left err                                    -- 如果 p2 失败，返回错误
      Right (a, s'') -> Right (f a, s'')                      -- 应用函数 f 到 p2 的结果 a

instance Alternative Parser where
  empty = P $ \state -> Left (FailError "No parses" (lineNum state))
  p1 <|> p2 = P $ \state -> case doParse p1 state of
    Left (FatalError msg line) -> Left (FatalError msg line)  -- 遇到致命错误，直接终止
    Left (FailError _ _) -> doParse p2 state                  -- 尝试下一个解析器
    success -> success                                        -- 返回成功

instance Monad Parser where
  return = pure
  (>>=) = bindParser

bindParser :: Parser a -> (a -> Parser b) -> Parser b
bindParser p f = P $ \s -> case doParse p s of
  Left (FatalError msg line) -> Left (FatalError msg line)    -- 如果 p 致命错误，返回错误
  Left err -> Left err                                        -- 传播非致命错误
  Right (a, s') -> doParse (f a) s'                           -- 如果成功，用结果 a 调用 f

-- 抛出致命错误，包含行号
fatalError :: String -> Parser a
fatalError msg = P $ \state -> Left (FatalError (msg ++ "\n" ++ show state)  (lineNum state))

fatalErrorWithExpectStr :: String -> String -> Int -> Parser a
fatalErrorWithExpectStr msg expectStr len = P $ \state ->
    Left (FatalError (msg ++ " expected: `" ++ expectStr ++ "`, but got `" ++ peekStr len state ++ "`" ++ "\n" ++ show state)
    (lineNum state))

-- 抛出可恢复错误，包含行号
failError :: String -> Parser a
failError msg = P $ \state -> Left (FailError msg (lineNum state))

failErrorWithExpectStr :: String -> String -> Int -> Parser a
failErrorWithExpectStr  msg expectStr len = P $ \state ->
    Left (FailError (msg ++ ", expected: `" ++ expectStr ++ "`, but got `" ++ peekStr len state ++ "`")
    (lineNum state))

-- | Update a parser to upgrade FailError to FatalError
updateFailToFatal :: String -> Parser a -> Parser a
updateFailToFatal errorMsg parser = P $ \state -> case doParse parser state of
    Left (FailError msg _) -> Left (FatalError (errorMsg ++ ": " ++ msg) (lineNum state))  -- 升级为 FatalError
    result               -> result                                 -- 返回其他结果

-- ｜Combines two parsers
-- | If the first parser passes, the FailError will be upgraded to FatalError in the second parser
upgradeToFatalIfFirstSucceeds :: String -> Parser () -> Parser a -> Parser a
upgradeToFatalIfFirstSucceeds errorMsg conditionParser mainParser = do
    _ <- conditionParser
    P $ \state -> case doParse mainParser state of
        Left (FailError msg _) -> Left (FatalError (errorMsg ++ ": " ++ msg) (lineNum state))  -- 升级为 FatalError
        result                 -> result                                     -- 返回其他结果

-- | Strip whitespace from the beginning and end of a parser
wsP :: Parser a -> Parser a
wsP p = many space *> p <* many space


strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

stripP :: Parser String -> Parser String
stripP p = do
    result <- p
    let stripped = strip result
    if null stripped
        then failError "empty string after stripping"
        else return stripped

-- | Return the next character from the input
get :: Parser Char
get = P $ \s -> case input s of
  (c:cs) ->
    let newState = s { input = cs
                     , lineNum = lineNum s + if c == '\n' then 1 else 0 }
    in Right (c, newState)
  [] ->
    Left (FailError ("Unexpected EOF at line " ++ show (lineNum s)) (lineNum s))

-- | This parser *only* succeeds at the end of the input.
eof :: Parser ()
eof = P $ \s -> case input s of
  [] -> Right ((), s)  -- 输入为空，成功解析
  _  -> Left (FailError ("Expected EOF at line " ++ show (lineNum s) ++ "but got `" ++ peekStr 5 s ++ "`...") (lineNum s))

-- | Filter the parsing results by a predicate
filter :: Show a => (a -> Bool) -> Parser a -> Parser a
filter f p = P $ \s -> case doParse p s of
  Left err -> Left err  -- 错误已经包含行号信息
  Right (c, s') ->
    if f c
      then Right (c, s')
      else Left (FailError ("Value " ++ show c ++ " does not satisfy the predicate at line " ++ show (lineNum s')) (lineNum s'))

-- | Return the next character if it satisfies the given predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = filter p get

isSpaceOrTab :: Char -> Bool
isSpaceOrTab = (`elem` " \t\r")

-- | Parsers for specific sorts of characters
alpha, digit, upper, lower, space :: Parser Char
alpha = satisfy isAlpha
digit = satisfy isDigit
upper = satisfy isUpper
lower = satisfy isLower
space = satisfy isSpaceOrTab

-- | Return the next character if it matches the given character
char :: Char -> Parser Char
char c = satisfy (== c)

delims :: Set Char
delims = Set.fromList [ '(', ')', '[', ']', '{', '}',' ',',']

isDelim :: Char -> Bool
isDelim c = Set.member c delims

-- | Return the next n characters from the input without consuming them
peek :: Int -> Parser String
peek n = P $ \s ->
    if null (input s)
    then Right ("", s)
    else Right (take n (input s), s)

peekStr :: Int -> ParseState -> String
peekStr n state =
    let currentInput = input state
    in take n currentInput

intP :: Parser Int
intP = do
  firstChar <- peek 1
  if null firstChar
    then fatalError "Unexpected EOF in intP"
    else do
      sign <- optional (char '-')                     -- 可选负号
      digits <- some digit <|> fatalErrorWithExpectStr "intP()" "a digit" 1
      nextChar <- peek 1                              -- 查看下一个字符
      let numStr = maybe "" (:[]) sign ++ digits      -- 组合成完整数字字符串
      if null nextChar || isSpace (head nextChar) || isDelim (head nextChar)
        then case readMaybe numStr of
               Just x  -> return x                    -- 成功解析为整数
               Nothing -> fatalError $ "Invalid integer: " ++ numStr
        else fatalError $ "Invalid integer: " ++ numStr ++ nextChar

between :: Parser open -> Parser a -> Parser close -> Parser a
between open p close = open *> p <* close

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

count :: Eq a => a -> [a] -> Int
count x = length . P.filter (== x)

-- | Parse a string
string :: String -> Parser String
string str = P $ \s ->
    if str `isPrefixOf` input s
        then Right (str, s { input = drop (length str) (input s), lineNum = lineNum s + count '\n' str })
        else Left (FailError ("Expected string " ++ show str ++ " at line " ++ show (lineNum s)) (lineNum s))

-- | Parse a string with leading and trailing whitespace
stringP :: String -> Parser ()
stringP s = wsP (string s) *> pure ()

parens :: Parser a -> Parser a
parens x = between (stringP "(") x (stringP ")")

brackets :: Parser a -> Parser a
brackets x = between (stringP "[") x (stringP "]")

constP :: String -> a -> Parser a
constP s x = wsP (string s) *> pure x
             <|>
             (do peek (length s) >>= \s' -> failErrorWithExpectStr "constP()" s (length s))

-- | Parse a string with provided parse but does not consume input
-- constP consumes input if successful, this does not consume input in any case
-- Partial function that is similar to `satify` but for strings and does not consume input 
expect :: String -> Parser () -> Parser ()
expect str p =  P $ \state -> case doParse p state of
    Right (_, _) -> Right ((), state) -- only check if the string is present, do not change the state
    Left (FailError _ _) -> Left (FailError ("Expected " ++ str) (lineNum state))
    Left fatalError      -> Left fatalError

-- | Same as `constP` but does not consume input or change the state
expectString :: String -> Parser ()
expectString str = expect str (void $ string str)
-- | Same as `constIgnoreCaseP` but does not consume input or change the state
expectStringIgnoreCase :: String -> Parser ()
expectStringIgnoreCase str = expect str (void $ stringIgnoreCase str)

charIgnoreCase :: Char -> Parser Char
charIgnoreCase c = satisfy (\x -> toLower x == toLower c)

stringIgnoreCase :: String -> Parser String
stringIgnoreCase = traverse charIgnoreCase

constIgnoreCaseP :: String -> a -> Parser a
constIgnoreCaseP s x = wsP (stringIgnoreCase s) *> pure x

advanceLine :: String -> Int -> Int
advanceLine s currentLine = currentLine + length (P.filter (== '\n') s)

-- | Parse a newline character

newline :: Parser ()
newline = void (string "\r\n" <|> string "\n" <|> string "\r")

emptyLine :: Parser ()
emptyLine = wsP newline

commentLine :: Parser ()
commentLine = wsP (string "//") *> many (satisfy (/= '\n')) *> newline

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
--                      解析器实现部分
-- ---------------------------------------------------------------

coordinateP :: Parser Coordinate
coordinateP = constP ":" All <|> Coordinate <$> intP

coordinatePairP :: Parser CoordinatePair
coordinatePairP = parens $ (,) <$> wsP coordinateP <* wsP (char ',') <*> wsP coordinateP

coordinateListP :: Parser [CoordinatePair]
coordinateListP = brackets $ coordinatePairP `sepBy` wsP (char ',')

circleP :: Parser EffectRange
circleP = upgradeToFatalIfFirstSucceeds "Error while parsing Circle"
    (constIgnoreCaseP "Circle" ())
    (Circle <$> wsP intP)

rectangleP :: Parser EffectRange
rectangleP = upgradeToFatalIfFirstSucceeds "Error while parsing Rectangle"
    (constIgnoreCaseP "Rectangle" ())
    (Rectangle <$> wsP intP <*> wsP intP)

diamondP :: Parser EffectRange
diamondP = upgradeToFatalIfFirstSucceeds "Error while parsing Diamond"
    (constIgnoreCaseP "Diamond" ())
    (Diamond <$> wsP intP)

arbitraryP :: Parser EffectRange
arbitraryP = upgradeToFatalIfFirstSucceeds "Error while parsing Arbitrary"
    (constIgnoreCaseP "Arbitrary" ())
    (Arbitrary <$> wsP coordinateListP)

effectRangeP :: Parser EffectRange
effectRangeP = circleP
    <|> rectangleP
    <|> diamondP
    <|> arbitraryP
    <|> fatalError "Unrecognized effect range type"

effectNameP :: Parser String
effectNameP =
    constIgnoreCaseP "effect_name:" ()
    *> stripP (some (satisfy (/= '\n')))
    <* newline
    <|> fatalErrorWithExpectStr "effectNameP()" "effect_name: <name>" 16

effectRangeLineP:: Parser EffectRange
effectRangeLineP = stringIgnoreCase "effect_range:" *> wsP effectRangeP <* newline

-- | Parse a comparison operator
-- This parser parses the operators defined in the `Operator` data type and operatorMapping.
-- Does not check if the operator is allowed. This check is done in calling func such as `effectRequirementP`.
operatorP :: Parser Operator
operatorP = foldr ((<|>) . makeOpParser) empty operatorMapping
  where
    makeOpParser (str, op) = constP str op

effectRequirementP :: Parser EffectRequirement
effectRequirementP = do
    constIgnoreCaseP "effect_requirement:" ()
    EffectRequirement <$> (wsP operatorP >>= checkOperator) <*> (wsP intP >>= checkPositive) <* newline
    <|> fatalErrorWithExpectStr "effectRequirementP()" "effect_requirement: [>, >=, =] <number>" 23
  where
    -- check if the operator is allowed
    checkOperator op
        | op `elem` allowedEffectReqOperators = return op
        | otherwise = fatalError $ "operator " ++ show op ++ " is not allowed"
    -- check if the number is positive
    checkPositive n
        | n < 0     = fatalErrorWithExpectStr "checkPositive()" "a positive number" 3
        | otherwise = return n

-- 解析 effect_description
effectDescriptionP :: Parser String
effectDescriptionP = do
    constIgnoreCaseP "effect_description:" () 
        <|> fatalErrorWithExpectStr "effectDescriptionP()" "`effect_description:`" 23
    stripP (many (satisfy (/= '\n'))) <* newline 
    <|> fatalErrorWithExpectStr "effectDescriptionP()" "`effect_description: <some descrp>`" 23

-- 更新 Difficulty 的 effects 列表
updateDifficulty :: (Difficulty -> Difficulty) -> Parser ()
updateDifficulty f = P $ \state -> Right ((), state { difficulty = f (difficulty state) })


effectP :: Parser ()
effectP = do
    name <- updateFailToFatal "Error parsing effect name" effectNameP
    range <- updateFailToFatal "Error parsing effect range" effectRangeLineP
    requirement <- updateFailToFatal "Error parsing effect requirement" effectRequirementP
    description <- updateFailToFatal "Error parsing effect description" effectDescriptionP
    let effect = Effect name range requirement description
    updateDifficulty (\d -> d { effectMap = Map.insert name effect (effectMap d) })

effectsP :: Parser ()
effectsP = void $ some $ skipCommentOrEmptyLines
           *> expectString "effect_"  -- if a block starts with "effect", it is an effectP block
           *> effectP <* skipCommentOrEmptyLines

difficultyConstantP :: Parser ()
difficultyConstantP = do
    -- check if the block is a difficulty constant block
    skipCommentOrEmptyLines
    constIgnoreCaseP "difficulty_constant" () <*newline

    -- parse dimension
    constP "dimension:" () <|> fatalErrorWithExpectStr "difficultyConstantP()" "`dimension:`" 13
    -- parse the number and skip the newline
    dimension <- (wsP intP <* newline) <|> fatalError "error parsing dimension int"
    when (dimension < 3) $ fatalError "dimension must be >= 3"

    -- parse max_steps
    constP "max_steps:" () <|> fatalErrorWithExpectStr "difficultyConstantP()" "`max_steps:`" 14
    maxSteps <- (wsP intP <* newline) <|> fatalError "error parsing max_steps int"
    when (maxSteps < 3) $ fatalError "max_steps must be >= 3"

    updateDifficulty (\d -> d { dimension = dimension, maxSteps = maxSteps })

-- 解析 shape_name
shapeNameP :: Parser String
shapeNameP = do
    constIgnoreCaseP "shape_name:" () <|> fatalError "expected `shape_name:`, but got something else."
    name <- stripP (some (satisfy (/= '\n'))) <|> fatalError "shape_name cannot be empty"
    newline <|> fatalError "missing newline after shape_name"
    return name

-- 解析 shape_icon
shapeIconP :: Parser String
shapeIconP = do
    constIgnoreCaseP "shape_icon:" () <|> fatalErrorWithExpectStr "shapeIconP()" "`shape_icon: <icon>`" 13
    icon <- stripP (some (satisfy (/= '\n'))) <|> fatalError "shape_icon cannot be empty"
    newline <|> fatalError "Missing newline after shape_icon"
    return icon

-- 解析 effect_name
effectNameRefP :: Parser String
effectNameRefP = do
    constIgnoreCaseP "effect_name:" () <|> fatalErrorWithExpectStr "effectNameRefP()" "`effect_name: <name>`" 14
    effectName <- stripP (some (satisfy (/= '\n'))) <|> fatalError "effect name cannot be empty"
    newline <|> fatalError "Missing newline after effect_name"
    return effectName

effectNameToEffect :: String -> Parser Effect
effectNameToEffect name = do
    difficulty <- P $ \state -> Right (difficulty state, state)
    case Map.lookup name (effectMap difficulty) of
        Just effect -> return effect
        Nothing -> fatalError $ "Effect `" ++ name ++ "` not found in the effect map"

candyP :: Parser ()
candyP = do
    skipCommentOrEmptyLines
    name <- shapeNameP
    icon <- shapeIconP
    effectName <- effectNameRefP
    effect <- effectNameToEffect effectName
    let candyDef = CandyDefinition name icon effectName
    updateDifficulty (\d -> d { candyMap = Map.insert name (Candy candyDef effect) (candyMap d) })

candiesP :: Parser ()
candiesP = void $ some $ skipCommentOrEmptyLines
           *> expectString "shape_"  -- if a block starts with "shape", it is a candyP block
           *> candyP <* skipCommentOrEmptyLines

parseLoop :: Parser ()
parseLoop = do
    skipCommentOrEmptyLines
    notEOF <- (eof *> pure False) <|> pure True
    when notEOF $ do
        effectsP
            <|> candiesP
            <|> difficultyConstantP
            <|> failErrorWithExpectStr "parseLoop():" "one of [`effect_`, `shape_`, `difficulty_constant`]" 10
        parseLoop

-- | Parse the entire input file into a Difficulty object
fileP :: String -> Either ParseError Difficulty
fileP input = case doParse (parseLoop <* eof) (ParseState input 1 emptyDifficulty) of
    Right (_, state) -> Right (difficulty state)  -- 返回解析后的 Difficulty
    Left err -> Left err  -- 返回解析错误

parseFile :: String -> IO (Either ParseError Difficulty)
parseFile filename = do
    -- read the file content, catch any IO exceptions
    result <- try (readFile filename) :: IO (Either IOException String)
    case result of
        Left ex -> return $ Left (FatalError ("error reading `" ++ filename ++ "`") 0)
        Right content -> return (fileP content)


