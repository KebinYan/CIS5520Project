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

import Candy

type Name = String
-- Difficulty level
data Difficulty = Difficulty
    { dimension :: Int
    , candyMap  :: Map Name CandyShape
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
fatalError msg = P $ \state -> Left (FatalError msg (lineNum state))

-- 抛出可恢复错误，包含行号
failError :: String -> Parser a
failError msg = P $ \state -> Left (FailError msg (lineNum state))

-- | Update a parser to upgrade FailError to FatalError
updateFailToFatal :: String -> Parser a -> Parser a
updateFailToFatal msg parser = P $ \state -> case doParse parser state of
    Left (FailError _ _) -> Left (FatalError msg (lineNum state))  -- 升级为 FatalError
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

-- | Combine two Maybe values together, producing the first
-- successful result
firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just x) _ = Just x
firstJust Nothing  y = y

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
  _  -> Left (FailError ("Expected EOF but found more input at line " ++ show (lineNum s)) (lineNum s))

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

intP :: Parser Int
intP = do
  firstChar <- peek 1
  if null firstChar
    then fatalError "Unexpected EOF in intP"
    else do
      sign <- optional (char '-')                     -- 可选负号
      digits <- some digit <|> fatalError "Missing digits in intP"  -- 至少一个数字
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

string :: String -> Parser String
string str = P $ \s ->
    if str `isPrefixOf` input s
        then Right (str, s { input = drop (length str) (input s) })
        else Left (FailError ("Expected string " ++ show str ++ " at line " ++ show (lineNum s)) (lineNum s))

stringP :: String -> Parser ()
stringP s = wsP (string s) *> pure ()

parens :: Parser a -> Parser a
parens x = between (stringP "(") x (stringP ")")

brackets :: Parser a -> Parser a
brackets x = between (stringP "[") x (stringP "]")

constP :: String -> a -> Parser a
constP s x = wsP (string s) *> pure x

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
newline = trace "\nnewline called once" $ void $ char '\n'


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
effectNameP = upgradeToFatalIfFirstSucceeds "error while parsing effect name"
    (constIgnoreCaseP "effect_name:" ())
    (do
        name <- wsP (some (satisfy (/= '\n')))
        if null name
            then fatalError "Effect name cannot be empty"
            else return name
        <* (wsP newline <|> fatalError "Missing newline after effect name")
    )



testEffectNameP :: Test
testEffectNameP = TestList
  [ "Valid effect name" ~: 
      doParse effectNameP (ParseState "effect_name: StripedRow\n" 1 emptyDifficulty)
      ~?= Right ("StripedRow", ParseState "" 2 emptyDifficulty)
    , "Effect name with spaces" ~: 
        doParse effectNameP (ParseState "effect_name: Striped Row\n" 1 emptyDifficulty)
        ~?= Right ("Striped Row", ParseState "" 2 emptyDifficulty)
    , "Missing effect name" ~: assertIsFatalError 
        (doParse effectNameP (ParseState "effect_name: \n" 1 emptyDifficulty))
        "Expected a FatalError for missing effect name"
    , "Invalid prefix" ~: assertIsFatalError
        (doParse effectNameP (ParseState "invalid_effect_name: 123\n" 1 emptyDifficulty))
        "Expected a FatalError for invalid effect name"
    , "Missing EOF" ~: assertIsFatalError
        (doParse effectNameP (ParseState "effect_name: StripedRow" 1 emptyDifficulty))
        "Expected a FatalError for missing newline"
  ]
testEffectRangeLineP :: Test
testEffectRangeLineP = TestList
  [ "Valid effect range" ~: 
      doParse effectRangeLineP (ParseState "effect_range: Arbitrary [(:,0)]\n" 1 emptyDifficulty)
      ~?= Right (Arbitrary [(All, Coordinate 0)], ParseState "" 2 emptyDifficulty)
  , "Invalid effect range" ~:  assertIsFatalError 
    (doParse effectRangeLineP (ParseState "effect_range: InvalidRange\n" 1 emptyDifficulty))
    "Expected a FatalError for invalid effect range"
  ]
testOperatorP :: Test
testOperatorP = TestList
  [ "Valid operators" ~: TestList 
      [ doParse operatorP (ParseState "=\n" 1 emptyDifficulty) ~?= Right (Eq, ParseState "\n" 1 emptyDifficulty)
      , doParse operatorP (ParseState ">\n" 1 emptyDifficulty) ~?= Right (Gt, ParseState "\n" 1 emptyDifficulty)
      , doParse operatorP (ParseState ">=\n" 1 emptyDifficulty) ~?= Right (Ge, ParseState "\n" 1 emptyDifficulty)
      ]
    , "Invalid operator le" ~: assertIsFailError 
        (doParse operatorP (ParseState "<=\n" 1 emptyDifficulty))
        "Expected a FailError for invalid operator"
    , "invalid operator lt" ~: assertIsFailError 
        (doParse operatorP (ParseState "<\n" 1 emptyDifficulty))
        "Expected a FailError for invalid operator"
    , "unknown operator" ~: assertIsFailError 
        (doParse operatorP (ParseState "--\n" 1 emptyDifficulty))
        "Expected a FailError for unknown operator"
  ]
testEffectRequirementP :: Test
testEffectRequirementP = TestList
  [ "Valid effect requirement with default =" ~: 
      doParse effectRequirementP (ParseState "effect_requirement: 5\n" 1 emptyDifficulty)
      ~?= Right (EffectRequirement Eq 5, ParseState "" 2 emptyDifficulty)
    , "Valid effect requirement with =" ~: 
        doParse effectRequirementP (ParseState "effect_requirement: = 5\n" 1 emptyDifficulty)
        ~?= Right (EffectRequirement Eq 5, ParseState "" 2 emptyDifficulty)
    , "Valid effect requirement with >" ~: 
        doParse effectRequirementP (ParseState "effect_requirement: > 3\n" 1 emptyDifficulty)
        ~?= Right (EffectRequirement Gt 3, ParseState "" 2 emptyDifficulty)
    , "Invalid operator in requirement" ~: 
        assertIsFatalError 
            (doParse effectRequirementP (ParseState "effect_requirement: <= 5\n" 1 emptyDifficulty))
            "Expected a FailError for invalid operator"
    , "Invalid negative number in requirement" ~: 
        assertIsFatalError 
            (doParse effectRequirementP (ParseState "effect_requirement: > -5\n" 1 emptyDifficulty))
            "Expected a FailError for negative number"
    , "Invalid number in requirement" ~: 
        assertIsFatalError 
            (doParse effectRequirementP (ParseState "effect_requirement: > abc\n" 1 emptyDifficulty))
            "Expected a FailError for invalid number"
    , "Missing number in requirement" ~: 
        assertIsFatalError 
            (doParse effectRequirementP (ParseState "effect_requirement: >\n" 1 emptyDifficulty))
            "Expected a FailError for missing number"
    , "Invalid operator in requirement" ~: 
        assertIsFatalError 
            (doParse effectRequirementP (ParseState "invalid_effect_requirement: > 5\n" 1 emptyDifficulty))
            "Expected a FatalError for invalid effect requirement"
  ]
testEffectDescriptionP :: Test
testEffectDescriptionP = TestList
  [ "Valid effect description" ~: 
      doParse effectDescriptionP (ParseState "effect_description: Clears the row\n" 1 emptyDifficulty)
      ~?= Right ("Clears the row", ParseState "" 2 emptyDifficulty)
  , "Missing effect description" ~: doParse effectDescriptionP (ParseState "effect_description: \n" 1 emptyDifficulty)
        ~?= Right ("", ParseState "" 1 emptyDifficulty)
  ]
effectRangeLineP:: Parser EffectRange
effectRangeLineP = stringIgnoreCase "effect_range:" *> wsP effectRangeP <* newline

operatorP :: Parser Operator
operatorP = foldr ((<|>) . makeOpParser) empty operatorMapping
  where
    makeOpParser (str, op) = constP str op

effectRequirementP :: Parser EffectRequirement
effectRequirementP =  upgradeToFatalIfFirstSucceeds "error while parsing EffectRequirement"
    (constIgnoreCaseP "effect_requirement:" ())
    ((EffectRequirement <$> (wsP operatorP >>= checkOperator) <*> (wsP intP >>= checkPositive)) <* newline)
  where
    -- check if the operator is allowed
    checkOperator op
        | op `elem` allowedEffectReqOperators = return op
        | otherwise = fatalError $ "operator " ++ show op ++ " is not allowed"
    -- check if the number is positive
    checkPositive n
        | n < 0     = fatalError "expected a positive number"
        | otherwise = return n

-- 解析 effect_description
effectDescriptionP :: Parser String
effectDescriptionP = upgradeToFatalIfFirstSucceeds "error while parsing effect description"
    (constIgnoreCaseP "effect_description:" ())
    (wsP (many (satisfy (/= '\n'))) <* newline)

-- 更新 Difficulty 的 effects 列表
updateDifficulty :: (Difficulty -> Difficulty) -> Parser ()
updateDifficulty f = P $ \state -> Right ((), state { difficulty = f (difficulty state) })

effectP :: Parser ()
effectP = do
    name <- effectNameP
    range <- effectRangeLineP
    requirement <- effectRequirementP
    description <- effectDescriptionP
    let effect = Effect name range requirement description
    updateDifficulty (\d -> d { effectMap = Map.insert name effect (effectMap d) })


effectsP :: Parser ()
effectsP = void $ many $ skipCommentOrEmptyLines *> effectP <* skipCommentOrEmptyLines
-- ---------------------------------------------------------------
--                      测试用例
-- ---------------------------------------------------------------

-- 为了测试，我们需要定义一些辅助函数

-- 断言解析成功
assertIsSuccess :: (Show a, Eq b, Show b) => Either a b -> b -> String -> IO ()
assertIsSuccess result expected errMsg =
  case result of
    Right x | x == expected -> return () -- 解析成功且结果符合预期，测试通过
            | otherwise -> error $ errMsg ++ ", but got: " ++ show x
    Left e -> error $ errMsg ++ ", but got error: " ++ show e

-- 断言解析失败（FatalError）
assertIsFatalError :: (Show b) => Either ParseError b -> String -> IO ()
assertIsFatalError result errMsg =
  case result of
    Left (FatalError _ _) -> return () -- 解析失败，测试通过
    Left (FailError e line) -> error $ errMsg ++ ", but got fail error at line " ++ show line ++ ": " ++ e
    Right x -> error $ errMsg ++ ", but got: " ++ show x

-- 断言解析失败（FailError）
assertIsFailError :: (Show b) => Either ParseError b -> String -> IO ()
assertIsFailError result errMsg =
  case result of
    Left (FailError _ _) -> return () -- 解析失败，测试通过
    Left (FatalError e line) -> error $ errMsg ++ ", but got fatal error at line " ++ show line ++ ": " ++ e
    Right x -> error $ errMsg ++ ", but got: " ++ show x


testIntP :: Test
testIntP = TestList
  [ "Valid positive integer" ~: TestCase $ assertIsSuccess
        (doParse intP (ParseState "123 abc" 1 emptyDifficulty))
        (123, ParseState " abc" 1 emptyDifficulty)
        "Expected to parse a valid positive integer"
  , "Valid negative integer" ~:TestCase $ assertIsSuccess
        (doParse intP (ParseState "-456 xyz" 1 emptyDifficulty))
        (-456, ParseState " xyz" 1 emptyDifficulty)
        "Expected to parse a valid negative integer"
  , "Empty input" ~: assertIsFatalError
        (doParse intP (ParseState "" 1 emptyDifficulty))
        "Expected an error for empty input"
  , "Invalid integer with only '-'" ~: assertIsFatalError
        (doParse intP (ParseState "- xyz" 1 emptyDifficulty))
        "Expected an error for invalid '-' input"
  , "Trailing invalid characters" ~: assertIsFatalError
        (doParse intP (ParseState "123.45 xyz" 1 emptyDifficulty))
        "Expected an error for trailing invalid characters"
  , "Extra characters" ~: assertIsFatalError
        (doParse intP (ParseState "123abc" 1 emptyDifficulty))
        "Expected to stop parsing at the first invalid character"
  ]

testBetween :: Test
testBetween = TestList [
    "Between parentheses" ~:
        doParse (between (stringP "(") (string "content") (stringP ")"))
        (ParseState "(content)" 1 emptyDifficulty)
        ~?= Right ("content", ParseState "" 1 emptyDifficulty),
    "Between brackets with space" ~:
        doParse (between (stringP "[") (string "content") (stringP "]"))
        (ParseState "[ content ]" 1 emptyDifficulty)
        ~?= Right ("content", ParseState "" 1 emptyDifficulty),
    "inalid between mismatched brackets" ~:
        TestCase $ case doParse (between (stringP "[") (string "content") (stringP "]"))
                (ParseState "[content)" 1 emptyDifficulty) of
            Left (FailError _ _) -> return ()
            _                  -> assertFailure "Expected a FailError for mismatched brackets"
    ]
testSepBy :: Test
testSepBy = TestList [
    "Comma-separated values" ~:
        doParse (sepBy (string "item") (stringP ","))
        (ParseState "item,item,item" 1 emptyDifficulty)
        ~?= Right (["item", "item", "item"], ParseState "" 1 emptyDifficulty),
    "Single item" ~:
        doParse (sepBy (string "item") (stringP ","))
        (ParseState "item" 1 emptyDifficulty)
        ~?= Right (["item"], ParseState "" 1 emptyDifficulty),
    "Empty list" ~:
        doParse (sepBy (string "item") (stringP ","))
        (ParseState "" 1 emptyDifficulty)
        ~?= Right ([], ParseState "" 1 emptyDifficulty)
    ]

testSepBy1 :: Test
testSepBy1 = TestList [
    "Comma-separated values" ~:
        doParse (sepBy1 (string "item") (stringP ","))
        (ParseState "item,item,item" 1 emptyDifficulty)
        ~?= Right (["item", "item", "item"], ParseState "" 1 emptyDifficulty),
    "Single item" ~:
        doParse (sepBy1 (string "item") (stringP ","))
        (ParseState "item" 1 emptyDifficulty)
        ~?= Right (["item"], ParseState "" 1 emptyDifficulty),
    "Empty list" ~:
        TestCase $ case doParse (sepBy1 (string "item") (stringP ","))
                (ParseState "" 1 emptyDifficulty) of
            Left (FailError _ _) -> return ()
            _                  -> assertFailure "Expected a FailError for an empty list"
    ]

testBrackets :: Test
testBrackets = TestList [
    "Value inside brackets" ~:
        doParse (brackets (string "inner"))
        (ParseState "[inner]" 1 emptyDifficulty)
        ~?= Right ("inner", ParseState "" 1 emptyDifficulty),
    "Mismatched brackets" ~:
        TestCase $ case doParse (brackets (string "inner"))
                (ParseState "[inner)" 1 emptyDifficulty) of
            Left (FailError _ _) -> return ()
            _                  -> assertFailure "Expected a FailError for mismatched brackets"
    ]

testParens :: Test
testParens = TestList [
    "Value inside parentheses" ~:
        doParse (parens (string "inner"))
        (ParseState "(inner)" 1 emptyDifficulty)
        ~?= Right ("inner", ParseState "" 1 emptyDifficulty),
    "Missing closing parenthesis" ~:
        TestCase $ case doParse (parens (string "inner"))
                (ParseState "(inner" 1 emptyDifficulty) of
            Left (FailError _ _) -> return ()
            _                  -> assertFailure "Expected a FailError for missing closing parenthesis"
    ]

testConstP :: Test
testConstP = TestList [
    "Constant parsing with exact match (no trailing whitespaces)" ~:
        doParse (constP "constant" 42)
        (ParseState "constant followed by text" 1 emptyDifficulty)
        ~?= Right (42, ParseState "followed by text" 1 emptyDifficulty),
    "Mismatch for constant" ~:
        TestCase $ case doParse (constP "constant" 42)
                (ParseState "wrong input" 1 emptyDifficulty) of
            Left (FailError _ _) -> return ()
            _                  -> assertFailure "Expected a FailError for mismatched constant"
    ]


testString :: Test
testString = TestList [
    "Exact match (no trailing whitespaces)" ~:
        doParse (string "hello")
        (ParseState "hello world" 1 emptyDifficulty)
        ~?= Right ("hello", ParseState " world" 1 emptyDifficulty),
    "Mismatch" ~:
        TestCase $ case doParse (string "hello")
                (ParseState "hi world" 1 emptyDifficulty) of
            Left (FailError _ _) -> return ()
            _                  -> assertFailure "Expected a FailError for mismatch"
    ]

testStringP :: Test
testStringP = TestList [
    "Exact match with whitespace" ~:
        doParse (stringP "hello")
        (ParseState "  hello  world" 1 emptyDifficulty)
        ~?= Right ((), ParseState "world" 1 emptyDifficulty),
    "Mismatch" ~:
        TestCase $ case doParse (stringP "hello")
                (ParseState "  hi world" 1 emptyDifficulty) of
            Left (FailError _ _) -> return ()
            _                  -> assertFailure "Expected a FailError for mismatch"
    ]

testCoordinateP :: Test
testCoordinateP = TestList [
    "Specific Pos coordinate" ~: assertIsSuccess
        (doParse coordinateP ( ParseState "5" 1 emptyDifficulty))
        (Coordinate 5, ParseState "" 1 emptyDifficulty)
        "Expected to parse a valid positive integer"
    , "Specific Neg coordinate" ~: assertIsSuccess
        (doParse coordinateP ( ParseState "-5" 1 emptyDifficulty))
        (Coordinate (-5), ParseState "" 1 emptyDifficulty)
        "Expected to parse a valid negative integer"
    , "Wildcard coordinate" ~: assertIsSuccess
        (doParse coordinateP ( ParseState ":" 1 emptyDifficulty))
        (All, ParseState "" 1 emptyDifficulty)
        "Expected to parse a wildcard coordinate"
    , "Invalid coordinate" ~: assertIsFatalError
        (doParse coordinateP ( ParseState "abc" 1 emptyDifficulty))
        "Expected a fatal error for invalid input"
    ]
testCoordinatePairP :: Test
testCoordinatePairP = TestList
    [
        "Valid coordinate pair 1" ~: doParse coordinatePairP (ParseState "( -100 , 2 )" 1 emptyDifficulty)
            ~?= Right ((Coordinate (-100), Coordinate 2), ParseState "" 1 emptyDifficulty)
        ,"Valid coordinate pair 2" ~: doParse coordinatePairP (ParseState "(1,1)" 1 emptyDifficulty)
            ~?= Right ((Coordinate 1, Coordinate 1), ParseState "" 1 emptyDifficulty)
        ,"Valid coordinate pair 3" ~: doParse coordinatePairP (ParseState "(-1, 1000)" 1 emptyDifficulty)
            ~?= Right ((Coordinate (-1), Coordinate 1000), ParseState "" 1 emptyDifficulty)
        ,"Valid coordinate pair 4" ~: doParse coordinatePairP (ParseState "( 1000,-1)" 1 emptyDifficulty)
            ~?= Right ((Coordinate 1000, Coordinate (-1)), ParseState "" 1 emptyDifficulty)
         ,"Valid coordinate pair 5" ~: doParse coordinatePairP (ParseState "(-1,-1)" 1 emptyDifficulty)
            ~?= Right ((Coordinate (-1), Coordinate (-1)), ParseState "" 1 emptyDifficulty)
        , "Wildcard Pos Pair" ~: doParse coordinatePairP (ParseState "( 1  , :)" 1 emptyDifficulty)
            ~?= Right ((Coordinate 1, All), ParseState "" 1 emptyDifficulty)
        , "Wildcard Neg Pair" ~: doParse coordinatePairP (ParseState "( :, -2  )" 1 emptyDifficulty)
            ~?= Right ((All, Coordinate (-2)), ParseState "" 1 emptyDifficulty)
        , "Wildcard pair" ~: doParse coordinatePairP (ParseState "( : , : )" 1 emptyDifficulty)
            ~?= Right ((All, All), ParseState "" 1 emptyDifficulty)
        , "Invalid coordinate pair 1" ~: assertIsFatalError
            (doParse coordinatePairP (ParseState "(1, abc)" 1 emptyDifficulty))
            "Expected a fatal error for invalid input"
        ,  "Valid brackets with single pair" ~: doParse coordinateListP (ParseState "[(1,2)]" 1 emptyDifficulty)
            ~?= Right ([(Coordinate 1, Coordinate 2)], ParseState "" 1 emptyDifficulty)
        ,  "Valid brackets with multiple pairs" ~: doParse coordinateListP (ParseState "[(1,2),(3,4)]" 1 emptyDifficulty)
            ~?= Right ([(Coordinate 1, Coordinate 2), (Coordinate 3, Coordinate 4)], ParseState "" 1 emptyDifficulty)
        , "Empty brackets" ~:
            doParse coordinateListP (ParseState "[]" 1 emptyDifficulty)
            ~?= Right ([], ParseState "" 1 emptyDifficulty)
        , "testCoordinatePairP Missing closing bracket" ~: assertIsFailError
            (doParse coordinateListP (ParseState "[(1,2),(3,4)" 1 emptyDifficulty))
            "Expected a fatal error for missing closing bracket"
        , "Invalid coordinate pair 2" ~: assertIsFailError
            (doParse coordinateListP (ParseState "[(1,2),(3)]" 1 emptyDifficulty))
            "Expected a fatal error for invalid coordinate pair"
    ]
testArbitraryPIgnoreCase :: Test
testArbitraryPIgnoreCase = TestList
    [ "Arbitrary with single coordinate pair" ~:
        doParse arbitraryP (ParseState "Arbitrary [(1,2)]" 1 emptyDifficulty)
        ~?= Right (Arbitrary [(Coordinate 1, Coordinate 2)], ParseState "" 1 emptyDifficulty)
    , "Arbitrary with wildcard coordinates" ~:
        doParse arbitraryP (ParseState "Arbitrary [(0,:)]" 1 emptyDifficulty)
        ~?= Right (Arbitrary [(Coordinate 0, All)], ParseState "" 1 emptyDifficulty)
    , "Arbitrary with multiple coordinate pairs" ~:
        doParse arbitraryP (ParseState "Arbitrary [(1,2),(:,:),(-2,12)]" 1 emptyDifficulty)
        ~?= Right (Arbitrary [(Coordinate 1, Coordinate 2), (All, All), (Coordinate (-2), Coordinate 12)], ParseState "" 1 emptyDifficulty)
    , "testArbitraryPIgnoreCase Arbitrary with missing closing bracket" ~: assertIsFatalError
        (doParse arbitraryP (ParseState "Arbitrary [(1,2),(:,:),(-2,12)" 1 emptyDifficulty))
        "Expected a fatal error for missing closing bracket"
    , "Arbitrary with invalid coordinate tuple" ~: assertIsFatalError
        (doParse arbitraryP (ParseState "Arbitrary [(1,2),(1,2,3)]" 1 emptyDifficulty))
        "Expected a fatal error for invalid tuple"
    ]

testCoordinateListP :: Test
testCoordinateListP = TestList
    [
        "Empty list" ~: doParse coordinateListP (ParseState "[]" 1 emptyDifficulty)
            ~?= Right ([], ParseState "" 1 emptyDifficulty)
        , "Single pair" ~: doParse coordinateListP (ParseState "[ (1 , 2 ) ]" 1 emptyDifficulty)
            ~?= Right ([(Coordinate 1, Coordinate 2)], ParseState "" 1 emptyDifficulty)
        , "Multiple pairs" ~: doParse coordinateListP (ParseState "[ ( 1,2 ), (:,:), (-2, 12)]" 1 emptyDifficulty)
            ~?= Right ([(Coordinate 1, Coordinate 2), (All, All), (Coordinate (-2), Coordinate 12)], ParseState "" 1 emptyDifficulty)
    ]

testCirclePIgnoreCase :: Test
testCirclePIgnoreCase = TestList
    [ "circle 3" ~: doParse circleP (ParseState "Circle 3" 1 emptyDifficulty)
        ~?= Right (Circle 3, ParseState "" 1 emptyDifficulty)
    , "cirCle 0" ~: doParse circleP (ParseState "cirCle 0" 1 emptyDifficulty)
        ~?= Right (Circle 0, ParseState "" 1 emptyDifficulty)
    , "cIrcLe -3" ~: doParse circleP (ParseState "cIrcLe -3" 1 emptyDifficulty)
        ~?= Right (Circle (-3), ParseState "" 1 emptyDifficulty)
    , "CIRCLE 100" ~: doParse circleP (ParseState "CIRCLE 100" 1 emptyDifficulty)
        ~?= Right (Circle 100, ParseState "" 1 emptyDifficulty)
    , "circle 3.15" ~: assertIsFatalError
        (doParse circleP (ParseState "Circle 3.15" 1 emptyDifficulty))
        "Expected a fatal error for invalid decimal radius"
    , "CIRCLE 3 4 5" ~: doParse circleP (ParseState "CIRCLE 3 4 5" 1 emptyDifficulty)
        ~?= Right (Circle 3, ParseState "4 5" 1 emptyDifficulty)
    , "CIRCLE 3 4" ~: doParse circleP (ParseState "CIRCLE 3 4" 1 emptyDifficulty)
        ~?= Right (Circle 3, ParseState "4" 1 emptyDifficulty)
    , "circle a" ~: assertIsFatalError
        (doParse circleP (ParseState "circle a" 1 emptyDifficulty))
        "Expected a fatal error for invalid input"
    ]
testRectanglePIgnoreCase :: Test
testRectanglePIgnoreCase = TestList
    [ "Rectangle with positive dimensions" ~:
        doParse rectangleP (ParseState "Rectangle 3 4" 1 emptyDifficulty)
        ~?= Right (Rectangle 3 4, ParseState "" 1 emptyDifficulty)
    , "Rectangle with zero dimensions" ~:
        doParse rectangleP (ParseState "ReCtAnGlE 0 0" 1 emptyDifficulty)
        ~?= Right (Rectangle 0 0, ParseState "" 1 emptyDifficulty)
    , "Rectangle with mixed dimensions" ~:
        doParse rectangleP (ParseState "rEctAngLe -3 4" 1 emptyDifficulty)
        ~?= Right (Rectangle (-3) 4, ParseState "" 1 emptyDifficulty)
    , "Rectangle with trailing input" ~:
        doParse rectangleP (ParseState "RECTANGLE 3 4 5 6 7" 1 emptyDifficulty)
        ~?= Right (Rectangle 3 4, ParseState "5 6 7" 1 emptyDifficulty)
    , "Rectangle with missing dimension" ~: assertIsFatalError
        (doParse rectangleP (ParseState "rectangle 3" 1 emptyDifficulty))
        "Expected a fail error for missing dimension"
    , "Rectangle with invalid input" ~: assertIsFatalError
        (doParse rectangleP (ParseState "rectangle a 4" 1 emptyDifficulty))
        "Expected a fatal error for invalid input"
    ]
testDiamondPIgnoreCase :: Test
testDiamondPIgnoreCase = TestList
    [ "Diamond with positive radius" ~:
        doParse diamondP (ParseState "Diamond 3" 1 emptyDifficulty)
        ~?= Right (Diamond 3, ParseState "" 1 emptyDifficulty)
    , "Diamond with zero radius" ~:
        doParse diamondP (ParseState "DIAMOND 0" 1 emptyDifficulty)
        ~?= Right (Diamond 0, ParseState "" 1 emptyDifficulty)
    , "Diamond with negative radius" ~:
        doParse diamondP (ParseState "dIaMoNd -3" 1 emptyDifficulty)
        ~?= Right (Diamond (-3), ParseState "" 1 emptyDifficulty)
    , "Diamond with trailing input" ~:
        doParse diamondP (ParseState "DIAMOND 3 4" 1 emptyDifficulty)
        ~?= Right (Diamond 3, ParseState "4" 1 emptyDifficulty)
    , "Diamond with invalid input" ~: assertIsFatalError
        (doParse diamondP (ParseState "diamond a" 1 emptyDifficulty))
        "Expected a fatal error for invalid input"
    ]

testEffectRangeP :: Test
testEffectRangeP = TestList
    [ "testEffectRangeP valid Circle" ~: doParse effectRangeP (ParseState "cirCle 3" 1 emptyDifficulty)
          ~?= Right (Circle 3, ParseState "" 1 emptyDifficulty)
    ,"testEffectRangeP invalid Circle 1" ~: assertIsFatalError
        (doParse effectRangeP (ParseState "Circle 3.15" 1 emptyDifficulty))
        "Expected a fatal error for invalid decimal radius"
    , "testEffectRangeP valid Rectangle" ~: doParse effectRangeP (ParseState "Rectangle 3 4" 1 emptyDifficulty)
          ~?= Right (Rectangle 3 4, ParseState "" 1 emptyDifficulty)
    , "testEffectRangeP valid Diamond" ~: doParse effectRangeP (ParseState "Diamond 5" 1 emptyDifficulty)
          ~?= Right (Diamond 5, ParseState "" 1 emptyDifficulty)
    , "testEffectRangeP valid Arbitrary" ~: doParse effectRangeP (ParseState "Arbitrary [(1,2), (:,:)]" 1 emptyDifficulty)
          ~?= Right (Arbitrary [(Coordinate 1, Coordinate 2), (All, All)], ParseState "" 1 emptyDifficulty)
    , "testEffectRangeP invalid Arbitrary" ~: assertIsFatalError
        (doParse effectRangeP (ParseState "Arbitrary [(1.3,2), (:,:)]" 1 emptyDifficulty))
        "Expected a fatal error for unrecognized effect range type"
    , "Unrecognized" ~: assertIsFatalError
        (doParse effectRangeP (ParseState "InvalidRange 7" 1 emptyDifficulty))
        "Unrecognized effect range type"
    ]


allUnitTests :: Test
allUnitTests = TestList
    [ testIntP
    , testBetween
    , testSepBy
    , testSepBy1
    , testString
    , testStringP
    , testParens
    , testBrackets
    , testConstP
    , testCoordinateP
    , testCoordinatePairP
    , testCoordinateListP
    , testCirclePIgnoreCase
    , testRectanglePIgnoreCase
    , testDiamondPIgnoreCase
    , testArbitraryPIgnoreCase
    , testEffectRangeP
    ]