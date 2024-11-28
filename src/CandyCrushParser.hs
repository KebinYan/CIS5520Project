
module CandyCrushParser where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Either (lefts, rights)
import Data.List (isPrefixOf, stripPrefix, isInfixOf, find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import Text.Read (readMaybe)

import Candy


-- Difficulty level
data Difficulty = Difficulty
    { dimension :: Int
    , candies   :: [Candy]
    , maxSteps  :: Int
    } deriving (Show, Eq)
-- 解析器类型和错误定义
data Position = Position {
    line :: Int,
    column :: Int
} deriving (Show)

data ParseState = ParseState {
    input :: String,
    position :: Position
} deriving (Show)

data ParseError = ParseError {
    errorMsg :: String,
    errorPos :: Position
} deriving (Show)

-- 修改后的 Parser 类型
newtype Parser a = P { doParse :: ParseState -> Either ParseError (a, ParseState) }

-- 更新位置函数
updatePos :: Position -> Char -> Position
updatePos (Position l c) ch = case ch of
    '\n' -> Position (l + 1) 1
    _    -> Position l (c + 1)

-- 基础解析器和组合子
instance Functor Parser where
    fmap f p = P $ \s -> case doParse p s of
        Left err      -> Left err
        Right (a, s') -> Right (f a, s')

instance Applicative Parser where
    pure x = P $ \s -> Right (x, s)
    p1 <*> p2 = P $ \s -> case doParse p1 s of
        Left err      -> Left err
        Right (f, s') -> case doParse p2 s' of
            Left err       -> Left err
            Right (a, s'') -> Right (f a, s'')

instance Monad Parser where
    return = pure
    p >>= f = P $ \s -> case doParse p s of
        Left err      -> Left err
        Right (a, s') -> doParse (f a) s'

instance Alternative Parser where
    empty = P $ \s -> Left (ParseError "Parse failed" (position s))
    p1 <|> p2 = P $ \s -> case doParse p1 s of
        Left _  -> doParse p2 s
        res     -> res

-- 解析器基本函数
failParser :: String -> Parser a
failParser msg = P $ \s -> Left (ParseError msg (position s))

get :: Parser Char
get = P $ \s -> case input s of
    (c:cs) -> Right (c, s { input = cs, position = updatePos (position s) c })
    []     -> Left (ParseError "Unexpected EOF" (position s))

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
    c <- get
    if p c then return c else failParser ("Unexpected character: " ++ [c])

char :: Char -> Parser Char
char c = satisfy (== c) <|> failParser ("Expected character: " ++ [c])

string :: String -> Parser String
string ""     = return ""
string (c:cs) = do
    char c
    string cs
    return (c:cs)

getPosition :: Parser Position
getPosition = P $ \s -> Right (position s, s)

wsP :: Parser a -> Parser a
wsP p = many (satisfy isSpace) *> p <* many (satisfy isSpace)


-- | Combine all parsers in the list (sequentially)
choice :: [Parser a] -> Parser a
choice = asum -- equivalent to: foldr (<|>) empty

-- | @between open close p@ parses @open@, followed by @p@ and finally
--   @close@. Only the value of @p@ is pureed.
between :: Parser open -> Parser a -> Parser close -> Parser a
between open p close = open *> p <* close

try :: Parser a -> Parser a
try p = P $ \s -> case doParse p s of
    Left _  -> Left (ParseError "Parse failed" (position s))
    result  -> result

expectString :: String -> Parser ()
expectString str = wsP (void $ string str) <|> failParser ("Expected '" ++ str ++ "'")

-- | @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

intP :: Parser Int
intP = do
    sign <- optional (char '-')
    digits <- some (satisfy isDigit)
    let numStr = maybe "" (:[]) sign ++ digits
    return $ read numStr

-- 定义解析器
operatorP :: Parser Operator
operatorP = (string ">=" *> pure Ge)
    <|> (string ">" *> pure Gt)
    <|> (string "=" *> pure Eq)
    <|> failParser "Expected operator (>=, >, or =)"

requirementP :: Parser Requirement
requirementP = do
    op <- operatorP <|> pure Eq
    n <- wsP intP
    return $ Requirement op n

effectRangeP :: Parser EffectRange
effectRangeP = wsP $ choice
    [ try circleP
    , try rectangleP
    , try diamondP
    , try arbitraryP
    ] <|> failParser "Expected effect_range"

circleP :: Parser EffectRange
circleP = do
    wsP (string "Circle")
    radius <- wsP intP
    return $ Circle radius

rectangleP :: Parser EffectRange
rectangleP = do
    wsP (string "Rectangle")
    width <- wsP intP
    height <- wsP intP
    return $ Rectangle width height

diamondP :: Parser EffectRange
diamondP = do
    wsP (string "Diamond")
    radius <- wsP intP
    return $ Diamond radius

arbitraryP :: Parser EffectRange
arbitraryP = do
    wsP (string "Arbitrary")
    offsets <- between (wsP (char '[')) (sepBy coordP (wsP (char ','))) (wsP (char ']'))
    return $ Arbitrary offsets

coordinateP :: Parser Coordinate
coordinateP = (Coordinate <$> wsP intP) <|> (wsP (char ':') *> pure All)

coordP :: Parser CoordinatePair
coordP = do
    wsP (char '(')
    x <- coordinateP
    wsP (char ',')
    y <- coordinateP
    wsP (char ')')
    return (x, y)

effectP :: Parser Effect
effectP = do
    expectString "effect_name:"
    name <- wsP $ many (satisfy (/= '\n'))
    expectString "effect_range:"
    range <- wsP effectRangeP
    expectString "effect_requirement:"
    req <- wsP requirementP
    expectString "effect_description:"
    desc <- wsP $ many (satisfy (/= '\n'))
    return $ Effect name range req desc

candyP :: Parser (CandyDefinition, Position)
candyP = do
    pos <- getPosition  -- 获取当前解析位置
    expectString "shape_name:"
    name <- wsP $ many (satisfy (/= '\n'))
    expectString "shape_icon:"
    icon <- wsP $ many (satisfy (/= '\n'))
    expectString "effect_name:"
    effectRef <- wsP $ many (satisfy (/= '\n'))
    return (CandyDefinition name icon effectRef, pos)

skipMany :: Parser a -> Parser ()
skipMany p = void (many p)

commentP :: Parser ()
commentP = do
    skipMany (satisfy isSpace)
    void (string "//")
    comment <- many (satisfy (/= '\n'))
    pos <- getPosition
    void $ optional (char '\n') 
    return ()

emptyLineP :: Parser ()
emptyLineP = do
    skipMany (satisfy isSpace)
    void (char '\n')
    return ()

-- 修改后的 many 解析器
manyP :: Parser a -> Parser [a]
manyP p = P $ \s -> case doParse p s of
    Left _          -> Right ([], s)  -- 解析失败，返回空列表
    Right (a, s')   -> case doParse (manyP p) s' of
        Right (as, s'') -> Right (a:as, s'')  -- 解析成功，递归处理
        Left _          -> Right ([a], s')   -- 不应发生，但以防止边界问题

-- DIFFICULTY_CONSTANT 解析器
difficultyConstantP :: Parser (Int, Int)
difficultyConstantP = do
    expectString "DIFFICULTY_CONSTANT"
    expectString "dimension:"
    dim <- wsP intP
    expectString "max_steps:"
    steps <- wsP intP
    return (dim, steps)

fileP :: Parser (Either String Difficulty)
fileP = do
    wsP $ many (commentP <|> emptyLineP)  -- 跳过注释和空行
    (dim, steps) <- difficultyConstantP <* wsP (many (commentP <|> emptyLineP))
    effects <- many $ effectP <* wsP (many (commentP <|> emptyLineP))
    candies <- many $ candyP <* wsP (many (commentP <|> emptyLineP))
    ensureEOF
    let candyResults = map (candyFromDefinition effects) candies
    return $ case sequence candyResults of
        Left err -> Left err
        Right candyObjects -> Right $ Difficulty dim candyObjects steps

-- 确保解析到文件末尾
ensureEOF :: Parser ()
ensureEOF = do
    remaining <- wsP (many get)
    unless (null remaining) $
        failParser $ "Unexpected content: " ++ take 20 remaining

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

candyFromDefinition :: [Effect] -> (CandyDefinition, Position) -> Either String Candy
candyFromDefinition effects (CandyDefinition name icon effectNameRef, pos) =
    let effectNameRefStripped = strip effectNameRef
    in case find (\e -> strip (effectName e) == effectNameRefStripped) effects of
        Just effect -> Right $ Candy (CandyDefinition name icon effectNameRef) effect
        Nothing -> Left $ "Effect not found: " ++ effectNameRef
                        ++ " at line " ++ show (line pos) ++ ", column " ++ show (column pos)

parseFile :: String -> Either String Difficulty
parseFile content =
    case doParse fileP (ParseState content (Position 1 1)) of
        Left (ParseError msg pos) ->
            Left $ "Parse Error at line " ++ show (line pos) ++ ", column " ++ show (column pos) ++ ": " ++ msg
        Right (Left semanticErr, _) -> Left $ "Semantic Error: " ++ semanticErr
        Right (Right difficulty, _) -> Right difficulty

testCandyTxt :: IO ()
testCandyTxt = do
    content <- readFile "src/candies.txt"
    case parseFile content of
        Left err -> do
            putStrLn "Parsing failed:"
            putStrLn err
        Right difficulty -> do
            putStrLn "Parsed Difficulty:"
            print difficulty


basicCorrect :: String
basicCorrect = unlines
    ["// basic correct input"   
        , "DIFFICULTY_CONSTANT"
        , "dimension: 5"
        , "max_steps: 50"
        , ""
        , "effect_name: Bomb"
        , "effect_range: Rectangle 3 3"
        , "effect_requirement: 5"
        , "effect_description: Explodes candies in a 3x3 area"
        , ""
        , "shape_name: Circle"
        , "shape_icon: ●"
        , "effect_name: Bomb"
    ]

missingField :: String
missingField = unlines
    [ "effect_name: Bomb"
    , "effect_range: Rectangle 3 3"
    , "effect_requirement: 5"
    , "// Missing effect_description"
    , ""
    , "shape_name: Circle"
    , "shape_icon: ●"
    , "effect_name: Bomb"
    ]

invalidFormat :: String
invalidFormat = unlines
    [ "effect_name Bomb  // Missing colon after effect_name"
    , "effect_range: Rectangle 3 3"
    , "effect_requirement: 5"
    , "effect_description: Explodes candies in a 3x3 area"
    ]

withCommentsAndBlankLines :: String
withCommentsAndBlankLines = unlines
    [ "// This is a test case with comments and blank lines"
    , ""
    , "effect_name: Bomb"
    , "effect_range: Rectangle 3 3"
    , "effect_requirement: 5"
    , "effect_description: Explodes candies in a 3x3 area"
    , ""
    , "// Another effect"
    , "effect_name: CircleBomb"
    , "effect_range: Circle 2"
    , "effect_requirement: >3"
    , "effect_description: Explodes candies in a circular area"
    ]

emptyFile :: String
emptyFile = ""

onlyComments :: String
onlyComments = unlines
    [ "// This file only contains comments"
    , "// No effects or candies defined"
    ]

testParser :: String -> String -> IO ()
testParser testName content = do
    putStrLn $ "\nRunning test: " ++ testName
    case parseFile content of
        Left err -> putStrLn $ "Error: " ++ err
        Right difficulty -> do
            putStrLn "Parsed Difficulty:"
            print difficulty

-- 测试入口
tt :: IO ()
tt = do
    testParser "Basic Correct Input" basicCorrect
    testParser "Missing Field" missingField
    testParser "Invalid Format" invalidFormat
    testParser "With Comments and Blank Lines" withCommentsAndBlankLines
    testParser "Empty File" emptyFile
    testParser "Only Comments" onlyComments
















-- module CandyCrushParser where

-- import Control.Applicative
-- import Control.Monad
-- import Data.Char
-- import Data.Either (lefts, rights)
-- import Data.List (isPrefixOf, stripPrefix, isInfixOf)
-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map
-- import Text.Printf (printf)
-- import Text.Read (readMaybe)

-- import Candy

-- -- Difficulty level
-- data Difficulty = Difficulty
--     { dimension :: Int
--     , candies   :: [Candy]
--     , maxSteps  :: Int
--     } deriving (Show, Eq)

-- -- Simplified Parser type
-- newtype Parser a = Parser { parse :: [String] -> Int -> Either String (a, [String], Int) }

-- -- Functor instance
-- instance Functor Parser where
--     fmap f p = Parser $ \input lineNum -> case parse p input lineNum of
--         Left err           -> Left err
--         Right (a, rest, l) -> Right (f a, rest, l)

-- -- Applicative instance
-- instance Applicative Parser where
--     pure x = Parser $ \input lineNum -> Right (x, input, lineNum)
--     pf <*> pa = Parser $ \input lineNum -> case parse pf input lineNum of
--         Left err           -> Left err
--         Right (f, rest, l) -> case parse pa rest l of
--             Left err'            -> Left err'
--             Right (a, rest', l') -> Right (f a, rest', l')

-- -- Monad instance
-- instance Monad Parser where
--     return = pure
--     p >>= f = Parser $ \input lineNum -> case parse p input lineNum of
--         Left err           -> Left err
--         Right (a, rest, l) -> parse (f a) rest l

-- -- Alternative instance
-- instance Alternative Parser where
--     empty = Parser $ \_ lineNum -> Left (printf "Parsing failed at line %d" lineNum)
--     p1 <|> p2 = Parser $ \input lineNum -> case parse p1 input lineNum of
--         Left _  -> parse p2 input lineNum
--         result  -> result

-- -- Utility functions

-- -- Consume the next non-empty, non-comment line
-- nextLine :: Parser String
-- nextLine = do
--     skipCommentsAndEmptyLines
--     Parser $ \input lineNum ->
--         case input of
--             [] -> Left (printf "Unexpected end of input at line %d" lineNum)
--             (l:ls) -> Right (l, ls, lineNum + 1)

-- -- Look ahead without consuming
-- peekLine :: Parser String
-- peekLine = Parser $ \input lineNum ->
--     case input of
--         [] -> Left (printf "Unexpected end of input at line %d" lineNum)
--         (l:_) -> Right (l, input, lineNum)

-- -- Parse key-value pair with arbitrary spaces around the colon
-- parseKeyValue :: String -> Parser String
-- parseKeyValue expectedKey = do
--     line <- nextLine
--     case wordsByColon line of
--         [key, value]
--             | trim key == expectedKey -> return $ trim value
--             | otherwise -> Parser $ \_ lineNum ->
--                 Left (printf "Expected key '%s' at line %d, but got '%s'" expectedKey lineNum line)
--         _ -> Parser $ \_ lineNum ->
--             Left (printf "Malformed key-value pair at line %d: '%s'" lineNum line)

-- -- Helper to split by colon with arbitrary spaces
-- wordsByColon :: String -> [String]
-- wordsByColon str =
--     let (key, rest) = break (== ':') str
--     in if null rest
--         then [trim key]
--         else [trim key, trim (drop 1 rest)]

-- -- Helper functions for line parsing
-- type LineParser a = String -> Maybe (a, String)

-- -- Run LineParser on a line
-- runLineParser :: LineParser a -> String -> Maybe (a, String)
-- runLineParser p  = p

-- -- Parse key (non-space, non-colon characters)
-- parseKey :: LineParser String
-- parseKey s = Just (span (\c -> not (isSpace c) && c /= ':') s)

-- -- Skip spaces
-- skipSpaces :: LineParser ()
-- skipSpaces s = Just ((), dropWhile isSpace s)

-- -- Parse a specific character
-- char :: Char -> LineParser Char
-- char c (x:xs) | x == c = Just (c, xs)
-- char _ _ = Nothing

-- -- Parse rest of the line
-- parseRest :: LineParser String
-- parseRest s = Just (s, "")

-- -- Trim whitespace
-- trim :: String -> String
-- trim = f . f
--    where f = reverse . dropWhile isSpace

-- -- Parse integer value
-- parseIntValue :: String -> Parser Int
-- parseIntValue key = do
--     valueStr <- parseKeyValue key
--     case reads valueStr of
--         [(n, "")] -> return n
--         _         -> Parser $ \_ lineNum -> Left (printf "Invalid integer for key '%s' at line %d" key lineNum)

-- -- Parse requirement
-- parseRequirement :: String -> Parser Requirement
-- parseRequirement str =
--     let parts = words str
--     in case parts of
--         [numStr] -> case reads numStr of
--             [(n, "")] -> return $ Requirement Eq n
--             _         -> Parser $ \_ lineNum -> Left (printf "Invalid number in effect_requirement '%s' at line %d" str lineNum)
--         [opStr, numStr] -> case (parseOperatorStr opStr, reads numStr) of
--             (Right op, [(n, "")]) -> return $ Requirement op n
--             (Left err, _)         -> Parser $ \_ lineNum -> Left (printf "%s at line %d" err lineNum)
--             (_, _)                -> Parser $ \_ lineNum -> Left (printf "Invalid number '%s' in effect_requirement at line %d" numStr lineNum)
--         _ -> Parser $ \_ lineNum -> Left (printf "Invalid format for effect_requirement '%s' at line %d" str lineNum)

-- -- Parse operator as a helper function
-- parseOperatorStr :: String -> Either String Operator
-- parseOperatorStr ">"  = Right Gt
-- parseOperatorStr ">=" = Right Ge
-- parseOperatorStr "="  = Right Eq
-- parseOperatorStr ""   = Right Eq -- Default to Eq
-- parseOperatorStr op   = Left $ printf "Invalid operator '%s'" op

-- -- parseCoordinate function
-- parseCoordinate :: String -> Either String Coordinate
-- parseCoordinate ":" = Right All
-- parseCoordinate s = case reads s of
--     [(n, "")] -> Right (Coordinate n)
--     _         -> Left $ printf "Invalid coordinate '%s'" s

-- -- Parse effect range
-- parseEffectRange :: String -> Parser EffectRange
-- parseEffectRange str =
--     let (rangeType, param) = break isSpace str
--         paramTrimmed = trim (drop 1 param) -- 去掉前导空格
--     in case rangeType of
--         "Circle"    -> parseCircleRange paramTrimmed
--         "Rectangle" -> parseRectangleRange paramTrimmed
--         "Diamond"   -> parseDiamondRange paramTrimmed
--         "Arbitrary" -> parseArbitraryRange paramTrimmed
--         _           -> Parser $ \_ lineNum -> Left (printf "Invalid rangeType '%s' in effect_range at line %d" rangeType lineNum)

-- parseCircleRange :: String -> Parser EffectRange
-- parseCircleRange param = Parser $ \_ lineNum ->
--     case reads param of
--         [(n, "")] -> Right (Circle n, [], lineNum)
--         _         -> Left (printf "Invalid radius '%s' for Circle at line %d" param lineNum)

-- parseRectangleRange :: String -> Parser EffectRange
-- parseRectangleRange param = Parser $ \_ lineNum ->
--     case words param of
--         [wStr, hStr] -> case (reads wStr, reads hStr) of
--             ([(w, "")], [(h, "")]) -> Right (Rectangle w h, [], lineNum)
--             _                      -> Left (printf "Invalid width or height '%s' for Rectangle at line %d" param lineNum)
--         _ -> Left (printf "Rectangle requires two parameters but got '%s' at line %d" param lineNum)

-- parseDiamondRange :: String -> Parser EffectRange
-- parseDiamondRange param = Parser $ \_ lineNum ->
--     case reads param of
--         [(n, "")] -> Right (Diamond n, [], lineNum)
--         _         -> Left (printf "Invalid radius '%s' for Diamond at line %d" param lineNum)

-- -- parseArbitraryRange function
-- parseArbitraryRange :: String -> Parser EffectRange
-- parseArbitraryRange param = Parser $ \_ lineNum ->
--     let content = trim param
--     in if not (null content) && head content == '[' && last content == ']'
--         then
--             let offsetsStr = init (tail content) -- 去掉中括号
--                 offsetsList = splitOffsets offsetsStr
--             in case mapM parseCoordinatePairHelper offsetsList of
--                 Right offsets -> Right (Arbitrary offsets, [], lineNum)
--                 Left err      -> Left $ printf "Invalid offsets in Arbitrary at line %d: %s" lineNum err
--         else Left $ printf "Arbitrary requires a list of offsets but got '%s' at line %d" param lineNum

-- -- Helper function to extract coordinate pairs
-- extractCoordinatePairs :: String -> [String]
-- extractCoordinatePairs s = go s []
--   where
--     go "" acc = reverse acc
--     go str acc =
--         case dropWhile (/= '(') str of
--             "" -> reverse acc
--             ('(' : rest) ->
--                 let (pair, rest') = span (/= ')') rest
--                     rest'' = dropWhile (/= '(') rest'
--                 in if null rest' || head rest' /= ')'
--                     then reverse acc -- no closing parenthesis
--                     else go rest'' (('(' : pair ++ ")") : acc)
--             _ -> reverse acc

-- -- Helper function to parse individual coordinate pairs
-- parseCoordinatePairHelper :: String -> Either String CoordinatePair
-- parseCoordinatePairHelper str =
--     let s = trim str
--     in if not (null s) && head s == '(' && last s == ')'
--         then
--             let content = init (tail s) -- 去掉括号
--                 (xStrRaw, rest) = break (== ',') content -- 按逗号分割
--             in if null rest
--                 then Left $ printf "Malformed coordinate pair '%s'" str
--                 else
--                     let xStr = trim xStrRaw
--                         yStr = trim (drop 1 rest) -- 去掉逗号
--                     in case (parseCoordinate xStr, parseCoordinate yStr) of
--                         (Right x, Right y) -> Right (x, y)
--                         (Left err, _)      -> Left $ printf "Invalid x coordinate '%s': %s" xStr err
--                         (_, Left err)      -> Left $ printf "Invalid y coordinate '%s': %s" yStr err
--         else Left $ printf "Malformed coordinate pair '%s'" str

-- -- Updated splitOffsets function
-- splitOffsets :: String -> [String]
-- splitOffsets s = splitOffsets' s 0 "" []
--   where
--     splitOffsets' :: String -> Int -> String -> [String] -> [String]
--     splitOffsets' "" _ current acc = if null (trim current) then reverse acc else reverse (trim current : acc)
--     splitOffsets' (c:cs) depth current acc
--         | c == ',' && depth == 0 = splitOffsets' cs depth "" (trim current : acc)
--         | c == '(' = splitOffsets' cs (depth + 1) (current ++ [c]) acc
--         | c == ')' = splitOffsets' cs (depth - 1) (current ++ [c]) acc
--         | otherwise = splitOffsets' cs depth (current ++ [c]) acc

-- skipCommentsAndEmptyLines :: Parser ()
-- skipCommentsAndEmptyLines = Parser $ \input lineNum ->
--     let rest = dropWhile (\l -> isComment l || isEmpty l) input
--         skippedLines = length input - length rest
--     in Right ((), rest, lineNum + skippedLines)
--   where
--     isComment line = "//" `isPrefixOf` dropWhile isSpace line
--     isEmpty line = null (words line)

-- isEndOfInput :: Parser Bool
-- isEndOfInput = Parser $ \input lineNum ->
--     if null input
--         then Right (True, input, lineNum)
--         else Right (False, input, lineNum)

-- -- Parse Candy
-- parseCandy :: Parser CandyDefinition
-- parseCandy = do
--     name <- parseKeyValue "shape_name"
--     icon <- parseKeyValue "shape_icon"
--     effectRef <- parseKeyValue "effect_name"
--     return $ CandyDefinition name icon effectRef

-- -- Parse difficulty constants
-- parseDifficultyConstants :: Parser (Int, Int)
-- parseDifficultyConstants = do
--     skipCommentsAndEmptyLines
--     keyword "DIFFICULTY_CONSTANT"
--     dimension <- parseIntValue "dimension"
--     maxSteps  <- parseIntValue "max_steps"
--     return (dimension, maxSteps)

-- -- Parse specific keyword
-- keyword :: String -> Parser ()
-- keyword kw = do
--     line <- nextLine
--     let lineWords = words line
--     if kw `elem` lineWords
--         then return ()
--         else Parser $ \_ lineNum -> Left (printf "Expected keyword '%s' at line %d, but got '%s'" kw lineNum line)

-- -- Parse a single effect
-- parseEffect :: Parser Effect
-- parseEffect = do
--     name <- parseKeyValue "effect_name"
--     rangeStr <- parseKeyValue "effect_range"
--     range <- parseEffectRange rangeStr
--     reqStr <- parseKeyValue "effect_requirement"
--     req <- parseRequirement reqStr
--     desc <- parseKeyValue "effect_description"
--     return $ Effect name range req desc

-- parseEffects :: Map String Effect -> Parser (Map String Effect)
-- parseEffects effects = do
--     skipCommentsAndEmptyLines
--     endOfInput <- isEndOfInput
--     if endOfInput
--         then return effects
--         else do
--             effect <- parseEffect
--             let effectName' = effectName effect
--             if Map.member effectName' effects
--                 then Parser $ \_ lineNum -> Left (printf "Duplicate effect_name '%s' at line %d" effectName' lineNum)
--                 else parseEffects (Map.insert effectName' effect effects)

-- -- Parse all candies
-- parseCandies :: Map String Effect -> [Candy] -> Parser [Candy]
-- parseCandies effects candies = do
--     skipCommentsAndEmptyLines
--     result <- peekLine
--     if "shape_name" `isInfixOf` result
--         then do
--             candyDef <- parseCandy
--             let effectRef = effectNameRef candyDef
--             case Map.lookup effectRef effects of
--                 Just eff -> parseCandies effects (candies ++ [Candy candyDef eff])
--                 Nothing  -> Parser $ \_ lineNum -> Left (printf "Undefined effect_name '%s' at line %d" effectRef lineNum)
--         else do
--             Parser $ \input lineNum ->
--                 if null input
--                     then Right (candies, input, lineNum)
--                     else Left (printf "Unexpected content '%s' at line %d" (head input) lineNum)

-- -- Main parser
-- parseConfig :: Parser Difficulty
-- parseConfig = do
--     (dim, steps) <- parseDifficultyConstants
--     effects <- parseEffects Map.empty
--     candies <- parseCandies effects []
--     return $ Difficulty dim candies steps

-- -- Entry point
-- runParser :: String -> Either String Difficulty
-- runParser content = case parse parseConfig (lines content) 1 of
--     Left err          -> Left err
--     Right (result, _, _) -> Right result

-- main :: IO ()
-- main = do
--     content <- readFile "src/Candies.txt"
--     case runParser content of
--         Left err -> putStrLn $ "Error: " ++ err
--         Right difficulty -> print difficulty

-- testParseCoordinatePair :: IO ()
-- testParseCoordinatePair = do
--     let pairs = ["(0, 0)", "(:, 0)", "(0, :)"]
--     forM_ pairs $ \pair -> case parseCoordinatePairHelper pair of
--         Left err  -> putStrLn $ "Error: " ++ err
--         Right res -> print res

-- testParseRequirement :: IO ()
-- testParseRequirement = do
--     let requirements = ["0", "> 5", ">= 10", "5"]
--     forM_ requirements $ \req -> case parse (parseRequirement req) [] 1 of
--         Left err      -> putStrLn $ "Error: " ++ err
--         Right (res, _, _) -> print res

-- testParseEffectRange :: IO ()
-- testParseEffectRange = do
--     let ranges =
--             [ "Circle 2"
--             , "Rectangle 3 4"
--             , "Diamond 3"
--             , "Arbitrary [(0, 0), (0, :), (:, 0)]"
--             , "InvalidType param"
--             , "Circle abc"
--             , "Rectangle 3 abc"
--             , "Arbitrary [(0, 0), (:)]"
--             ]
--     forM_ ranges $ \range -> case parse (parseEffectRange range) [] 1 of
--         Left err      -> putStrLn $ "Error: " ++ err
--         Right (res, _, _) -> print res

-- testParseEffect2 :: IO ()
-- testParseEffect2 = do
--     let effects =
--             unlines
--                 [ "effect_name: Normal"
--                 , "effect_range: Arbitrary [(0,0)]"
--                 , "effect_requirement: 0"
--                 , "effect_description: Normal candies that can be matched with other normal candies."
--                 , ""
--                 , "effect_name: StripedRow"
--                 , "effect_range: Arbitrary [(0, :)]"
--                 , "effect_requirement: 4"
--                 , "effect_description: When matched, clears the entire row."
--                 , ""
--                 ]
--     case parse (parseEffects Map.empty) (lines effects) 1 of
--         Left err -> putStrLn $ "Error: " ++ err
--         Right (res, _, _) -> mapM_ (putStrLn . showEffect) (Map.toList res)

-- showEffect :: (String, Effect) -> String
-- showEffect (name, effect) =
--     "Effect Name: " ++ name ++ "\n" ++
--     "Effect: " ++ show effect ++ "\n"
