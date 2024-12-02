module CandyCrushParser where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List (find)
import Text.Read (readMaybe)

import Candy

-- Difficulty level
data Difficulty = Difficulty
    { dimension :: Int
    , candies   :: [Candy]
    , maxSteps  :: Int
    } deriving (Show, Eq)

data ParseState = ParseState {
    input :: String,
    lineNum :: Int
} deriving (Show, Eq)

data ParseError = ParseError {
    errorMsg :: String,
    errorLineNum :: Int
} deriving (Show, Eq)

-- Parser 类型
newtype Parser a = P { doParse :: ParseState -> Either ParseError (a, ParseState) }

-- 更新行号函数
updateLineNum :: Int -> Char -> Int
updateLineNum lineNum ch = if ch == '\n' then lineNum + 1 else lineNum

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
    empty = P $ \s -> Left (ParseError "Parse failed" (lineNum s))
    p1 <|> p2 = P $ \s -> case doParse p1 s of
        Left err1 -> case doParse p2 s of
            Left _  -> Left err1 
            res     -> res      
        res     -> res          

-- 基础解析器函数
failParser :: String -> Parser a
failParser msg = P $ \s -> Left (ParseError msg (lineNum s))

get :: Parser Char
get = P $ \s -> case input s of
    (c:cs) -> let newLineNum = updateLineNum (lineNum s) c
              in Right (c, s { input = cs, lineNum = newLineNum })
    []     -> Left (ParseError "Unexpected EOF" (lineNum s))

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
    c <- get
    if p c then return c else failParser ("Unexpected character: " ++ [c])

char :: Char -> Parser Char
char c = satisfy (== c) <|> failParser ("Expected character: " ++ [c])

string :: String -> Parser String
string = traverse char

wsP :: Parser a -> Parser a
wsP p = many (satisfy isSpace) *> p <* many (satisfy isSpace)

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

between :: Parser open -> Parser a -> Parser close -> Parser a
between open p close = open *> p <* close

expectString :: String -> Parser ()
expectString str = do
    lineNum <- getLineNum
    result <- optional (void (wsP (string str)))
    case result of
        Just () -> return ()
        Nothing -> failParser ("Expected '" ++ str ++ "' at line " ++ show lineNum)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = (p `sepBy1` sep) <|> pure []

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

intP :: Parser Int
intP = do
    lineNum <- getLineNum
    sign <- optional (char '-')
    digitsResult <- some (satisfy isDigit) <|> failParser ("Invalid integer at line " ++ show lineNum)
    let numStr = maybe "" (:[]) sign ++ digitsResult
    case readMaybe numStr of
        Just n  -> return n
        Nothing -> failParser ("Invalid integer: " ++ numStr ++ " at line " ++ show lineNum)

operatorP :: Parser Operator
operatorP = do
    lineNum <- getLineNum
    (string ">=" *> pure Ge)
        <|> (char '>' *> pure Gt)
        <|> (char '=' *> pure Eq)
        <|> failParser ("Invalid operator in requirement at line " ++ show lineNum)

requirementP :: Parser Requirement
requirementP = do
    lineNum <- getLineNum
    op <- operatorP <|> failParser ("Invalid operator in effect_requirement at line " ++ show lineNum)
    val <- wsP intP <|> failParser ("Invalid integer in effect_requirement at line " ++ show lineNum)
    return $ Requirement op val

effectRangeP :: Parser EffectRange
effectRangeP = wsP $ choice
    [ arbitraryP
    , circleP
    , rectangleP
    , diamondP
    ] <|> failParser "Expected effect_range"

circleP :: Parser EffectRange
circleP = do
    expectString "Circle"
    radius <- wsP intP <|> failParser "Invalid radius for Circle"
    return $ Circle radius

rectangleP :: Parser EffectRange
rectangleP = do
    expectString "Rectangle"
    width <- wsP intP <|> failParser "Invalid width for Rectangle"
    height <- wsP intP <|> failParser "Invalid height for Rectangle"
    return $ Rectangle width height

diamondP :: Parser EffectRange
diamondP = do
    expectString "Diamond"
    size <- wsP intP <|> failParser "Invalid size for Diamond"
    return $ Diamond size

arbitraryP :: Parser EffectRange
arbitraryP = do
    expectString "Arbitrary"
    lineNum <- getLineNum
    _ <- wsP (char '[') <|> failParser ("Expected '[' at line " ++ show lineNum)
    offsets <- coordP `sepBy1` wsP (char ',') <|> failParser ("Invalid coordinate list at line " ++ show lineNum)
    _ <- wsP (char ']') <|> failParser ("Expected ']' at line " ++ show lineNum)
    return $ Arbitrary offsets

coordinateP :: Parser Coordinate
coordinateP = do
    lineNum <- getLineNum
    wsP (Coordinate <$> intP)
        <|> (wsP (char ':') *> pure All)
        <|> failParser ("Expected coordinate (integer or ':') at line " ++ show lineNum)

coordP :: Parser CoordinatePair
coordP = between (char '(') pair (char ')')
  where
    pair = do
        x <- coordinateP
        wsP $ char ','
        y <- coordinateP
        return (x, y)

effectP :: Parser Effect
effectP = do
    (_, name) <- getFieldWithLineNum (expectString "effect_name:" *> wsP (some (satisfy (/= '\n')))) "effect_name"
    (_, range) <- getFieldWithLineNum (expectString "effect_range:" *> effectRangeP) "effect_range"
    (_, req) <- getFieldWithLineNum (expectString "effect_requirement:" *> requirementP) "effect_requirement"
    (_, desc) <- getFieldWithLineNum (expectString "effect_description:" *> wsP (some (satisfy (/= '\n')))) "effect_description"
    return $ Effect name range req desc

candyP :: Parser (CandyDefinition, Int)
candyP = do
    lineNum <- getLineNum
    (_, name) <- getFieldWithLineNum (expectString "shape_name:" *> wsP (some (satisfy (/= '\n')))) "shape_name"
    (_, icon) <- getFieldWithLineNum (expectString "shape_icon:" *> wsP (some (satisfy (/= '\n')))) "shape_icon"
    (_, effectRef) <- getFieldWithLineNum (expectString "effect_name:" *> wsP (some (satisfy (/= '\n')))) "effect_name"
    return (CandyDefinition name icon effectRef, lineNum)

getLineNum :: Parser Int
getLineNum = P $ \s -> Right (lineNum s, s)

commentP :: Parser ()
commentP = do
    wsP $ string "//"
    void $ many (satisfy (/= '\n'))
    void $ optional (char '\n')

emptyLineP :: Parser ()
emptyLineP = void $ many (satisfy isSpace) *> char '\n'

getFieldWithLineNum :: Parser a -> String -> Parser (Int, a)
getFieldWithLineNum parser fieldName = do
    lineNum <- getLineNum
    result <- parser <|> failParser ("Invalid or missing " ++ fieldName ++ " at line " ++ show lineNum)
    return (lineNum, result)

-- 修改 difficultyConstantP，遇到错误立即返回
difficultyConstantP :: Parser (Int, Int)
difficultyConstantP = do
    expectString "DIFFICULTY_CONSTANT"
    (_, dim) <- getFieldWithLineNum (expectString "dimension:" *> wsP intP) "dimension"
    (_, steps) <- getFieldWithLineNum (expectString "max_steps:" *> wsP intP) "max_steps"
    return (dim, steps)

parseEffects :: Parser [Effect]
parseEffects = some (effectP <* skipOptionalWhitespace)

parseCandies :: [Effect] -> Parser [Candy]
parseCandies effects = some (candyP <* skipOptionalWhitespace) >>= mapM (candyFromDefinition effects)

fileP :: Parser Difficulty
fileP = do
    skipOptionalWhitespace
    (dim, steps) <- difficultyConstantP
    skipOptionalWhitespace
    effects <- parseEffects
    candies <- parseCandies effects
    skipOptionalWhitespace
    ensureEOF
    return $ Difficulty dim candies steps

skipOptionalWhitespace :: Parser ()
skipOptionalWhitespace = void $ many (commentP <|> emptyLineP)

ensureEOF :: Parser ()
ensureEOF = P $ \s ->
    if null (input s)
        then Right ((), s)
        else Left (ParseError ("Unexpected content: " ++ take 20 (input s)) (lineNum s))

strip :: String -> String
strip = f . f
   where f = reverse . dropWhile isSpace

candyFromDefinition :: [Effect] -> (CandyDefinition, Int) -> Parser Candy
candyFromDefinition effects (def@(CandyDefinition _ _ effectNameRef), lineNum) = do
    let effectNameRefStripped = strip effectNameRef
    case find (\e -> strip (effectName e) == effectNameRefStripped) effects of
        Just effect -> return $ Candy def effect
        Nothing -> failParser $ "Effect not found: " ++ effectNameRef
                             ++ " at line " ++ show lineNum

parseFile :: String -> Either String Difficulty
parseFile content =
    case doParse fileP (ParseState content 1) of
        Left (ParseError msg lineNum) ->
            Left $ "Parse Error at line " ++ show lineNum ++ ": " ++ msg
        Right (difficulty, _) -> Right difficulty

testCandyTxt :: IO ()
testCandyTxt = do
    content <- readFile "src/candies.txt"
    case parseFile content of
        Left err -> putStrLn $ "Parsing failed:\n" ++ err
        Right difficulty -> do
            putStrLn "Parsed Difficulty:"
            print difficulty

testSmallTxt :: IO ()
testSmallTxt = do
    content <- readFile "src/small.txt"
    case parseFile content of
        Left err -> putStrLn $ "Parsing failed:\n" ++ err
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


testInvalidEffectRequirement :: IO ()
testInvalidEffectRequirement = do
    let invalidEffect = unlines
            [ "effect_name: invalid effect"
            , "effect_range: Arbitrary [(0, :), (:, 0)]"
            , "effect_requirement: --5"
            , "effect_description: When matched, clears the entire row and column."
            ]
    let parseResult = doParse effectP (ParseState invalidEffect 1)
    case parseResult of
        Left (ParseError msg lineNum) -> do
            putStrLn "Test: Invalid effect_requirement"
            putStrLn $ "Expected error: Invalid integer in effect_requirement at line 3"
            putStrLn $ "Actual error: " ++ msg ++ " at line " ++ show lineNum
            if msg == "Invalid integer in effect_requirement at line 3" && lineNum == 3
                then putStrLn "Result: PASS"
                else putStrLn "Result: FAIL"
        Right _ -> do
            putStrLn "Test: Invalid effect_requirement"
            putStrLn "Expected error but got successful parse"
            putStrLn "Result: FAIL"

{-

module CandyCrushParser where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List (find)
import Text.Read (readMaybe)

import Candy

-- Difficulty level
data Difficulty = Difficulty
    { dimension :: Int
    , candies   :: [Candy]
    , maxSteps  :: Int
    } deriving (Show, Eq)

data ParseState = ParseState {
    input :: String,
    lineNum :: Int
} deriving (Show, Eq)

data ParseError = ParseError {
    errorMsg :: String,
    errorLineNum :: Int
} deriving (Show, Eq)

-- Parser 类型
newtype Parser a = P { doParse :: ParseState -> Either ParseError (a, ParseState) }

-- 更新行号函数
updateLineNum :: Int -> Char -> Int
updateLineNum lineNum ch = if ch == '\n' then lineNum + 1 else lineNum

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
    empty = P $ \s -> Left (ParseError "Parse failed" (lineNum s))
    p1 <|> p2 = P $ \s -> case doParse p1 s of
        Left err1 -> case doParse p2 s of
            Left _  -> Left err1 
            res     -> res      
        res     -> res          

-- 基础解析器函数
failParser :: String -> Parser a
failParser msg = P $ \s -> Left (ParseError msg (lineNum s))

get :: Parser Char
get = P $ \s -> case input s of
    (c:cs) -> let newLineNum = updateLineNum (lineNum s) c
              in Right (c, s { input = cs, lineNum = newLineNum })
    []     -> Left (ParseError "Unexpected EOF" (lineNum s))

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
    c <- get
    if p c then return c else failParser ("Unexpected character: " ++ [c])

char :: Char -> Parser Char
char c = satisfy (== c) <|> failParser ("Expected character: " ++ [c])

string :: String -> Parser String
string = traverse char

wsP :: Parser a -> Parser a
wsP p = many (satisfy isSpace) *> p <* many (satisfy isSpace)

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

between :: Parser open -> Parser a -> Parser close -> Parser a
between open p close = open *> p <* close

expectString :: String -> Parser ()
expectString str = do
    lineNum <- getLineNum
    result <- optional (void (wsP (string str)))
    case result of
        Just () -> return ()
        Nothing -> failParser ("Expected '" ++ str ++ "' at line " ++ show lineNum)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = (p `sepBy1` sep) <|> pure []

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

intP :: Parser Int
intP = do
    lineNum <- getLineNum
    sign <- optional (char '-')
    digitsResult <- some (satisfy isDigit) <|> failParser ("Invalid integer at line " ++ show lineNum)
    let numStr = maybe "" (:[]) sign ++ digitsResult
    case readMaybe numStr of
        Just n  -> return n
        Nothing -> failParser ("Invalid integer: " ++ numStr ++ " at line " ++ show lineNum)

operatorP :: Parser Operator
operatorP = do
    lineNum <- getLineNum
    (string ">=" *> pure Ge)
        <|> (char '>' *> pure Gt)
        <|> (char '=' *> pure Eq)
        <|> failParser ("Invalid operator in requirement at line " ++ show lineNum)

requirementP :: Parser Requirement
requirementP = do
    lineNum <- getLineNum
    op <- operatorP <|> failParser ("Invalid operator in effect_requirement at line " ++ show lineNum)
    val <- wsP intP <|> failParser ("Invalid integer in effect_requirement at line " ++ show lineNum)
    return $ Requirement op val

effectRangeP :: Parser EffectRange
effectRangeP = wsP $ choice
    [ arbitraryP
    , circleP
    , rectangleP
    , diamondP
    ] <|> failParser "Expected effect_range"

circleP :: Parser EffectRange
circleP = do
    expectString "Circle"
    radius <- wsP intP <|> failParser "Invalid radius for Circle"
    return $ Circle radius

rectangleP :: Parser EffectRange
rectangleP = do
    expectString "Rectangle"
    width <- wsP intP <|> failParser "Invalid width for Rectangle"
    height <- wsP intP <|> failParser "Invalid height for Rectangle"
    return $ Rectangle width height

diamondP :: Parser EffectRange
diamondP = do
    expectString "Diamond"
    size <- wsP intP <|> failParser "Invalid size for Diamond"
    return $ Diamond size

arbitraryP :: Parser EffectRange
arbitraryP = do
    expectString "Arbitrary"
    lineNum <- getLineNum
    _ <- wsP (char '[') <|> failParser ("Expected '[' at line " ++ show lineNum)
    offsets <- coordP `sepBy1` wsP (char ',') <|> failParser ("Invalid coordinate list at line " ++ show lineNum)
    _ <- wsP (char ']') <|> failParser ("Expected ']' at line " ++ show lineNum)
    return $ Arbitrary offsets

coordinateP :: Parser Coordinate
coordinateP = do
    lineNum <- getLineNum
    wsP (Coordinate <$> intP)
        <|> (wsP (char ':') *> pure All)
        <|> failParser ("Expected coordinate (integer or ':') at line " ++ show lineNum)

coordP :: Parser CoordinatePair
coordP = between (char '(') pair (char ')')
  where
    pair = do
        x <- coordinateP
        wsP $ char ','
        y <- coordinateP
        return (x, y)

effectP :: Parser Effect
effectP = do
    (_, name) <- getFieldWithLineNum (expectString "effect_name:" *> wsP (some (satisfy (/= '\n')))) "effect_name"
    (_, range) <- getFieldWithLineNum (expectString "effect_range:" *> effectRangeP) "effect_range"
    (_, req) <- getFieldWithLineNum (expectString "effect_requirement:" *> requirementP) "effect_requirement"
    (_, desc) <- getFieldWithLineNum (expectString "effect_description:" *> wsP (some (satisfy (/= '\n')))) "effect_description"
    return $ Effect name range req desc

candyP :: Parser (CandyDefinition, Int)
candyP = do
    lineNum <- getLineNum
    (_, name) <- getFieldWithLineNum (expectString "shape_name:" *> wsP (some (satisfy (/= '\n')))) "shape_name"
    (_, icon) <- getFieldWithLineNum (expectString "shape_icon:" *> wsP (some (satisfy (/= '\n')))) "shape_icon"
    (_, effectRef) <- getFieldWithLineNum (expectString "effect_name:" *> wsP (some (satisfy (/= '\n')))) "effect_name"
    return (CandyDefinition name icon effectRef, lineNum)

getLineNum :: Parser Int
getLineNum = P $ \s -> Right (lineNum s, s)

commentP :: Parser ()
commentP = do
    wsP $ string "//"
    void $ many (satisfy (/= '\n'))
    void $ optional (char '\n')

emptyLineP :: Parser ()
emptyLineP = void $ many (satisfy isSpace) *> char '\n'

getFieldWithLineNum :: Parser a -> String -> Parser (Int, a)
getFieldWithLineNum parser fieldName = do
    lineNum <- getLineNum
    result <- parser <|> failParser ("Invalid or missing " ++ fieldName ++ " at line " ++ show lineNum)
    return (lineNum, result)

-- 修改 difficultyConstantP，遇到错误立即返回
difficultyConstantP :: Parser (Int, Int)
difficultyConstantP = do
    expectString "DIFFICULTY_CONSTANT"
    (_, dim) <- getFieldWithLineNum (expectString "dimension:" *> wsP intP) "dimension"
    (_, steps) <- getFieldWithLineNum (expectString "max_steps:" *> wsP intP) "max_steps"
    return (dim, steps)

parseEffects :: Parser [Effect]
parseEffects = some (effectP <* skipOptionalWhitespace)

parseCandies :: [Effect] -> Parser [Candy]
parseCandies effects = some (candyP <* skipOptionalWhitespace) >>= mapM (candyFromDefinition effects)

fileP :: Parser Difficulty
fileP = do
    skipOptionalWhitespace
    (dim, steps) <- difficultyConstantP
    skipOptionalWhitespace
    effects <- parseEffects
    candies <- parseCandies effects
    skipOptionalWhitespace
    ensureEOF
    return $ Difficulty dim candies steps

skipOptionalWhitespace :: Parser ()
skipOptionalWhitespace = void $ many (commentP <|> emptyLineP)

ensureEOF :: Parser ()
ensureEOF = P $ \s ->
    if null (input s)
        then Right ((), s)
        else Left (ParseError ("Unexpected content: " ++ take 20 (input s)) (lineNum s))

strip :: String -> String
strip = f . f
   where f = reverse . dropWhile isSpace

candyFromDefinition :: [Effect] -> (CandyDefinition, Int) -> Parser Candy
candyFromDefinition effects (def@(CandyDefinition _ _ effectNameRef), lineNum) = do
    let effectNameRefStripped = strip effectNameRef
    case find (\e -> strip (effectName e) == effectNameRefStripped) effects of
        Just effect -> return $ Candy def effect
        Nothing -> failParser $ "Effect not found: " ++ effectNameRef
                             ++ " at line " ++ show lineNum

parseFile :: String -> Either String Difficulty
parseFile content =
    case doParse fileP (ParseState content 1) of
        Left (ParseError msg lineNum) ->
            Left $ "Parse Error at line " ++ show lineNum ++ ": " ++ msg
        Right (difficulty, _) -> Right difficulty

testCandyTxt :: IO ()
testCandyTxt = do
    content <- readFile "src/candies.txt"
    case parseFile content of
        Left err -> putStrLn $ "Parsing failed:\n" ++ err
        Right difficulty -> do
            putStrLn "Parsed Difficulty:"
            print difficulty


-}