
module CandyCrushParser1 where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List (find, isPrefixOf)
import Text.Read (readMaybe)

import Candy
-- Difficulty level
data Difficulty = Difficulty
    { dimension :: Int
    , candies   :: [Candy]
    , maxSteps  :: Int
    } deriving (Show, Eq)

data ParseDifficulty = ParseDifficulty
    { pdDimension :: Maybe Int
    , pdMaxSteps  :: Maybe Int
    , pdEffects   :: [Effect]
    , pdCandies   :: [Candy]
    } deriving (Show, Eq)

data ParseState = ParseState
    { input          :: String
    , lineNum        :: Int
    , parseDifficulty :: ParseDifficulty
    } deriving (Show, Eq)

data ParseError = ParseError
    { errorMsg     :: String
    , errorLineNum :: Int
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
        Left _  -> doParse p2 s
        res     -> res

failParser :: String -> Parser a
failParser msg = P $ \s ->
    let context = take 10 (input s) -- 限制上下文字符长度
    in Left (ParseError (msg ++ " Context: '" ++ context ++ "'") (lineNum s))

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
    digits <- some (satisfy isDigit) <|> failParser ("Invalid integer at line " ++ show lineNum)
    let numStr = maybe "" (:[]) sign ++ digits
    case readMaybe numStr of
        Just n  -> return n
        Nothing -> failParser ("Invalid integer: " ++ numStr ++ " at line " ++ show lineNum)


operatorP :: Parser Operator
operatorP = do
    lineNum <- getLineNum
    (string ">=" *> pure Ge)
        <|> (string ">" *> pure Gt)
        <|> (string "=" *> pure Eq)
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

getLineNum :: Parser Int
getLineNum = P $ \s -> Right (lineNum s, s)

commentP :: Parser ()
commentP = do
    wsP $ string "//"
    void $ many (satisfy (/= '\n'))
    void $ optional (char '\n')

emptyLineP :: Parser ()
emptyLineP = void $ many (satisfy isSpace) *> char '\n'

skipOptionalWhitespace :: Parser ()
skipOptionalWhitespace = void $ many (commentP <|> emptyLineP)

strip :: String -> String
strip = f . f
   where f = reverse . dropWhile isSpace

getField :: Parser a -> String -> Int -> Parser a
getField parser fieldName lineNum = parser <|> failParser ("Invalid or missing " ++ fieldName ++ " at line " ++ show lineNum)

getParseState :: Parser ParseState
getParseState = P $ \s -> Right (s, s)

putParseState :: ParseState -> Parser ()
putParseState s = P $ \_ -> Right ((), s)

getParseDifficulty :: Parser ParseDifficulty
getParseDifficulty = P $ \s -> Right (parseDifficulty s, s)

modifyParseDifficulty :: (ParseDifficulty -> ParseDifficulty) -> Parser ()
modifyParseDifficulty f = P $ \s -> Right ((), s { parseDifficulty = f (parseDifficulty s) })

difficultyConstantP :: Parser ()
difficultyConstantP = do
    expectString "DIFFICULTY_CONSTANT"
    lineNum <- getLineNum
    dim <- getField (expectString "dimension:" *> wsP intP) "dimension" lineNum
    steps <- getField (expectString "max_steps:" *> wsP intP) "max_steps" lineNum
    modifyParseDifficulty $ \pd -> pd { pdDimension = Just dim, pdMaxSteps = Just steps }

effectP :: Parser ()
effectP = do
    lineNum <- getLineNum
    name <- getField (expectString "effect_name:" *> wsP (some (satisfy (/= '\n')))) "effect_name" lineNum
    range <- getField (expectString "effect_range:" *> effectRangeP) "effect_range" (lineNum + 1)
    req <- getField (expectString "effect_requirement:" *> requirementP) "effect_requirement" (lineNum + 2)
    desc <- getField (expectString "effect_description:" *> wsP (some (satisfy (/= '\n')))) "effect_description" (lineNum + 3)
    parseDifficulty <- getParseDifficulty
    if any (\e -> effectName e == name) (pdEffects parseDifficulty)
        then failParser ("Effect already defined: " ++ name ++ " at line " ++ show lineNum)
        else do
            let effect = Effect name range req desc
            modifyParseDifficulty $ \pd -> pd { pdEffects = pdEffects pd ++ [effect] }


candyP :: Parser ()
candyP = do
    lineNum <- getLineNum
    name <- getField (expectString "shape_name:" *> wsP (some (satisfy (/= '\n')))) "shape_name" lineNum
    icon <- getField (expectString "shape_icon:" *> wsP (some (satisfy (/= '\n')))) "shape_icon" lineNum
    effectRef <- getField (expectString "effect_name:" *> wsP (some (satisfy (/= '\n')))) "effect_name" lineNum
    parseDifficulty <- getParseDifficulty
    let effectNameRefStripped = strip effectRef
    case find (\e -> strip (effectName e) == effectNameRefStripped) (pdEffects parseDifficulty) of
        Just effect -> do
            let def = CandyDefinition name icon effectRef
            let candy = Candy def effect
            modifyParseDifficulty $ \pd -> pd { pdCandies = pdCandies pd ++ [candy] }
        Nothing -> failParser $ "Effect not found: " ++ effectRef ++ " at line " ++ show lineNum


-- 修改 itemP
itemP :: Parser ()
itemP = do
    skipOptionalWhitespace
    line <- lookahead (takeLineP 30)  -- 提取当前行的前 x 个字符，用于匹配判断
    if "DIFFICULTY_CONSTANT" `isPrefixOf` line
        then difficultyConstantP
        else if "effect_name:" `isPrefixOf` line
            then effectP
            else if "shape_name:" `isPrefixOf` line
                then candyP
                else failParser "Unexpected item. Expected one of: 'DIFFICULTY_CONSTANT', 'effect_name:', or 'shape_name:'"

lookahead :: Parser a -> Parser a
lookahead p = P $ \s -> case doParse p s of
    Left err -> Left err
    Right (result, _) -> Right (result, s)
takeLineP :: Int -> Parser String
takeLineP n = P $ \s ->
    let lineContent = take n (input s)
    in Right (lineContent, s)


-- 修改 fileP
fileP :: Parser ()
fileP = do
    skipOptionalWhitespace
    some itemP -- 让 some 自然处理 itemP 的失败
    skipOptionalWhitespace
    ensureEOF


ensureEOF :: Parser ()
ensureEOF = P $ \s ->
    if null (input s)
        then Right ((), s)
        else Left (ParseError ("Unexpected content: " ++ take 20 (input s)) (lineNum s))


parseFile :: String -> Either String Difficulty
parseFile content =
    let initialState = ParseState content 1 (ParseDifficulty Nothing Nothing [] [])
    in case doParse fileP initialState of
        Left (ParseError msg lineNum) ->
            Left $ "Parse Error at line " ++ show lineNum ++ ": " ++ msg
        Right (_, finalState) -> 
            let pd = parseDifficulty finalState
            in validateParseDifficulty pd

validateParseDifficulty :: ParseDifficulty -> Either String Difficulty
validateParseDifficulty pd = case (pdDimension pd, pdMaxSteps pd) of
    (Nothing, _) -> Left "Parse Error: 'dimension' is missing from the configuration"
    (_, Nothing) -> Left "Parse Error: 'max_steps' is missing from the configuration"
    (Just dim, Just steps) -> 
        Right $ Difficulty dim (pdCandies pd) steps


testCandyTxt :: IO ()
testCandyTxt = txtFilePTest "src/candies.txt"

testSmallTxt :: IO ()
testSmallTxt = txtFilePTest "src/small.txt"

txtFilePTest :: String -> IO ()
txtFilePTest fileName = do
    content <- readFile fileName
    testFileP content
    
txtEffectPTest :: String -> IO ()
txtEffectPTest fileName = do
    content <- readFile fileName
    testEffectP content

testInput1 :: String
testInput1 = unlines
    [ "effect_name: invalid effect"
    , "effect_range: Arbitrary [(0, :), (:, 0)]"
    , "effect_requirement: --5"
    , "effect_description: When matched, clears the entire row and column."
    ]

testInput2 :: String
testInput2 = unlines
    [ "effect_name: RowClear"
    , "effect_range: Arbitrary [(0, :), (:, 0)]"
    , "effect_requirement: >= 5"
    , "effect_description: When matched, clears the entire row and column."
    ,  "effect_name: invalid effect"
    , "effect_range: Arbitrary [(0, :), (:, 0)]"
    , "effect_requirement: --5"
    , "effect_description: When matched, clears the entire row and column."
    ]



testFileP :: String -> IO ()
testFileP input = do
    -- Initialize parse state
    let initialState = ParseState input 1 (ParseDifficulty Nothing Nothing [] [])
    -- Run the parser
    let result = doParse fileP initialState
    -- Print the result
    putStrLn "Result of fileP:"
    case result of
        Left (ParseError msg lineNum) ->
            putStrLn $ "Parsing failed at line " ++ show lineNum ++ ": " ++ msg
        Right (_, finalState) -> do
            let parsedDifficulty = parseDifficulty finalState
            case validateParseDifficulty parsedDifficulty of
                Left err -> putStrLn $ "Validation failed: " ++ err
                Right difficulty -> putStrLn $ "Parsed Difficulty:\n" ++ show difficulty



-- Helper function to parse with `effectP` and print the result
testEffectP :: String -> IO ()
testEffectP input = do
    let initialState = ParseState input 1 (ParseDifficulty Nothing Nothing [] [])
    let result = doParse (effectP >> getParseDifficulty) initialState
    putStrLn "Result of effectP:"
    case result of
        Left (ParseError msg lineNum) ->
            putStrLn $ "Parsing failed at line " ++ show lineNum ++ ": " ++ msg
        Right (parsedDifficulty, _) ->
            putStrLn $ "Parsed Effects:\n" ++ show (pdEffects parsedDifficulty)

-- Function to test `some itemP` and print state
testSomeItemP :: String -> IO ()
testSomeItemP input = do
    -- Initialize parse state
    let initialState = ParseState input 1 (ParseDifficulty Nothing Nothing [] [])
    -- Run the parser
    let result = doParse (some itemP) initialState
    -- Print the result
    putStrLn "Result of some itemP:"
    case result of
        Left (ParseError msg lineNum) -> do
            putStrLn $ "Parsing failed at line " ++ show lineNum ++ ": " ++ msg
            -- Print the state at the time of failure
            putStrLn "State at failure:"
            case doParse (some itemP >> getParseState) initialState of
                Left err -> print initialState
                Right _         -> return ()
        Right (parsedItems, finalState) -> do
            putStrLn $ "Parsed Items Count: " ++ show (length parsedItems)
            putStrLn "Final State:"
            print finalState
