module Candy where

import Data.Char
import Control.Applicative
import Parser
import Data.Either (rights)
import Data.Functor (void)

-- Data Types
type CandyShape = String
type CandyEffect = String

data Coordinate = Coordinate Int | All
    deriving (Show, Eq)

type CoordinatePair = (Coordinate, Coordinate)


data Candy = Candy
    { candyDef :: CandyDefinition
    , candyEffect :: Effect
    } | EmptyCandy deriving (Show, Eq)


data EffectRange
    = Circle Int
    | Rectangle Int Int
    | Diamond Int
    | Arbitrary [CoordinatePair]
    deriving (Show, Eq)

data Operator = Eq | Gt | Ge | Lt | Le deriving (Show, Eq)
data EffectRequirement = EffectRequirement Operator Int deriving (Show, Eq)
operatorMapping :: [(String, Operator)]
operatorMapping =
  [ (">=", Ge)
  , (">", Gt)
  , ("<=", Le)
  , ("<", Lt)
  , ("=", Eq)
  ]
allowedEffectReqOperators :: [Operator]
allowedEffectReqOperators = [Eq, Gt, Ge]


data Effect = Effect
    { effectName        :: String
    , effectRange       :: EffectRange
    , effectRequirement :: EffectRequirement
    , effectDescription :: String
    } deriving (Show, Eq)

data CandyDefinition = CandyDefinition
    { shapeName     :: String
    , shapeIcon     :: String
    , effectNameRef :: String
    } deriving (Show, Eq)

-- -- Parsers
-- operatorP :: Parser Operator
-- operatorP = (string ">=" *> pure Ge) <|> (string ">" *> pure Gt) <|> (string "=" *> pure Eq)

-- requirementP :: Parser Requirement
-- requirementP = do
--     wsP $ pure ()
--     op <- operatorP <|> pure Eq
--     n <- wsP intP
--     return $ Requirement op n

-- effectRangeP :: Parser EffectRange
-- effectRangeP =
--     wsP $ choice [try circleP, try rectangleP, try diamondP, try arbitraryP]

-- circleP :: Parser EffectRange
-- circleP = do
--     wsP (string "Circle")
--     radius <- wsP intP
--     return $ Circle radius

-- rectangleP :: Parser EffectRange
-- rectangleP = do
--     wsP (string "Rectangle")
--     width <- wsP intP
--     height <- wsP intP
--     return $ Rectangle width height

-- diamondP :: Parser EffectRange
-- diamondP = do
--     wsP (string "Diamond")
--     radius <- wsP intP
--     return $ Diamond radius

-- arbitraryP :: Parser EffectRange
-- arbitraryP = do
--     wsP (string "Arbitrary")
--     offsets <- between (wsP (char '[')) (sepBy coordP (wsP (char ','))) (wsP (char ']'))
--     return $ Arbitrary offsets

-- coordP :: Parser CoordinatePair
-- coordP = do
--     wsP (char '(')
--     x <- coordinateP
--     wsP (char ',')
--     y <- coordinateP
--     wsP (char ')')
--     return (x, y)

-- coordinateP :: Parser Coordinate
-- coordinateP = (Coordinate <$> wsP intP) <|> (wsP (char ':') *> pure All)

-- effectP :: Parser Effect
-- effectP = do
--     wsP (string "effect_name:")
--     name <- wsP $ many (satisfy (/= '\n'))
--     wsP (string "effect_range:")
--     range <- wsP effectRangeP
--     wsP (string "effect_requirement:")
--     req <- wsP requirementP
--     wsP (string "effect_description:")
--     desc <- wsP $ many (satisfy (/= '\n'))
--     return $ Effect name range req desc

-- candyP :: Parser CandyDefinition
-- candyP = do
--     wsP (string "shape_name:")
--     name <- wsP $ many (satisfy (/= '\n'))
--     wsP (string "shape_icon:")
--     icon <- wsP $ many (satisfy (/= '\n'))
--     wsP (string "effect_name:")
--     effectRef <- wsP $ many (satisfy (/= '\n'))
--     return $ CandyDefinition name icon effectRef

-- lookForHeaderOrEOF :: Parser ()
-- lookForHeaderOrEOF = do
--     many (wsP (commentP <|> emptyLineP))
--     wsP $ pure ()
--     (void (string "effect_name:" <|> string "shape_name:")) <|> eof

-- effectWithRecovery :: Parser (Either String Effect)
-- effectWithRecovery =
--     (Right <$> try effectP)
--     <|> (Left <$> manyTill anyChar (lookAhead lookForHeaderOrEOF <|> eof))

-- candyWithRecovery :: Parser (Either String CandyDefinition)
-- candyWithRecovery =
--     (Right <$> try candyP)
--     <|> (Left <$> manyTill anyChar (lookAhead lookForHeaderOrEOF <|> eof))



-- fileP :: Parser ([Effect], [CandyDefinition])
-- fileP = do
--     wsP $ many (commentP <|> emptyLineP)
--     effects <- many $ effectWithRecovery <* wsP (many (commentP <|> emptyLineP))
--     candies <- many $ candyWithRecovery <* wsP (many (commentP <|> emptyLineP))
--     return (rights effects, rights candies)

-- commentP :: Parser ()
-- commentP = wsP $ do
--     string "//"
--     many (satisfy (/= '\n'))
--     char '\n'
--     return ()

-- emptyLineP :: Parser ()
-- emptyLineP = wsP $ do
--     many (satisfy isSpace)
--     char '\n'
--     return ()

-- main :: IO ()
-- main = do
--     content <- readFile "src/candies.txt"
--     case parse fileP content of
--         Left err -> putStrLn $ "Parse Error: " ++ err
--         Right (effects, candies) -> do
--             putStrLn "Parsing Successful:"
--             putStrLn "Effects Parsed:"
--             mapM_ print effects
--             putStrLn "\nCandies Parsed:"
--             mapM_ print candies











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
-- -- 解析器类型和错误定义
-- data Position = Position {
--     line :: Int,
--     column :: Int
-- } deriving (Show)

-- data ParseState = ParseState {
--     input :: String,
--     position :: Position
-- } deriving (Show)

-- data ParseError = ParseError {
--     errorMsg :: String,
--     errorPos :: Position
-- } deriving (Show)

-- -- 修改后的 Parser 类型
-- newtype Parser a = P { doParse :: ParseState -> Either ParseError (a, ParseState) }

-- -- 更新位置函数
-- updatePos :: Position -> Char -> Position
-- updatePos (Position l c) ch = case ch of
--     '\n' -> Position (l + 1) 1
--     _    -> Position l (c + 1)

-- -- 基础解析器和组合子
-- instance Functor Parser where
--     fmap f p = P $ \s -> case doParse p s of
--         Left err      -> Left err
--         Right (a, s') -> Right (f a, s')

-- instance Applicative Parser where
--     pure x = P $ \s -> Right (x, s)
--     p1 <*> p2 = P $ \s -> case doParse p1 s of
--         Left err      -> Left err
--         Right (f, s') -> case doParse p2 s' of
--             Left err       -> Left err
--             Right (a, s'') -> Right (f a, s'')

-- instance Monad Parser where
--     return = pure
--     p >>= f = P $ \s -> case doParse p s of
--         Left err      -> Left err
--         Right (a, s') -> doParse (f a) s'

-- instance Alternative Parser where
--     empty = P $ \s -> Left (ParseError "Parse failed" (position s))
--     p1 <|> p2 = P $ \s -> case doParse p1 s of
--         Left _  -> doParse p2 s
--         res     -> res

-- -- 解析器基本函数
-- failParser :: String -> Parser a
-- failParser msg = P $ \s -> Left (ParseError msg (position s))

-- get :: Parser Char
-- get = P $ \s -> case input s of
--     (c:cs) -> Right (c, s { input = cs, position = updatePos (position s) c })
--     []     -> Left (ParseError "Unexpected EOF" (position s))

-- satisfy :: (Char -> Bool) -> Parser Char
-- satisfy p = do
--     c <- get
--     if p c then return c else failParser ("Unexpected character: " ++ [c])

-- char :: Char -> Parser Char
-- char c = satisfy (== c) <|> failParser ("Expected character: " ++ [c])

-- string :: String -> Parser String
-- string ""     = return ""
-- string (c:cs) = do
--     char c
--     string cs
--     return (c:cs)

-- wsP :: Parser a -> Parser a
-- wsP p = many (satisfy isSpace) *> p <* many (satisfy isSpace)


-- -- | Combine all parsers in the list (sequentially)
-- choice :: [Parser a] -> Parser a
-- choice = asum -- equivalent to: foldr (<|>) empty

-- -- | @between open close p@ parses @open@, followed by @p@ and finally
-- --   @close@. Only the value of @p@ is pureed.
-- between :: Parser open -> Parser a -> Parser close -> Parser a
-- between open p close = open *> p <* close

-- try :: Parser a -> Parser a
-- try p = P $ \s -> case doParse p s of
--     Left _  -> Left (ParseError "Parse failed" (position s))
--     result  -> result

-- expectString :: String -> Parser ()
-- expectString str = wsP (void $ string str) <|> failParser ("Expected '" ++ str ++ "'")

-- -- | @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
-- --   Returns a list of values returned by @p@.
-- sepBy :: Parser a -> Parser sep -> Parser [a]
-- sepBy p sep = sepBy1 p sep <|> pure []

-- -- | @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
-- --   Returns a list of values returned by @p@.
-- sepBy1 :: Parser a -> Parser sep -> Parser [a]
-- sepBy1 p sep = (:) <$> p <*> many (sep *> p)

-- intP :: Parser Int
-- intP = do
--     sign <- optional (char '-')
--     digits <- some (satisfy isDigit)
--     let numStr = maybe "" (:[]) sign ++ digits
--     return $ read numStr

-- -- 定义解析器
-- operatorP :: Parser Operator
-- operatorP = (string ">=" *> pure Ge)
--     <|> (string ">" *> pure Gt)
--     <|> (string "=" *> pure Eq)
--     <|> failParser "Expected operator (>=, >, or =)"

-- requirementP :: Parser Requirement
-- requirementP = do
--     op <- operatorP <|> pure Eq
--     n <- wsP intP
--     return $ Requirement op n

-- effectRangeP :: Parser EffectRange
-- effectRangeP = wsP $ choice
--     [ try circleP
--     , try rectangleP
--     , try diamondP
--     , try arbitraryP
--     ] <|> failParser "Expected effect_range"

-- circleP :: Parser EffectRange
-- circleP = do
--     wsP (string "Circle")
--     radius <- wsP intP
--     return $ Circle radius

-- rectangleP :: Parser EffectRange
-- rectangleP = do
--     wsP (string "Rectangle")
--     width <- wsP intP
--     height <- wsP intP
--     return $ Rectangle width height

-- diamondP :: Parser EffectRange
-- diamondP = do
--     wsP (string "Diamond")
--     radius <- wsP intP
--     return $ Diamond radius

-- arbitraryP :: Parser EffectRange
-- arbitraryP = do
--     wsP (string "Arbitrary")
--     offsets <- between (wsP (char '[')) (sepBy coordP (wsP (char ','))) (wsP (char ']'))
--     return $ Arbitrary offsets

-- coordinateP :: Parser Coordinate
-- coordinateP = (Coordinate <$> wsP intP) <|> (wsP (char ':') *> pure All)

-- coordP :: Parser CoordinatePair
-- coordP = do
--     wsP (char '(')
--     x <- coordinateP
--     wsP (char ',')
--     y <- coordinateP
--     wsP (char ')')
--     return (x, y)

-- effectP :: Parser Effect
-- effectP = do
--     expectString "effect_name:"
--     name <- wsP $ many (satisfy (/= '\n'))
--     expectString "effect_range:"
--     range <- wsP effectRangeP
--     expectString "effect_requirement:"
--     req <- wsP requirementP
--     expectString "effect_description:"
--     desc <- wsP $ many (satisfy (/= '\n'))
--     return $ Effect name range req desc

-- candyP :: Parser CandyDefinition
-- candyP = do
--     expectString "shape_name:"
--     name <- wsP $ many (satisfy (/= '\n'))
--     expectString "shape_icon:"
--     icon <- wsP $ many (satisfy (/= '\n'))
--     expectString "effect_name:"
--     effectRef <- wsP $ many (satisfy (/= '\n'))
--     return $ CandyDefinition name icon effectRef

-- skipMany :: Parser a -> Parser ()
-- skipMany p = void (many p)

-- -- 修改后的 emptyLineP
-- emptyLineP :: Parser ()
-- emptyLineP = do
--     skipMany (satisfy isSpace)  -- 跳过空白字符
--     void (char '\n')            -- 必须匹配一个换行符

-- -- 修改后的 commentP
-- commentP :: Parser ()
-- commentP = do
--     skipMany (satisfy isSpace)  -- 跳过空白字符
--     void (string "//")          -- 匹配注释开始符
--     skipMany (satisfy (/= '\n')) -- 跳过注释内容
--     optional (char '\n')        -- 可选匹配换行符
--     return ()

-- -- 修改后的 many 解析器
-- manyP :: Parser a -> Parser [a]
-- manyP p = P $ \s -> case doParse p s of
--     Left _          -> Right ([], s)  -- 解析失败，返回空列表
--     Right (a, s')   -> case doParse (manyP p) s' of
--         Right (as, s'') -> Right (a:as, s'')  -- 解析成功，递归处理
--         Left _          -> Right ([a], s')   -- 不应发生，但以防止边界问题

-- -- 文件解析器
-- fileP :: Parser ([Effect], [CandyDefinition])
-- fileP = do
--     wsP $ many (commentP <|> emptyLineP)  -- 跳过注释和空行
--     effects <- many $ effectP <* wsP (many (commentP <|> emptyLineP))
--     candies <- many $ candyP <* wsP (many (commentP <|> emptyLineP))
--     return (effects, candies)

-- -- 顶级解析函数
-- parseFile :: String -> Either ParseError ([Effect], [CandyDefinition])
-- parseFile content = do
--     let initialState = ParseState content (Position 1 1)
--     case doParse fileP initialState of
--         Left err       -> Left err
--         Right (res, _) -> Right res

-- -- 测试解析并打印错误
-- tCandy :: IO ()
-- tCandy = do
--     content <- readFile "src/candies.txt"
--     case parseFile content of
--         Left (ParseError msg pos) -> do
--             putStrLn $ "Error at line " ++ show (line pos) ++ ", column " ++ show (column pos)
--             putStrLn $ "Error message: " ++ msg
--         Right (effects, candies)  -> do
--             putStrLn "Parsed Effects:"
--             mapM_ print effects
--             putStrLn "\nParsed Candies:"
--             mapM_ print candies


-- -- 测试用例字符串
-- basicCorrect :: String
-- basicCorrect = unlines
--     [ "// A simple test case with one effect and one candy"
--     , "effect_name: Bomb"
--     , "effect_range: Rectangle 3 3"
--     , "effect_requirement: 5"
--     , "effect_description: Explodes candies in a 3x3 area"
--     , ""
--     , "shape_name: Circle"
--     , "shape_icon: ●"
--     , "effect_name: Bomb"
--     ]

-- missingField :: String
-- missingField = unlines
--     [ "effect_name: Bomb"
--     , "effect_range: Rectangle 3 3"
--     , "effect_requirement: 5"
--     , "// Missing effect_description"
--     , ""
--     , "shape_name: Circle"
--     , "shape_icon: ●"
--     , "effect_name: Bomb"
--     ]

-- invalidFormat :: String
-- invalidFormat = unlines
--     [ "effect_name Bomb  // Missing colon after effect_name"
--     , "effect_range: Rectangle 3 3"
--     , "effect_requirement: 5"
--     , "effect_description: Explodes candies in a 3x3 area"
--     ]

-- withCommentsAndBlankLines :: String
-- withCommentsAndBlankLines = unlines
--     [ "// This is a test case with comments and blank lines"
--     , ""
--     , "effect_name: Bomb"
--     , "effect_range: Rectangle 3 3"
--     , "effect_requirement: 5"
--     , "effect_description: Explodes candies in a 3x3 area"
--     , ""
--     , "// Another effect"
--     , "effect_name: CircleBomb"
--     , "effect_range: Circle 2"
--     , "effect_requirement: >3"
--     , "effect_description: Explodes candies in a circular area"
--     ]

-- emptyFile :: String
-- emptyFile = ""

-- onlyComments :: String
-- onlyComments = unlines
--     [ "// This file only contains comments"
--     , "// No effects or candies defined"
--     ]

-- -- 测试函数
-- testParser :: String -> String -> IO ()
-- testParser testName content = do
--     putStrLn $ "\nRunning test: " ++ testName
--     case parseFile content of
--         Left (ParseError msg pos) -> do
--             putStrLn $ "Error at line " ++ show (line pos) ++ ", column " ++ show (column pos)
--             putStrLn $ "Error message: " ++ msg
--         Right (effects, candies) -> do
--             putStrLn "Parsed Effects:"
--             mapM_ print effects
--             putStrLn "\nParsed Candies:"
--             mapM_ print candies

-- -- 测试入口
-- tt :: IO ()
-- tt = do
--     testParser "Basic Correct Input" basicCorrect
--     testParser "Missing Field" missingField
--     testParser "Invalid Format" invalidFormat
--     testParser "With Comments and Blank Lines" withCommentsAndBlankLines
--     testParser "Empty File" emptyFile
--     testParser "Only Comments" onlyComments