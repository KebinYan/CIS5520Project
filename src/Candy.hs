module Candy where

import Data.Char
import Control.Applicative
import Parser

-- Data Types
type CandyShape = String
type CandyEffect = String

data Coordinate = Coordinate Int | All  -- All means entire row or column
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
    | Arbitrary [CoordinatePair]  -- Use CoordinatePair here
    deriving (Show, Eq)

data Operator = Eq | Gt | Ge deriving (Show, Eq)

data Requirement = Requirement Operator Int deriving (Show, Eq)

data Effect = Effect
    { effectName        :: String
    , effectRange       :: EffectRange
    , effectRequirement :: Requirement
    , effectDescription :: String
    } deriving (Show, Eq)

normalEffect :: Effect
normalEffect = Effect "Normal" (Arbitrary [(Coordinate 0, Coordinate 0)]) (Requirement Eq 0) "No special effect"

data CandyDefinition = CandyDefinition
    { shapeName     :: String
    , shapeIcon     :: String
    , effectNameRef :: String
    } deriving (Show, Eq)

-- Parsers

operatorP :: Parser Operator
operatorP =
    (string ">=" *> pure Ge)
    <|> (string ">"  *> pure Gt)
    <|> (string "="  *> pure Eq)

requirementP :: Parser Requirement
requirementP = do
    wsP $ pure ()  -- Consume any leading whitespace
    op <- operatorP <|> pure Eq
    n <- wsP intP
    return $ Requirement op n

effectRangeP :: Parser EffectRange
effectRangeP = wsP $ choice
    [ circleP
    , rectangleP
    , diamondP
    , arbitraryP
    ]

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

coordP :: Parser CoordinatePair
coordP = do
    wsP (char '(')
    x <- coordinateP
    wsP (char ',')
    y <- coordinateP
    wsP (char ')')
    return (x, y)

coordinateP :: Parser Coordinate
coordinateP = (Coordinate <$> wsP intP) <|> (wsP (char ':') *> pure All)

effectP :: Parser Effect
effectP = do
    wsP (string "effect_name:")
    name <- wsP $ many (satisfy (/= '\n'))
    wsP (string "effect_range:")
    range <- wsP effectRangeP
    wsP (string "effect_requirement:")
    req <- wsP requirementP
    wsP (string "effect_description:")
    desc <- wsP $ many (satisfy (/= '\n'))
    return $ Effect name range req desc

candyP :: Parser CandyDefinition
candyP = do
    wsP (string "shape_name:")
    name <- wsP $ many (satisfy (/= '\n'))
    wsP (string "shape_icon:")
    icon <- wsP $ many (satisfy (/= '\n'))
    wsP (string "effect_name:")
    effectRef <- wsP $ many (satisfy (/= '\n'))
    return $ CandyDefinition name icon effectRef

fileP :: Parser ([Effect], [CandyDefinition])
fileP = do
    wsP $ many (commentP <|> emptyLineP)
    effects <- many (effectP <* wsP (many (commentP <|> emptyLineP)))
    candies <- many (candyP <* wsP (many (commentP <|> emptyLineP)))
    return (effects, candies)

commentP :: Parser ()
commentP = wsP $ do
    string "//"
    many (satisfy (/= '\n'))
    char '\n'
    return ()

emptyLineP :: Parser ()
emptyLineP = wsP $ do
    many (satisfy isSpace)
    char '\n'
    return ()

-- Main Function
main :: IO ()
main = do
    content <- readFile "src/candies.txt"
    case parse fileP content of
        Left err -> putStrLn $ "Parse Error: " ++ err
        Right (effects, candies) -> do
            putStrLn "Parsing Successful:"
            putStrLn "Effects Parsed:"
            mapM_ print effects
            putStrLn "\nCandies Parsed:"
            mapM_ print candies