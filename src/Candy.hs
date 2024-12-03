module Candy where

import Data.Char
import Control.Applicative
import Data.Either (rights)
import Data.Functor (void)

-- Data Types
type CandyShape = String
type CandyEffect = String

data Coordinate = Coordinate Int | All
    deriving (Show, Eq, Ord)

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
  , ("Eq", Eq)
  , ("Gt", Gt)
  , ("Ge", Ge)
  , ("Lt", Lt)
  , ("Le", Le)
  ]
allowedEffectReqOperators :: [Operator]
allowedEffectReqOperators = [Eq, Gt, Ge]


data Effect = Effect
    { effectName        :: String
    , effectRange       :: EffectRange
    , effectRequirement :: EffectRequirement
    , effectDescription :: String
    } deriving (Show, Eq)

normalEffect :: Effect
normalEffect = Effect "Normal" 
    (Arbitrary [(Coordinate 0, Coordinate 0)]) 
    (EffectRequirement Eq 0) 
    "No special effect"
normalCandy :: Candy 
normalCandy = Candy (CandyDefinition "Normal" "." "Normal") normalEffect

data CandyDefinition = CandyDefinition
    { shapeName     :: String
    , shapeIcon     :: String
    , effectNameRef :: String
    } deriving (Show, Eq)
