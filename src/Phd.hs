module Phd where -- for plain Haskell definition

import GeneralStateParser
import Data.Char
import Control.Applicative
import Data.Either (rights)
import Data.Functor (void)
import Data.List (sort, sortBy)
import Data.Map (Map, singleton, empty)
import Control.Monad.State

{------------------------------ Define GameConst ------------------------------}
type Name = String
-- | GameConst is a record containing the dimension of the game board, 
-- | a map of candies, a map of effects, and the maximum number of steps
data GameConst = GameConst
  { dimension :: Int
  , candyMap  :: Map Name Candy
  , effectMap :: Map Name Effect
  , maxSteps  :: Int
  , scorePerCandy :: Int
  , scorePerEffect :: Int
  } deriving (Show, Eq)

-- | An default GameConst with default values
defaultGameConst :: GameConst
defaultGameConst = GameConst 0 Data.Map.empty
  (singleton "Normal" normalEffect) 0 0 0

{-------------------------- Define a Parser with State ------------------------}
-- | Parser state containing the input, current line number, and gameConst
-- | This is a parser defined for parsing the game configuration file
data CandyFileParser = CandyFileParser
  { input      :: String       -- ^ Current input
  , lineNum    :: Int          -- ^ Current line number
  , pGameConst :: GameConst   -- ^ Current context's game constant
  } deriving (Show, Eq)

-- Make CandyFileParser an instance of ParserState
instance ParserState CandyFileParser where
  getInput :: CandyFileParser -> String
  getInput = input
  setInput :: String -> CandyFileParser -> CandyFileParser
  setInput i s = s { input = i }
  getLineNum :: CandyFileParser -> Int
  getLineNum = lineNum
  setLineNum :: Int -> CandyFileParser -> CandyFileParser
  setLineNum ln s = s { lineNum = ln }

-- | Parser type definition
type Parser a = StateParser CandyFileParser a

{------------------------------ Define Candies --------------------------------}
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

data CandyDefinition = CandyDefinition
    { shapeName     :: String
    , shapeIcon     :: String
    , effectNameRef :: String
    } deriving (Show, Eq)


{---------------------------- Define Game Grid --------------------------------}
-- A GameGrid consists of a 2D grid of candies
data GameGrid = GameGrid {
        board :: [[Candy]],
        normalCandies :: [Candy],
        specialCandies :: Map Int [Candy],
        crushScore :: Int,
        effectScore :: Int,
        scoreChange :: Int
    }
  deriving (Show)

-- check if two boards are equal
instance Eq GameGrid where
    (==) :: GameGrid -> GameGrid -> Bool
    (GameGrid board1 normalCandies1 specialCandies1 _ _ _) == 
        (GameGrid board2 normalCandies2 specialCandies2 _ _ _) =
        board1 == board2
        && normalCandies1 == normalCandies2
        && specialCandies1 == specialCandies2

{----------------------------- Define Game State ------------------------------}
-- The game state includes the current grid, game constants, history for undo, 
-- and remaining steps
data GameState = GameState {
    currentGrid :: GameGrid,
    gameConst :: GameConst,
    lastGrid :: Maybe GameGrid,  -- previous grids for undo
    remainingSteps :: Int,  -- number of steps left
    score :: Int            -- current score
} deriving (Eq, Show)

type GameMonad = StateT GameState IO

{----------------------------- Define Actions ---------------------------------}
data Action = Swap CoordinatePair CoordinatePair
            | Click CoordinatePair
            | Hint
            | Undo
            | Quit
            | Cheat CoordinatePair Candy -- for cheating
            | Trigger (CoordinatePair, Candy)
            | Disappear [CoordinatePair]
    deriving (Show)

instance Eq Action where
    (==) :: Action -> Action -> Bool
    (Swap c1 c2) == (Swap c3 c4) =
        (c1 == c3 && c2 == c4) || (c1 == c4 && c2 == c3)
    (Click c1) == (Click c2) = c1 == c2
    Undo == Undo = True
    Quit == Quit = True
    (Trigger (c1, candy1)) == (Trigger (c2, candy2)) =
        c1 == c2 && candy1 == candy2
    (Disappear cs1) == (Disappear cs2) =
        all (`elem` cs2) cs1 && length cs1 == length cs2
    (Cheat c1 candy1) == (Cheat c2 candy2) = c1 == c2 && candy1 == candy2
    _ == _ = False

-- Constructor for Disappear action that sorts the coordinates
constructDisappear :: [CoordinatePair] -> Action
constructDisappear = Disappear . sortCoordinates
    where
        sortCoordinates =
            sortBy (\(x1, y1) (x2, y2) -> compare (x1, y1) (x2, y2))

-- define the precedence of actions
instance Ord Action where
    compare :: Action -> Action -> Ordering
    compare Quit _ = LT
    compare Undo _ = LT
    compare Hint _ = LT
    compare (Cheat _ _) _ = LT
    compare (Swap _ _) _ = LT
    compare (Click _) _ = LT
    compare (Disappear cs1) (Disappear cs2) =
        compare (sort cs1) (sort cs2)
    compare (Disappear _) _ = LT
    compare (Trigger (c1, _)) (Trigger (c2, _)) = compare c1 c2
    compare (Trigger _) _ = LT
