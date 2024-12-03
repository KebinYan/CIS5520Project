module Phd where -- for plain Haskell definition

import Data.Char
import Control.Applicative
import Data.Either (rights)
import Data.Functor (void)
import Data.List (sort, sortBy)
import Data.Map (Map, singleton, empty)
import Control.Monad.State

{-------------------------- Define Difficulty ---------------------------}
type Name = String
-- | Difficulty level with various parameters
data Difficulty = Difficulty
  { dimension :: Int
  , candyMap  :: Map Name Candy
  , effectMap :: Map Name Effect
  , maxSteps  :: Int
  } deriving (Show, Eq)

-- | An empty Difficulty with default values
emptyDifficulty :: Difficulty
emptyDifficulty = Difficulty 0 Data.Map.empty (singleton "Normal" normalEffect) 0


{-------------------------- Define Parser ---------------------------}
-- | Parser state containing the input, current line number, and difficulty
data ParseState = ParseState
  { input      :: String       -- ^ Current input
  , lineNum    :: Int          -- ^ Current line number
  , pDifficulty :: Difficulty   -- ^ Current context's Difficulty
  } deriving (Show, Eq)

-- | Possible parse errors
data ParseError = FatalError String Int | FailError String Int
  deriving (Eq)

instance Show ParseError where
  show (FatalError msg line) =
    "Fatal error at line " ++ show line ++ ": " ++ msg
  show (FailError msg line) =
    "Error at line " ++ show line ++ ": " ++ msg

-- | Parser type definition
newtype Parser a = P { doParse :: ParseState -> Either ParseError (a, ParseState) }
  deriving Functor

instance Applicative Parser where
  pure x = P $ \s -> Right (x, s)
  p1 <*> p2 = P $ \s -> case doParse p1 s of
    Left (FatalError msg line) -> Left (FatalError msg line)  -- Stop on fatal error
    Left err -> Left err                                      -- Propagate other errors
    Right (f, s') -> case doParse p2 s' of                    -- Apply function if p1 succeeds
      Left err -> Left err
      Right (a, s'') -> Right (f a, s'')

instance Alternative Parser where
  empty = P $ \state -> Left (FailError "No parses" (lineNum state))
  p1 <|> p2 = P $ \state -> case doParse p1 state of
    Left (FatalError msg line) -> Left (FatalError msg line)  -- Stop on fatal error
    Left (FailError _ _) -> doParse p2 state                  -- Try p2 on failure
    success -> success                                        -- Return success

instance Monad Parser where
  return = pure
  (>>=) = bindParser

bindParser :: Parser a -> (a -> Parser b) -> Parser b
bindParser p f = P $ \s -> case doParse p s of
  Left (FatalError msg line) -> Left (FatalError msg line)    -- Return fatal errors
  Left err -> Left err                                        -- Propagate other errors
  Right (a, s') -> doParse (f a) s'                           -- Continue with f a



{-------------------------- Define Candies ---------------------------}
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


{-------------------------- Define Game Grid ---------------------------}
-- A GameGrid consists of a 2D grid of candies
data GameGrid = GameGrid {
        board :: [[Candy]],
        normalCandies :: [Candy],
        specialCandies :: Map Int [Candy]
    }
  deriving (Show)

-- check if two boards are equal
instance Eq GameGrid where
    (==) :: GameGrid -> GameGrid -> Bool
    (GameGrid board1 normalCandies1 specialCandies1) ==
        (GameGrid board2 normalCandies2 specialCandies2) =
        board1 == board2
        && normalCandies1 == normalCandies2
        && specialCandies1 == specialCandies2

{-------------------------- Define Game State ---------------------------}
-- The game state includes the current grid, difficulty level, history for undo, and remaining steps
data GameState = GameState {
    currentGrid :: GameGrid,
    difficulty :: Difficulty,
    lastGrid :: Maybe GameGrid,  -- previous grids for undo
    remainingSteps :: Int,  -- number of steps left
    score :: Int            -- current score
} deriving (Eq, Show)

type GameMonad = StateT GameState IO

{-------------------------- Define Actions ---------------------------}
data Action = Swap CoordinatePair CoordinatePair
            | Click CoordinatePair
            | Hint
            | Undo
            | Quit
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
    compare (Swap _ _) _ = LT
    compare (Click _) _ = LT
    compare (Disappear cs1) (Disappear cs2) =
        compare (sort cs1) (sort cs2)
    compare (Disappear _) _ = LT
    compare (Trigger (c1, _)) (Trigger (c2, _)) = compare c1 c2
    compare (Trigger _) _ = LT