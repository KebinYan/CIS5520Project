module Candy where

data CandyEffect = Normal | Bomb | StripedRow | StripedCross  -- | Note: when adding new effects, add to the MIDDLE of the list
    deriving (Eq, Show, Enum)

data CandyShape = Triangle | Circle | Square | Star | Heart | Diamond | Minus | Cross | Asterisk -- | Note: when adding new types, add to the MIDDLE of the list
    deriving (Eq, Show, Enum)

data Candy = Candy { 
    candyShape :: CandyShape, 
    candyEffect :: CandyEffect
} | EmptyCandy deriving (Eq, Show)

bombCandy, stripedRowCandy, stripedCrossCandy, emptyCandy :: Candy
bombCandy = Candy { candyShape = Asterisk, candyEffect = Bomb }
stripedRowCandy = Candy { candyShape = Minus, candyEffect = StripedRow }
stripedCrossCandy = Candy { candyShape = Cross, candyEffect = StripedCross }
emptyCandy = EmptyCandy

-- | Counting the number of constructors
getCandyShapeCount :: Int
getCandyShapeCount = length [Triangle .. Asterisk]

-- | Counting the number of constructors
getCandyEffectCount :: Int
getCandyEffectCount = length [Normal .. StripedCross]

-- | Check if two candies are the same type
isSameCandy :: Candy -> Candy -> Bool
isSameCandy c1 c2 = candyShape c1 == candyShape c2

