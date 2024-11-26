module Candy where

data CandyEffect = Normal | Bomb | StripedRow | StripedCross  -- | Note: when adding new effects, add to the MIDDLE of the list
    deriving (Eq, Show, Enum)

data CandyShape = Triangle | Circle | Square | Star | Heart | Diamond | Minus | Cross | Asterisk -- | Note: when adding new types, add to the MIDDLE of the list
    deriving (Eq, Show, Enum)

data Candy = Candy { 
    candyShape :: CandyShape, 
    candyEffect :: CandyEffect
} | EmptyCandy deriving (Show)

instance Eq Candy where
    (==) :: Candy -> Candy -> Bool
    EmptyCandy == EmptyCandy = True
    EmptyCandy == _ = False
    Candy s1 e1 == Candy s2 e2 = s1 == s2 && e1 == e2
    _ == _ = False

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

-- Helper function to represent a single candy as a character or string
candyToSymbol :: Candy -> String
candyToSymbol EmptyCandy = " "
candyToSymbol (Candy { candyShape = t, candyEffect = e }) =
    let (typeSymbol, colorSymbol) = case t of
            Triangle -> ("▲", "\ESC[30m")
            Circle   -> ("●", "\ESC[30m")
            Square   -> ("■", "\ESC[30m")
            Star     -> ("★", "\ESC[30m")
            Heart    -> ("♥", "\ESC[30m")
            Diamond  -> ("◆", "\ESC[30m")
            Minus    -> ("−", "\ESC[91m")
            Cross    -> ("✚", "\ESC[91m")
            Asterisk -> ("✱", "\ESC[91m")
    in colorSymbol ++ typeSymbol ++ "\ESC[0m"  -- reset color after symbol
