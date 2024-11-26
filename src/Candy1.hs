module Candy1 where

import Parser

type CandyShape = String
type CandyEffect = String

data Candy = Candy { 
    candyShape :: CandyShape
    , candyEffect :: CandyEffect
    , candySymbol :: String
} | EmptyCandy deriving (Show)

parseShape :: Parser CandyShape
parseShape = wsP *> stringP <* wsP
