module GameUtils where

import Candy
import GHC.Base (error)
import System.Random
import Control.Monad (replicateM)

-- Generate a random Candy based on a provided list of CandyShape
-- | candyShapes: the list of available CandyShape
generateRandomCandy :: [CandyShape] -> IO Candy
generateRandomCandy candyShapes = do
    -- randomIndex <- randomRIO (0, length candyShapes - 1)
    -- return Candy {
    --     candyShape = candyShapes !! randomIndex,
    --     candyEffect = Normal
    -- }
    undefined

-- Generate a list of random candies based on a provided list of CandyShape
-- | len: the number of candies to generate
-- | candyShapes: the list of available CandyShape
generateRandomCandyList :: Int -> [CandyShape] -> IO [Candy]
generateRandomCandyList len candyShapes =
    replicateM len (generateRandomCandy candyShapes)