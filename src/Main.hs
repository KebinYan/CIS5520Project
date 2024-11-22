module Main where

import Control.Monad.IO.Class
import Control.Monad.State

import Candy
import GameController
import GameGrid
import GameUtils

testGrid :: [[Candy]]
testGrid = [ [ Candy { candyShape = t, candyEffect = e } 
               | t <- [Triangle .. Asterisk], e <- [Normal .. StripedCross] ]] 
main :: IO ()
main = do 
    putStrLn "Welcome to Candy Crush!" 
    gameLoop hard
    -- gameState <- initializeGameState hard
    -- printGridState gameState
    -- printGrid GameGrid { board = testGrid, seed = 0 }

