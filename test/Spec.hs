import Test.HUnit
import Test.QuickCheck

import CandyTest
import GameStateTest
import GameUtilsTest
import GameControllerTest
import TestUtils


main :: IO ()
main = do
    putStrLn "Candy Crush Test Suite"
    putStrLn "Running HUnit tests..."
    putStrLn "Running Candy tests:"
    -- CandyTest.runUnitTests
    putStrLn "Running GameState tests:"
    GameStateTest.runUnitTests
    putStrLn "Running GameUtils tests:"
    GameUtilsTest.runUnitTests
    putStrLn "Running GameController tests:"
    GameControllerTest.runUnitTests
    putStrLn "Running QuickCheck tests..."
    putStrLn "\nRunning Candy tests:"
    -- CandyTest.runQuickCheckTests
    putStrLn "\nRunning GameState tests:"
    GameStateTest.runQuickCheckTests
    putStrLn "\nRunning GameUtils tests:"
    GameUtilsTest.runQuickCheckTests
    putStrLn "\nRunning GameController tests:"
    GameControllerTest.runQuickCheckTests
