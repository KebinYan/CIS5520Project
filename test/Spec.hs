import Test.HUnit
import Test.QuickCheck

import CandyCrushParserTest
import GameStateTest
import GameUtilsTest
import GameControllerTest
import TestUtils


main :: IO ()
main = do
    putStrLn "Candy Crush Test Suite"
    putStrLn "Running HUnit tests..."
    putStrLn "Running CandyCrushParser tests:"
    CandyCrushParserTest.runAllParserUnitTests
    putStrLn "Running GameState tests:"
    GameStateTest.runUnitTests
    putStrLn "Running GameUtils tests:"
    GameUtilsTest.runUnitTests
    putStrLn "Running GameController tests:"
    GameControllerTest.runUnitTests
    putStrLn "Running QuickCheck tests..."
    putStrLn "\nRunning GameState tests:"
    GameStateTest.runQuickCheckTests
    putStrLn "\nRunning GameUtils tests:"
    GameUtilsTest.runQuickCheckTests
    putStrLn "\nRunning GameController tests:"
    GameControllerTest.runQuickCheckTests
