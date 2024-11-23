import Test.HUnit
import Test.QuickCheck

import Lib
import qualified CandyTest
import qualified GameGridTest
import qualified GameUtilsTest


main :: IO ()
main = do
    putStrLn "Candy Crush Test Suite"
    putStrLn "Running HUnit tests..."
    putStrLn "Running Candy tests:"
    CandyTest.runUnitTests
    putStrLn "Running GameGrid tests:"
    GameGridTest.runUnitTests
    putStrLn "Running GameUtil tests:"
    GameUtilsTest.runUnitTests
    putStrLn "Running GameController tests:"
    -- GameGridTest.runUnitTests
    putStrLn "Running QuickCheck tests..."
    putStrLn "Running Candy tests:"
    CandyTest.runQuickCheckTests
    putStrLn "Running GameGrid tests:"
    GameGridTest.runQuickCheckTests
    putStrLn "Running GameUtil tests:"
    GameUtilsTest.runQuickCheckTests
