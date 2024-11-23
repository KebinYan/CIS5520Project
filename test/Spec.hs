import Test.HUnit
import Test.QuickCheck

import Lib
import qualified CandyTest
import qualified GameGridTest


main :: IO ()
main = do
    putStrLn "Candy Crush Test Suite"
    putStrLn "Running HUnit tests..."
    putStrLn "Running Candy tests:"
    CandyTest.runUnitTests
    putStrLn "Running GameGrid tests:"
    GameGridTest.runUnitTests
    -- GameGridTest.runUnitTests
    putStrLn "Running QuickCheck tests..."
    putStrLn "Running Candy tests:"
    CandyTest.runQuickCheckTests
    putStrLn "Running GameGrid tests:"
    GameGridTest.runQuickCheckTests
