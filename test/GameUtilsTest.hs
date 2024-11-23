module GameUtilsTest where
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Monadic

import GameUtils
import Candy

-- Unit test for generateRandomCandy
testGenerateRandomCandy :: Test
testGenerateRandomCandy = TestCase $ do
    let candyShapes = [Triangle, Circle, Square]
    candy <- generateRandomCandy candyShapes
    assertBool "Generated candy shape is within the provided list" (candyShape candy `elem` candyShapes)
    assertEqual "Generated candy effect is Normal" Normal (candyEffect candy)

-- Unit test for generateRandomCandyList
testGenerateRandomCandyList :: Test
testGenerateRandomCandyList = TestCase $ do
    let candyShapes = [Triangle, Circle, Square]
    candies <- generateRandomCandyList 10 candyShapes
    assertEqual "Correct number of candies generated" 10 (length candies)
    assertBool "All candy shapes are within the provided list" (all ((`elem` candyShapes) . candyShape) candies)
    assertBool "All candy effects are Normal" (all ((== Normal) . candyEffect) candies)

-- Arbitrary instance for CandyShape
instance Arbitrary CandyShape where
    arbitrary :: Gen CandyShape
    arbitrary = elements [Triangle, Circle, Square]

-- Property: Generated candy shape should be in the input list
prop_generateRandomCandy :: [CandyShape] -> Property
prop_generateRandomCandy candyShapes =
    not (null candyShapes) ==>
    monadicIO $ do
        candy <- run $ generateRandomCandy candyShapes
        Test.QuickCheck.Monadic.assert $ candyShape candy `elem` candyShapes

-- Property: Generated list of candies should have correct length and valid shapes
prop_generateRandomCandyList :: Positive Int -> [CandyShape] -> Property
prop_generateRandomCandyList (Positive len) candyShapes =
    not (null candyShapes) ==>
    monadicIO $ do
        candies <- run $ generateRandomCandyList len candyShapes
        Test.QuickCheck.Monadic.assert $ length candies == len
        Test.QuickCheck.Monadic.assert $ all ((`elem` candyShapes) . candyShape) candies


runUnitTests :: IO Counts
runUnitTests = runTestTT $ TestList [
    TestLabel "testGenerateRandomCandy" testGenerateRandomCandy,
    TestLabel "testGenerateRandomCandyList" testGenerateRandomCandyList
    ]

runQuickCheckTests :: IO ()
runQuickCheckTests = do
    putStrLn "prop_generateRandomCandy:"
    quickCheck prop_generateRandomCandy
    putStrLn "prop_generateRandomCandyList:"
    quickCheck prop_generateRandomCandyList
