module CandyTest where

import Test.HUnit
import Test.QuickCheck
import Candy
import GameGrid

testCandyEqualInstance :: Test
testCandyEqualInstance = TestList
    [ TestCase $ assertBool "Same shape and effect" $ Candy Triangle Normal == Candy Triangle Normal
    , TestCase $ assertBool "Different shape" $ Candy Triangle Normal /= Candy Circle Normal
    , TestCase $ assertBool "Different effect" $ Candy Triangle Normal /= Candy Triangle Bomb
    , TestCase $ assertBool "Different shape and effect" $ Candy Triangle Normal /= Candy Asterisk Bomb
    , TestCase $ assertBool "Empty candies" $ EmptyCandy == EmptyCandy
    , TestCase $ assertBool "Empty and non-empty" $ EmptyCandy /= Candy Triangle Normal
    , TestCase $ assertBool "Special candies" $ bombCandy == bombCandy
    , TestCase $ assertBool "Special candies" $ stripedRowCandy /= bombCandy
    ]

instance Arbitrary CandyShape where
    arbitrary = elements [Triangle .. Asterisk]

instance Arbitrary CandyEffect where
    arbitrary = elements [Normal .. StripedCross]

instance Arbitrary Candy where
    arbitrary = oneof [Candy <$> arbitrary <*> arbitrary, pure EmptyCandy]

-- prop_specialCandyConditions :: [Coordinate] -> Bool
-- prop_specialCandyConditions coords =
--     let len = length coords
--     in case redeemSpecialCandy (Disappear coords) of
--         Just stripedRowCandy -> len == 4
--         Just bombCandy       -> len == 5
--         Just stripedCrossCandy -> len >= 6
--         Nothing              -> len < 4

-- check CandyShape count matches defined constructors
prop_candyShapeCount :: Bool
prop_candyShapeCount = getCandyShapeCount == length [Triangle .. Asterisk]

-- check CandyEffect count matches defined constructors
prop_candyEffectCount :: Bool
prop_candyEffectCount = getCandyEffectCount == length [Normal .. StripedCross]

tests :: Test
tests = TestList [testCandyEqualInstance]

runUnitTests :: IO Counts
runUnitTests = runTestTT tests

runQuickCheckTests :: IO ()
runQuickCheckTests = do
--   quickCheck prop_specialCandyConditions
  putStrLn "prop_candyShapeCount:"
  quickCheck prop_candyShapeCount
  putStrLn "prop_candyEffectCount:"
  quickCheck prop_candyEffectCount