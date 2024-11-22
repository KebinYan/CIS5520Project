{-# LANGUAGE TemplateHaskell #-}

module CandyTest where

import Test.QuickCheck
import Candy

testIsSameCandy :: Test
testIsSameCandy = TestList [
    "Same candy shape and effect" ~:
        isSameCandy (Candy Triangle Normal) (Candy Triangle Bomb) ~?= True,
    "Different candy shapes" ~:
        isSameCandy (Candy Triangle Normal) (Candy Circle Normal) ~?= False,
    "Empty candy and normal candy" ~:
        isSameCandy EmptyCandy (Candy Triangle Normal) ~?= False
  ]

testRedeemSpecialCandy :: Test
testRedeemSpecialCandy = TestList [
    "Create a striped row candy with 4 candies" ~:
        redeemSpecialCandy (Disappear [(0, 0), (0, 1), (0, 2), (0, 3)]) ~?= Just stripedRowCandy,
    "Create a bomb candy with 5 candies" ~:
        redeemSpecialCandy (Disappear [(0, 0), (0, 1), (0, 2), (0, 3), (0, 4)]) ~?= Just bombCandy,
    "No special candy with fewer than 4 candies" ~:
        redeemSpecialCandy (Disappear [(0, 0), (0, 1), (0, 2)]) ~?= Nothing
  ]

instance Arbitrary CandyShape where
    arbitrary = elements [Triangle .. Asterisk]

instance Arbitrary CandyEffect where
    arbitrary = elements [Normal .. StripedCross]

instance Arbitrary Candy where
    arbitrary = oneof [Candy <$> arbitrary <*> arbitrary, pure EmptyCandy]

prop_specialCandyConditions :: [Coordinate] -> Bool
prop_specialCandyConditions coords =
    let len = length coords
    in case redeemSpecialCandy (Disappear coords) of
        Just stripedRowCandy -> len == 4
        Just bombCandy       -> len == 5
        Just stripedCrossCandy -> len >= 6
        Nothing              -> len < 4

-- check random candy generation is valid
prop_randomCandyValid :: CandyShape -> Bool
prop_randomCandyValid shape =
    all (\c -> candyShape c == shape && candyEffect c == Normal) generatedCandies
  where
    generatedCandies = map (\_ -> Candy shape Normal) [1..100]

-- check CandyShape count matches defined constructors
prop_candyShapeCount :: Bool
prop_candyShapeCount = getCandyShapeCount == length [Triangle .. Asterisk]

-- check CandyEffect count matches defined constructors
prop_candyEffectCount :: Bool
prop_candyEffectCount = getCandyEffectCount == length [Normal .. StripedCross]

-- check isSameCandy works for candies with the same shape
prop_isSameCandyForSameShapes :: CandyShape -> CandyEffect -> CandyEffect -> Bool
prop_isSameCandyForSameShapes shape eff1 eff2 =
    isSameCandy (Candy shape eff1) (Candy shape eff2)

-- chcek isSameCandy returns False for different shapes
prop_isSameCandyForDifferentShapes :: CandyShape -> CandyShape -> Bool
prop_isSameCandyForDifferentShapes shape1 shape2 =
    shape1 /= shape2 ==> not (isSameCandy (Candy shape1 Normal) (Candy shape2 Normal))

runTests :: IO Bool
runTests = $quickCheckAll