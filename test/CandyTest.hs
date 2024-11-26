module CandyTest where
import Prelude
import Test.HUnit
import Test.QuickCheck
import Candy
import Parser
-- import GameGrid

-- testCandyEqualInstance :: Test
-- testCandyEqualInstance = TestList
--     [ TestCase $ assertBool "Same shape and effect" $ Candy Triangle Normal == Candy Triangle Normal
--     , TestCase $ assertBool "Different shape" $ Candy Triangle Normal /= Candy Circle Normal
--     , TestCase $ assertBool "Different effect" $ Candy Triangle Normal /= Candy Triangle Bomb
--     , TestCase $ assertBool "Different shape and effect" $ Candy Triangle Normal /= Candy Asterisk Bomb
--     , TestCase $ assertBool "Empty candies" $ EmptyCandy == EmptyCandy
--     , TestCase $ assertBool "Empty and non-empty" $ EmptyCandy /= Candy Triangle Normal
--     , TestCase $ assertBool "Special candies" $ bombCandy == bombCandy
--     , TestCase $ assertBool "Special candies" $ stripedRowCandy /= bombCandy
--     ]

-- instance Arbitrary CandyShape where
--     arbitrary = elements [Triangle .. Asterisk]

-- instance Arbitrary CandyEffect where
--     arbitrary = elements [Normal .. StripedCross]

-- instance Arbitrary Candy where
--     arbitrary = oneof [Candy <$> arbitrary <*> arbitrary, pure EmptyCandy]

-- -- prop_specialCandyConditions :: [Coordinate] -> Bool
-- -- prop_specialCandyConditions coords =
-- --     let len = length coords
-- --     in case redeemSpecialCandy (Disappear coords) of
-- --         Just stripedRowCandy -> len == 4
-- --         Just bombCandy       -> len == 5
-- --         Just stripedCrossCandy -> len >= 6
-- --         Nothing              -> len < 4

-- -- check CandyShape count matches defined constructors
-- prop_candyShapeCount :: Bool
-- prop_candyShapeCount = getCandyShapeCount == length [Triangle .. Asterisk]

-- -- check CandyEffect count matches defined constructors
-- prop_candyEffectCount :: Bool
-- prop_candyEffectCount = getCandyEffectCount == length [Normal .. StripedCross]

-- tests :: Test
-- tests = TestList [testCandyEqualInstance]

-- runUnitTests :: IO Counts
-- runUnitTests = runTestTT testCandyEqualInstance

-- runQuickCheckTests :: IO ()
-- runQuickCheckTests = do
--     putStrLn "prop_candyShapeCount:"
--     quickCheck prop_candyShapeCount
--     putStrLn "prop_candyEffectCount:"
--     quickCheck prop_candyEffectCount


-- Helper function to run a parser
runParser :: Parser a -> String -> Either String a
runParser p input = case parse p input of
    Left _ -> Left "Parse error"
    Right result -> Right result

-- Tests (as previously defined)
testCoordinateInt :: Test
testCoordinateInt = TestCase $ do
    let input = "123"
    let expected = Right (Coordinate 123)
    assertEqual "Parses an integer coordinate" expected (runParser coordinateP input)

testCoordinateAll :: Test
testCoordinateAll = TestCase $ do
    let input = ":"
    let expected = Right All
    assertEqual "Parses an 'All' coordinate" expected (runParser coordinateP input)

testCoordinateInvalid :: Test
testCoordinateInvalid = TestCase $ do
    let input = "abc"
    let expected = Left "Parse error"
    assertEqual "Fails on invalid input" expected (runParser coordinateP input)

testCoordinatePairValid :: Test
testCoordinatePairValid = TestCase $ do
    let input = "(123, :)"
    let expected = Right (Coordinate 123, All)
    assertEqual "Parses a valid coordinate pair" expected (runParser coordP input)

testCoordinatePairInvalid :: Test
testCoordinatePairInvalid = TestCase $ do
    let input = "(123 :)"
    let expected = Left "Parse error"
    assertEqual "Fails on malformed input" expected (runParser coordP input)

testCircleEffect :: Test
testCircleEffect = TestCase $ do
    let input = "Circle 5"
    let expected = Right (Circle 5)
    assertEqual "Parses a Circle effect range" expected (runParser circleP input)

testRectangleEffect :: Test
testRectangleEffect = TestCase $ do
    let input = "Rectangle 4 3"
    let expected = Right (Rectangle 4 3)
    assertEqual "Parses a Rectangle effect range" expected (runParser rectangleP input)

testDiamondEffect :: Test
testDiamondEffect = TestCase $ do
    let input = "Diamond 2"
    let expected = Right (Diamond 2)
    assertEqual "Parses a Diamond effect range" expected (runParser diamondP input)

testArbitraryEffect :: Test
testArbitraryEffect = TestCase $ do
    let input = "Arbitrary [(0, :), (1, 2)]"
    let expected = Right (Arbitrary [(Coordinate 0, All), (Coordinate 1, Coordinate 2)])
    assertEqual "Parses an Arbitrary effect range" expected (runParser arbitraryP input)

testRequirementGt :: Test
testRequirementGt = TestCase $ do
    let input = "> 3"
    let expected = Right (Requirement Gt 3)
    assertEqual "Parses '>' requirement" expected (runParser requirementP input)

testRequirementGe :: Test
testRequirementGe = TestCase $ do
    let input = ">= 5"
    let expected = Right (Requirement Ge 5)
    assertEqual "Parses '>=' requirement" expected (runParser requirementP input)

testRequirementEq :: Test
testRequirementEq = TestCase $ do
    let input = "= 10"
    let expected = Right (Requirement Eq 10)
    assertEqual "Parses '=' requirement" expected (runParser requirementP input)

testEffectParser :: Test
testEffectParser = TestCase $ do
    let input = unlines
            [ "effect_name: TestEffect"
            , "effect_range: Circle 3"
            , "effect_requirement: >= 2"
            , "effect_description: A circular effect"
            ]
    let expected = Right (Effect "TestEffect" (Circle 3) (Requirement Ge 2) "A circular effect")
    assertEqual "Parses a complete effect definition" expected (runParser effectP input)

testCandyParser :: Test
testCandyParser = TestCase $ do
    let input = unlines
            [ "shape_name: CircleCandy"
            , "shape_icon: O"
            , "effect_name: CircleEffect"
            ]
    let expected = Right (CandyDefinition "CircleCandy" "O" "CircleEffect")
    assertEqual "Parses a complete candy definition" expected (runParser candyP input)

testEffectCheckerValid :: Test
testEffectCheckerValid = TestCase $ do
    let effects = [Effect "TestEffect" (Circle 3) (Requirement Ge 2) "A circular effect"]
    let candies = [CandyDefinition "CircleCandy" "O" "TestEffect"]
    assertEqual "Validates defined effects" (Right ()) (checkEffects effects candies)

checkEffects :: [Effect] -> [CandyDefinition] -> Either String ()
checkEffects effects candies =
    let effectNames = map effectName effects
        undefinedEffects = [e | c <- candies, let e = effectNameRef c, e `notElem` effectNames]
    in if null undefinedEffects
       then Right ()
       else Left $ "Undefined effects used in candies: " ++ show undefinedEffects
       
testEffectCheckerInvalid :: Test
testEffectCheckerInvalid = TestCase $ do
    let effects = [Effect "TestEffect" (Circle 3) (Requirement Ge 2) "A circular effect"]
    let candies = [CandyDefinition "CircleCandy" "O" "UndefinedEffect"]
    assertEqual "Detects undefined effects" 
        (Left "Undefined effects used in candies: [\"UndefinedEffect\"]") 
        (checkEffects effects candies)

testFileParserValid :: Test
testFileParserValid = TestCase $ do
    let input = unlines
            [ "effect_name: TestEffect"
            , "effect_range: Circle 3"
            , "effect_requirement: >= 2"
            , "effect_description: A circular effect"
            , ""
            , "shape_name: CircleCandy"
            , "shape_icon: O"
            , "effect_name: TestEffect"
            ]
    let expected = Right
            ( [ Effect "TestEffect" (Circle 3) (Requirement Ge 2) "A circular effect" ]
            , [ CandyDefinition "CircleCandy" "O" "TestEffect" ]
            )
    assertEqual "Parses a valid file with effects and candies" expected (runParser fileP  input)

testEffectValid :: Test
testEffectValid = TestCase $ do
    let input = "effect_name: Normal\neffect_range: Arbitrary [(0, 0)]\neffect_requirement: 0\neffect_description: Normal candies\n"
    let expected = Right (Effect "Normal" (Arbitrary [(Coordinate 0, Coordinate 0)]) (Requirement Eq 0) "Normal candies")
    assertEqual "Parses a valid effect" expected (runParser effectP input)

testEffectInvalidRange :: Test
testEffectInvalidRange = TestCase $ do
    let input = "effect_name: InvalidEffect\neffect_range: InvalidType 3\neffect_requirement: 0\neffect_description: Invalid effect\n"
    let result = runParser effectWithRecovery input
    assertBool "Handles invalid effect range gracefully" (isLeft result)

testCandyWithUndefinedEffect :: Test
testCandyWithUndefinedEffect = TestCase $ do
    let effects = [Effect "DefinedEffect" (Circle 3) (Requirement Ge 2) "A circular effect"]
    let candies = [CandyDefinition "SomeCandy" "O" "UndefinedEffect"]
    let expected = Left "Undefined effects used in candies: [\"UndefinedEffect\"]"
    assertEqual "Detects undefined effects in candies" expected (checkEffects effects candies)

testInvalidEffectInFile :: Test
testInvalidEffectInFile = TestCase $ do
    let input = unlines
            [ "effect_name: ValidEffect"
            , "effect_range: Circle 5"
            , "effect_requirement: >= 2"
            , "effect_description: A valid effect"
            , ""
            , "effect_name: InvalidEffect"
            , "effect_range: InvalidType 3"
            , "effect_requirement: 0"
            , "effect_description: An invalid effect"
            , ""
            , "shape_name: CircleCandy"
            , "shape_icon: O"
            , "effect_name: ValidEffect"
            , "shape_name: UndefinedCandy"
            , "shape_icon: X"
            , "effect_name: UndefinedEffect"
            ]
    let expected = Right
            ( [ Effect "ValidEffect" (Circle 5) (Requirement Ge 2) "A valid effect" ]
            , [ CandyDefinition "CircleCandy" "O" "ValidEffect" ]
            )
    let result = runParser fileP input
    assertEqual "Parses valid elements while skipping invalid ones" expected result

testFileParserUndefinedCandyAndEffect :: Test
testFileParserUndefinedCandyAndEffect = TestCase $ do
    let input = unlines
            [ "effect_name: TestEffect"
            , "effect_range: Circle 3"
            , "effect_requirement: >= 2"
            , "effect_description: A circular effect"
            , ""
            , "shape_name: CircleCandy"
            , "shape_icon: O"
            , "effect_name: TestEffect"
            , ""
            , "effect_name: InvalidEffect"
            , "effect_range: InvalidType 3"
            , "effect_requirement: 0"
            , "effect_description: Invalid effect"
            , ""
            , "effect_name: AnotherValidEffect"
            , "effect_range: Diamond 5"
            , "effect_requirement: > 10"
            , "effect_description: Another valid effect"
            , ""
            , "shape_name: UndefinedCandy"
            , "shape_icon: X"
            , "effect_name: UndefinedEffect"
            , ""
            , "shape_name: ValidCandy"
            , "shape_icon: V"
            , "effect_name: AnotherValidEffect"
            ]
    let expected = Right
            ( [ Effect "TestEffect" (Circle 3) (Requirement Ge 2) "A circular effect"
              , Effect "AnotherValidEffect" (Diamond 5) (Requirement Gt 10) "Another valid effect"
              ]
            , [ CandyDefinition "CircleCandy" "O" "TestEffect"
              , CandyDefinition "ValidCandy" "V" "AnotherValidEffect"
              ]
            )
    let result = runParser fileP input
    assertEqual "Parses valid elements while skipping invalid ones" expected result

-- Utility function to check if the result is `Left`
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

-- Run all unit tests with labels
runUnitTests :: IO Counts
runUnitTests = runTestTT $ TestList
    [ 
      TestLabel "testCoordinateInt" testCoordinateInt
    , TestLabel "testCoordinateAll" testCoordinateAll
    , TestLabel "testCoordinateInvalid" testCoordinateInvalid
    , TestLabel "testCoordinatePairValid" testCoordinatePairValid
    , TestLabel "testCoordinatePairInvalid" testCoordinatePairInvalid
    , TestLabel "testCircleEffect" testCircleEffect
    , TestLabel "testRectangleEffect" testRectangleEffect
    , TestLabel "testDiamondEffect" testDiamondEffect
    , TestLabel "testArbitraryEffect" testArbitraryEffect
    , TestLabel "testRequirementGt" testRequirementGt
    , TestLabel "testRequirementGe" testRequirementGe
    , TestLabel "testRequirementEq" testRequirementEq
    , TestLabel "testEffectParser" testEffectParser
    , TestLabel "testCandyParser" testCandyParser
    , TestLabel "testEffectCheckerValid" testEffectCheckerValid
    , TestLabel "testEffectCheckerInvalid" testEffectCheckerInvalid
    , TestLabel "testFileParserValid" testFileParserValid
    , TestLabel "testEffectValid" testEffectValid
    , TestLabel "testEffectInvalidRange" testEffectInvalidRange
    , TestLabel "testCandyWithUndefinedEffect" testCandyWithUndefinedEffect
    , TestLabel "testInvalidEffectInFile" testInvalidEffectInFile
    , TestLabel "testFileParserUndefinedCandyAndEffect" testFileParserUndefinedCandyAndEffect
    ]

-- Main function
main :: IO Counts
main = runUnitTests