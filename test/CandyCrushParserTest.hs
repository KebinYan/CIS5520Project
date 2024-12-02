module CandyCrushParserTest where

import CandyCrushParser
import Candy
import Test.HUnit

-- Test for intP
testIntP :: Test
testIntP = TestCase $ do
    assertEqual "Positive integer" (Right (123, ParseState "" 1)) (doParse intP (ParseState "123" 1))
    assertEqual "Negative integer" (Right (-456, ParseState "" 1)) (doParse intP (ParseState "-456" 1))
    assertEqual "Empty input" (Left (ParseError "Invalid integer:" 1)) (doParse intP (ParseState "" 1))
    assertEqual "Invalid integer" (Left (ParseError "Invalid integer: - at line 1" 1)) (doParse intP (ParseState "-" 1))

-- Test for operatorP
testOperatorP :: Test
testOperatorP = TestCase $ do
    assertEqual "Operator >=" (Right (Ge, ParseState "" 1)) (doParse operatorP (ParseState ">=" 1))
    assertEqual "Operator >" (Right (Gt, ParseState "" 1)) (doParse operatorP (ParseState ">" 1))
    assertEqual "Operator =" (Right (Eq, ParseState "" 1)) (doParse operatorP (ParseState "=" 1))
    assertEqual "Invalid operator" (Left (ParseError "Invalid operator in requirement at line 1" 1)) (doParse operatorP (ParseState "<" 1))

-- Test for requirementP
testRequirementP :: Test
testRequirementP = TestCase $ do
    assertEqual "Requirement >= 5" (Right (Requirement Ge 5, ParseState "" 1)) (doParse requirementP (ParseState ">= 5" 1))
    assertEqual "Requirement > 10" (Right (Requirement Gt 10, ParseState "" 1)) (doParse requirementP (ParseState "> 10" 1))
    assertEqual "Invalid requirement" (Left (ParseError "Invalid operator in effect_requirement at line 1" 1)) (doParse requirementP (ParseState "< 5" 1))

-- Test for effectRangeP
testEffectRangeP :: Test
testEffectRangeP = TestCase $ do
    assertEqual "Circle range" (Right (Circle 3, ParseState "" 1)) (doParse effectRangeP (ParseState "Circle 3" 1))
    assertEqual "Rectangle range" (Right (Rectangle 4 5, ParseState "" 1)) (doParse effectRangeP (ParseState "Rectangle 4 5" 1))
    assertEqual "Diamond range" (Right (Diamond 2, ParseState "" 1)) (doParse effectRangeP (ParseState "Diamond 2" 1))
    assertEqual "Arbitrary range" (Right (Arbitrary [(Coordinate 0, All)], ParseState "" 1)) 
        (doParse effectRangeP (ParseState "Arbitrary [(0, :)]" 1))
    assertEqual "Malformed range" (Left (ParseError "Invalid coordinate list at line 1" 1)) 
        (doParse effectRangeP (ParseState "Arbitrary [(,)]" 1))

-- Test for coordinateP
testCoordinateP :: Test
testCoordinateP = TestCase $ do
    assertEqual "Specific coordinate" (Right (Coordinate 5, ParseState "" 1)) (doParse coordinateP (ParseState "5" 1))
    assertEqual "Wildcard coordinate" (Right (All, ParseState "" 1)) (doParse coordinateP (ParseState ":" 1))
    assertEqual "Invalid coordinate" (Left (ParseError "Expected coordinate (integer or ':') at line 1" 1)) (doParse coordinateP (ParseState "abc" 1))

-- Test for coordP
testCoordP :: Test
testCoordP = TestCase $ do
    assertEqual "Valid coordinate pair" (Right ((Coordinate 1, Coordinate 2), ParseState "" 1)) 
        (doParse coordP (ParseState "(1, 2)" 1))
    assertEqual "Wildcard pair" (Right ((All, All), ParseState "" 1)) 
        (doParse coordP (ParseState "(:, :)" 1))
    assertEqual "Invalid coordinate pair" (Left (ParseError "Expected coordinate (integer or ':') at line 1" 1)) 
        (doParse coordP (ParseState "(1, abc)" 1))

-- Test for effectP
testEffectP :: Test
testEffectP = TestCase $ do
    let validEffect = unlines
            [ "effect_name: Bomb"
            , "effect_range: Circle 3"
            , "effect_requirement: >= 5"
            , "effect_description: Explodes candies in a 3x3 area"
            ]
    let invalidEffect = unlines
            [ "effect_name: Bomb"
            , "effect_range: Circle 3"
            , "effect_requirement: <5"
            , "effect_description: Explodes candies in a 3x3 area"
            ]
    
    assertEqual "Valid effect" 
        (Right (Effect "Bomb" (Circle 3) (Requirement Ge 5) "Explodes candies in a 3x3 area", ParseState "" 5)) 
        (doParse effectP (ParseState validEffect 1))
    assertEqual "Invalid requirement" 
        (Left (ParseError "Invalid operator in effect_requirement at line 3" 3)) 
        (doParse effectP (ParseState invalidEffect 1))

candyCrushTests :: Test
candyCrushTests = TestList
    [ TestLabel "Test intP" testIntP
    , TestLabel "Test operatorP" testOperatorP
    , TestLabel "Test requirementP" testRequirementP
    , TestLabel "Test effectRangeP" testEffectRangeP
    , TestLabel "Test coordinateP" testCoordinateP
    , TestLabel "Test coordP" testCoordP
    , TestLabel "Test effectP" testEffectP
    ]


runCandyCrushParserTests :: IO Counts
runCandyCrushParserTests = runTestTT candyCrushTests