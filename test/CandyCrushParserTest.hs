module CandyCrushParserTest where
import CandyCrushParser
import Candy
import Test.HUnit
import Data.Map (empty)
import Data.Map qualified as Map


-- assert that the result is a success and the expected value is returned
assertIsSuccess :: (Show a, Eq b, Show b) => Either a b -> b -> String -> IO ()
assertIsSuccess result expected errMsg =
  case result of
    Right x | x == expected -> return () -- parsing successful and expected value returned
            | otherwise -> error $ errMsg ++ ", but got: " ++ show x
    Left e -> error $ errMsg ++ ", but got error: " ++ show e

-- assert that the result is a fatal error
assertIsFatalError :: (Show b) => Either ParseError b -> String -> IO ()
assertIsFatalError result errMsg =
  case result of
    Left (FatalError _ _) -> return () -- fatal error, test passed
    Left (FailError e line) -> error $ errMsg ++ ", but got fail error at line " ++ show line ++ ": " ++ e
    Right x -> error $ errMsg ++ ", but got: " ++ show x

-- assert that the result is a fail error
assertIsFailError :: (Show b) => Either ParseError b -> String -> IO ()
assertIsFailError result errMsg =
  case result of
    Left (FailError _ _) -> return () -- fail error, test passed
    Left (FatalError e line) -> error $ errMsg ++ ", but got fatal error at line " ++ show line ++ ": " ++ e
    Right x -> error $ errMsg ++ ", but got: " ++ show x


testIntP :: Test
testIntP = TestList
  [ "Valid positive integer" ~: TestCase $ assertIsSuccess
        (doParse intP (ParseState "123 abc" 1 emptyDifficulty))
        (123, ParseState " abc" 1 emptyDifficulty)
        "Expected to parse a valid positive integer"
  , "Valid negative integer" ~:TestCase $ assertIsSuccess
        (doParse intP (ParseState "-456 xyz" 1 emptyDifficulty))
        (-456, ParseState " xyz" 1 emptyDifficulty)
        "Expected to parse a valid negative integer"
  , "Empty input" ~: assertIsFatalError
        (doParse intP (ParseState "" 1 emptyDifficulty))
        "Expected an error for empty input"
  , "Invalid integer with only '-'" ~: assertIsFatalError
        (doParse intP (ParseState "- xyz" 1 emptyDifficulty))
        "Expected an error for invalid '-' input"
  , "Trailing invalid characters" ~: assertIsFatalError
        (doParse intP (ParseState "123.45 xyz" 1 emptyDifficulty))
        "Expected an error for trailing invalid characters"
  , "Extra characters" ~: assertIsFatalError
        (doParse intP (ParseState "123abc" 1 emptyDifficulty))
        "Expected to stop parsing at the first invalid character"
  ]

testBetween :: Test
testBetween = TestList [
    "Between parentheses" ~:
        doParse (between (stringP "(") (string "content") (stringP ")"))
        (ParseState "(content)" 1 emptyDifficulty)
        ~?= Right ("content", ParseState "" 1 emptyDifficulty),
    "Between brackets with space" ~:
        doParse (between (stringP "[") (string "content") (stringP "]"))
        (ParseState "[ content ]" 1 emptyDifficulty)
        ~?= Right ("content", ParseState "" 1 emptyDifficulty),
    "inalid between mismatched brackets" ~:
        TestCase $ case doParse (between (stringP "[") (string "content") (stringP "]"))
                (ParseState "[content)" 1 emptyDifficulty) of
            Left (FailError _ _) -> return ()
            _                  -> assertFailure "Expected a FailError for mismatched brackets"
    ]
testSepBy :: Test
testSepBy = TestList [
    "Comma-separated values" ~:
        doParse (sepBy (string "item") (stringP ","))
        (ParseState "item,item,item" 1 emptyDifficulty)
        ~?= Right (["item", "item", "item"], ParseState "" 1 emptyDifficulty),
    "Single item" ~:
        doParse (sepBy (string "item") (stringP ","))
        (ParseState "item" 1 emptyDifficulty)
        ~?= Right (["item"], ParseState "" 1 emptyDifficulty),
    "Empty list" ~:
        doParse (sepBy (string "item") (stringP ","))
        (ParseState "" 1 emptyDifficulty)
        ~?= Right ([], ParseState "" 1 emptyDifficulty)
    ]

testSepBy1 :: Test
testSepBy1 = TestList [
    "Comma-separated values" ~:
        doParse (sepBy1 (string "item") (stringP ","))
        (ParseState "item,item,item" 1 emptyDifficulty)
        ~?= Right (["item", "item", "item"], ParseState "" 1 emptyDifficulty),
    "Single item" ~:
        doParse (sepBy1 (string "item") (stringP ","))
        (ParseState "item" 1 emptyDifficulty)
        ~?= Right (["item"], ParseState "" 1 emptyDifficulty),
    "Empty list" ~:
        TestCase $ case doParse (sepBy1 (string "item") (stringP ","))
                (ParseState "" 1 emptyDifficulty) of
            Left (FailError _ _) -> return ()
            _                  -> assertFailure "Expected a FailError for an empty list"
    ]

testBrackets :: Test
testBrackets = TestList [
    "Value inside brackets" ~:
        doParse (brackets (string "inner"))
        (ParseState "[inner]" 1 emptyDifficulty)
        ~?= Right ("inner", ParseState "" 1 emptyDifficulty),
    "Mismatched brackets" ~:
        TestCase $ case doParse (brackets (string "inner"))
                (ParseState "[inner)" 1 emptyDifficulty) of
            Left (FailError _ _) -> return ()
            _                  -> assertFailure "Expected a FailError for mismatched brackets"
    ]

testParens :: Test
testParens = TestList [
    "Value inside parentheses" ~:
        doParse (parens (string "inner"))
        (ParseState "(inner)" 1 emptyDifficulty)
        ~?= Right ("inner", ParseState "" 1 emptyDifficulty),
    "Missing closing parenthesis" ~:
        TestCase $ case doParse (parens (string "inner"))
                (ParseState "(inner" 1 emptyDifficulty) of
            Left (FailError _ _) -> return ()
            _                  -> assertFailure "Expected a FailError for missing closing parenthesis"
    ]

testConstP :: Test
testConstP = TestList [
    "Constant parsing with exact match (no trailing whitespaces)" ~:
        doParse (constP "constant" 42)
        (ParseState "constant followed by text" 1 emptyDifficulty)
        ~?= Right (42, ParseState "followed by text" 1 emptyDifficulty),
    "Mismatch for constant" ~:
        TestCase $ case doParse (constP "constant" 42)
                (ParseState "wrong input" 1 emptyDifficulty) of
            Left (FailError _ _) -> return ()
            _                  -> assertFailure "Expected a FailError for mismatched constant"
    ]

testStripP :: Test 
testStripP = TestList
    [ "stripP" ~: do
        let input = "  hello \n "
        let expected = "hello"
        let actual = doParse (stripP (pure input)) (ParseState "" 1 emptyDifficulty)
        actual ~?= Right (expected, ParseState "" 1 emptyDifficulty)
    ]

testString :: Test
testString = TestList [
    "Exact match (no trailing whitespaces)" ~:
        doParse (string "hello")
        (ParseState "hello world" 1 emptyDifficulty)
        ~?= Right ("hello", ParseState " world" 1 emptyDifficulty),
    "Mismatch" ~:
        TestCase $ case doParse (string "hello")
                (ParseState "hi world" 1 emptyDifficulty) of
            Left (FailError _ _) -> return ()
            _                  -> assertFailure "Expected a FailError for mismatch"
    ]

testStringP :: Test
testStringP = TestList [
    "Exact match with whitespace" ~:
        doParse (stringP "hello")
        (ParseState "  hello  world" 1 emptyDifficulty)
        ~?= Right ((), ParseState "world" 1 emptyDifficulty),
    "Mismatch" ~:
        TestCase $ case doParse (stringP "hello")
                (ParseState "  hi world" 1 emptyDifficulty) of
            Left (FailError _ _) -> return ()
            _                  -> assertFailure "Expected a FailError for mismatch"
    ]

testEmptyLine :: Test
testEmptyLine = TestList
  [ "empty line should parse correctly" ~:
      doParse emptyLine (ParseState "  \n" 1 emptyDifficulty)
      ~?= Right ((), ParseState "" 2 emptyDifficulty)
    , "parse one empty line a time 1" ~:
      doParse emptyLine (ParseState "  \n  \n" 1 emptyDifficulty)
      ~?= Right ((), ParseState "\n" 2 emptyDifficulty)
    , "parse one empty line a time 2" ~:
    doParse emptyLine (ParseState "\n\n" 1 emptyDifficulty)
      ~?= Right ((), ParseState "\n" 2 emptyDifficulty)
    , "no empty line should fail" ~: assertIsFailError
        (doParse emptyLine (ParseState "This is not an empty line" 1 emptyDifficulty))
        "Expected a fail error whe no empty line"
  ]
testCommentLine :: Test
testCommentLine = TestList
  [ "single-line comment should parse correctly" ~:
      doParse commentLine (ParseState "// This is a comment\n" 1 emptyDifficulty)
      ~?= Right ((), ParseState "" 2 emptyDifficulty)
    , "parse one empty line a time" ~:
    doParse commentLine (ParseState "// This is a comment\n// another comment\n" 1 emptyDifficulty)
    ~?= Right ((), ParseState "// another comment\n" 2 emptyDifficulty)
  , "non-comment line should fail" ~: assertIsFailError
      (doParse commentLine (ParseState "This is not a comment" 1 emptyDifficulty))
      "Expected a fail error when no comment line"
  ]
testSkipCommentOrEmptyLines :: Test
testSkipCommentOrEmptyLines = TestList
  [ "skip empty lines and comments" ~:
      doParse skipCommentOrEmptyLines (ParseState "\n// comment\n\n// another comment\n" 1 emptyDifficulty)
      ~?= Right ((), ParseState "" 5 emptyDifficulty)
  , "skip empty lines and comment with non-empty content after" ~:
      doParse skipCommentOrEmptyLines (ParseState "// comment\nSome content\n" 1 emptyDifficulty)
      ~?= Right ((), ParseState "Some content\n" 2 emptyDifficulty)
  , "skip comments and empty lines with content after" ~:
      doParse skipCommentOrEmptyLines (ParseState "// comment\n\nSome content\n" 1 emptyDifficulty)
      ~?= Right ((), ParseState "Some content\n" 3 emptyDifficulty)
  , "should not skip non-comment or non-empty lines" ~:
      doParse skipCommentOrEmptyLines (ParseState "Non-comment line" 1 emptyDifficulty)
      ~?= Right ((), ParseState "Non-comment line" 1 emptyDifficulty)
  ]
testCoordinateP :: Test
testCoordinateP = TestList [
    "Specific Pos coordinate" ~: assertIsSuccess
        (doParse coordinateP ( ParseState "5" 1 emptyDifficulty))
        (Coordinate 5, ParseState "" 1 emptyDifficulty)
        "Expected to parse a valid positive integer"
    , "Specific Neg coordinate" ~: assertIsSuccess
        (doParse coordinateP ( ParseState "-5" 1 emptyDifficulty))
        (Coordinate (-5), ParseState "" 1 emptyDifficulty)
        "Expected to parse a valid negative integer"
    , "Wildcard coordinate" ~: assertIsSuccess
        (doParse coordinateP ( ParseState ":" 1 emptyDifficulty))
        (All, ParseState "" 1 emptyDifficulty)
        "Expected to parse a wildcard coordinate"
    , "Invalid coordinate" ~: assertIsFatalError
        (doParse coordinateP ( ParseState "abc" 1 emptyDifficulty))
        "Expected a fatal error for invalid input"
    ]
testCoordinatePairP :: Test
testCoordinatePairP = TestList
    [
        "Valid coordinate pair 1" ~: doParse coordinatePairP (ParseState "( -100 , 2 )" 1 emptyDifficulty)
            ~?= Right ((Coordinate (-100), Coordinate 2), ParseState "" 1 emptyDifficulty)
        ,"Valid coordinate pair 2" ~: doParse coordinatePairP (ParseState "(1,1)" 1 emptyDifficulty)
            ~?= Right ((Coordinate 1, Coordinate 1), ParseState "" 1 emptyDifficulty)
        ,"Valid coordinate pair 3" ~: doParse coordinatePairP (ParseState "(-1, 1000)" 1 emptyDifficulty)
            ~?= Right ((Coordinate (-1), Coordinate 1000), ParseState "" 1 emptyDifficulty)
        ,"Valid coordinate pair 4" ~: doParse coordinatePairP (ParseState "( 1000,-1)" 1 emptyDifficulty)
            ~?= Right ((Coordinate 1000, Coordinate (-1)), ParseState "" 1 emptyDifficulty)
         ,"Valid coordinate pair 5" ~: doParse coordinatePairP (ParseState "(-1,-1)" 1 emptyDifficulty)
            ~?= Right ((Coordinate (-1), Coordinate (-1)), ParseState "" 1 emptyDifficulty)
        , "Wildcard Pos Pair" ~: doParse coordinatePairP (ParseState "( 1  , :)" 1 emptyDifficulty)
            ~?= Right ((Coordinate 1, All), ParseState "" 1 emptyDifficulty)
        , "Wildcard Neg Pair" ~: doParse coordinatePairP (ParseState "( :, -2  )" 1 emptyDifficulty)
            ~?= Right ((All, Coordinate (-2)), ParseState "" 1 emptyDifficulty)
        , "Wildcard pair" ~: doParse coordinatePairP (ParseState "( : , : )" 1 emptyDifficulty)
            ~?= Right ((All, All), ParseState "" 1 emptyDifficulty)
        , "Invalid coordinate pair 1" ~: assertIsFatalError
            (doParse coordinatePairP (ParseState "(1, abc)" 1 emptyDifficulty))
            "Expected a fatal error for invalid input"
        ,  "Valid brackets with single pair" ~: doParse coordinateListP (ParseState "[(1,2)]" 1 emptyDifficulty)
            ~?= Right ([(Coordinate 1, Coordinate 2)], ParseState "" 1 emptyDifficulty)
        ,  "Valid brackets with multiple pairs" ~: doParse coordinateListP (ParseState "[(1,2),(3,4)]" 1 emptyDifficulty)
            ~?= Right ([(Coordinate 1, Coordinate 2), (Coordinate 3, Coordinate 4)], ParseState "" 1 emptyDifficulty)
        , "Empty brackets" ~:
            doParse coordinateListP (ParseState "[]" 1 emptyDifficulty)
            ~?= Right ([], ParseState "" 1 emptyDifficulty)
        , "testCoordinatePairP Missing closing bracket" ~: assertIsFailError
            (doParse coordinateListP (ParseState "[(1,2),(3,4)" 1 emptyDifficulty))
            "Expected a fatal error for missing closing bracket"
        , "Invalid coordinate pair 2" ~: assertIsFailError
            (doParse coordinateListP (ParseState "[(1,2),(3)]" 1 emptyDifficulty))
            "Expected a fatal error for invalid coordinate pair"
    ]
testArbitraryPIgnoreCase :: Test
testArbitraryPIgnoreCase = TestList
    [ "Arbitrary with single coordinate pair" ~:
        doParse arbitraryP (ParseState "Arbitrary [(1,2)]" 1 emptyDifficulty)
        ~?= Right (Arbitrary [(Coordinate 1, Coordinate 2)], ParseState "" 1 emptyDifficulty)
    , "Arbitrary with wildcard coordinates" ~:
        doParse arbitraryP (ParseState "Arbitrary [(0,:)]" 1 emptyDifficulty)
        ~?= Right (Arbitrary [(Coordinate 0, All)], ParseState "" 1 emptyDifficulty)
    , "Arbitrary with multiple coordinate pairs" ~:
        doParse arbitraryP (ParseState "Arbitrary [(1,2),(:,:),(-2,12)]" 1 emptyDifficulty)
        ~?= Right (Arbitrary [(Coordinate 1, Coordinate 2), (All, All), (Coordinate (-2), Coordinate 12)], ParseState "" 1 emptyDifficulty)
    , "testArbitraryPIgnoreCase Arbitrary with missing closing bracket" ~: assertIsFatalError
        (doParse arbitraryP (ParseState "Arbitrary [(1,2),(:,:),(-2,12)" 1 emptyDifficulty))
        "Expected a fatal error for missing closing bracket"
    , "Arbitrary with invalid coordinate tuple" ~: assertIsFatalError
        (doParse arbitraryP (ParseState "Arbitrary [(1,2),(1,2,3)]" 1 emptyDifficulty))
        "Expected a fatal error for invalid tuple"
    ]

testCoordinateListP :: Test
testCoordinateListP = TestList
    [
        "Empty list" ~: doParse coordinateListP (ParseState "[]" 1 emptyDifficulty)
            ~?= Right ([], ParseState "" 1 emptyDifficulty)
        , "Single pair" ~: doParse coordinateListP (ParseState "[ (1 , 2 ) ]" 1 emptyDifficulty)
            ~?= Right ([(Coordinate 1, Coordinate 2)], ParseState "" 1 emptyDifficulty)
        , "Multiple pairs" ~: doParse coordinateListP (ParseState "[ ( 1,2 ), (:,:), (-2, 12)]" 1 emptyDifficulty)
            ~?= Right ([(Coordinate 1, Coordinate 2), (All, All), (Coordinate (-2), Coordinate 12)], ParseState "" 1 emptyDifficulty)
    ]

testCirclePIgnoreCase :: Test
testCirclePIgnoreCase = TestList
    [ "circle 3" ~: doParse circleP (ParseState "Circle 3" 1 emptyDifficulty)
        ~?= Right (Circle 3, ParseState "" 1 emptyDifficulty)
    , "cirCle 0" ~: doParse circleP (ParseState "cirCle 0" 1 emptyDifficulty)
        ~?= Right (Circle 0, ParseState "" 1 emptyDifficulty)
    , "cIrcLe -3" ~: doParse circleP (ParseState "cIrcLe -3" 1 emptyDifficulty)
        ~?= Right (Circle (-3), ParseState "" 1 emptyDifficulty)
    , "CIRCLE 100" ~: doParse circleP (ParseState "CIRCLE 100" 1 emptyDifficulty)
        ~?= Right (Circle 100, ParseState "" 1 emptyDifficulty)
    , "circle 3.15" ~: assertIsFatalError
        (doParse circleP (ParseState "Circle 3.15" 1 emptyDifficulty))
        "Expected a fatal error for invalid decimal radius"
    , "CIRCLE 3 4 5" ~: doParse circleP (ParseState "CIRCLE 3 4 5" 1 emptyDifficulty)
        ~?= Right (Circle 3, ParseState "4 5" 1 emptyDifficulty)
    , "CIRCLE 3 4" ~: doParse circleP (ParseState "CIRCLE 3 4" 1 emptyDifficulty)
        ~?= Right (Circle 3, ParseState "4" 1 emptyDifficulty)
    , "circle a" ~: assertIsFatalError
        (doParse circleP (ParseState "circle a" 1 emptyDifficulty))
        "Expected a fatal error for invalid input"
    ]
testRectanglePIgnoreCase :: Test
testRectanglePIgnoreCase = TestList
    [ "Rectangle with positive dimensions" ~:
        doParse rectangleP (ParseState "Rectangle 3 4" 1 emptyDifficulty)
        ~?= Right (Rectangle 3 4, ParseState "" 1 emptyDifficulty)
    , "Rectangle with zero dimensions" ~:
        doParse rectangleP (ParseState "ReCtAnGlE 0 0" 1 emptyDifficulty)
        ~?= Right (Rectangle 0 0, ParseState "" 1 emptyDifficulty)
    , "Rectangle with mixed dimensions" ~:
        doParse rectangleP (ParseState "rEctAngLe -3 4" 1 emptyDifficulty)
        ~?= Right (Rectangle (-3) 4, ParseState "" 1 emptyDifficulty)
    , "Rectangle with trailing input" ~:
        doParse rectangleP (ParseState "RECTANGLE 3 4 5 6 7" 1 emptyDifficulty)
        ~?= Right (Rectangle 3 4, ParseState "5 6 7" 1 emptyDifficulty)
    , "Rectangle with missing dimension" ~: assertIsFatalError
        (doParse rectangleP (ParseState "rectangle 3" 1 emptyDifficulty))
        "Expected a fail error for missing dimension"
    , "Rectangle with invalid input" ~: assertIsFatalError
        (doParse rectangleP (ParseState "rectangle a 4" 1 emptyDifficulty))
        "Expected a fatal error for invalid input"
    ]
testDiamondPIgnoreCase :: Test
testDiamondPIgnoreCase = TestList
    [ "Diamond with positive radius" ~:
        doParse diamondP (ParseState "Diamond 3" 1 emptyDifficulty)
        ~?= Right (Diamond 3, ParseState "" 1 emptyDifficulty)
    , "Diamond with zero radius" ~:
        doParse diamondP (ParseState "DIAMOND 0" 1 emptyDifficulty)
        ~?= Right (Diamond 0, ParseState "" 1 emptyDifficulty)
    , "Diamond with negative radius" ~:
        doParse diamondP (ParseState "dIaMoNd -3" 1 emptyDifficulty)
        ~?= Right (Diamond (-3), ParseState "" 1 emptyDifficulty)
    , "Diamond with trailing input" ~:
        doParse diamondP (ParseState "DIAMOND 3 4" 1 emptyDifficulty)
        ~?= Right (Diamond 3, ParseState "4" 1 emptyDifficulty)
    , "Diamond with invalid input" ~: assertIsFatalError
        (doParse diamondP (ParseState "diamond a" 1 emptyDifficulty))
        "Expected a fatal error for invalid input"
    ]

testEffectRangeP :: Test
testEffectRangeP = TestList
    [ "testEffectRangeP valid Circle" ~: doParse effectRangeP (ParseState "cirCle 3" 1 emptyDifficulty)
          ~?= Right (Circle 3, ParseState "" 1 emptyDifficulty)
    ,"testEffectRangeP invalid Circle 1" ~: assertIsFatalError
        (doParse effectRangeP (ParseState "Circle 3.15" 1 emptyDifficulty))
        "Expected a fatal error for invalid decimal radius"
    , "testEffectRangeP valid Rectangle" ~: doParse effectRangeP (ParseState "Rectangle 3 4" 1 emptyDifficulty)
          ~?= Right (Rectangle 3 4, ParseState "" 1 emptyDifficulty)
    , "testEffectRangeP valid Diamond" ~: doParse effectRangeP (ParseState "Diamond 5" 1 emptyDifficulty)
          ~?= Right (Diamond 5, ParseState "" 1 emptyDifficulty)
    , "testEffectRangeP valid Arbitrary" ~: doParse effectRangeP (ParseState "Arbitrary [(1,2), (:,:)]" 1 emptyDifficulty)
          ~?= Right (Arbitrary [(Coordinate 1, Coordinate 2), (All, All)], ParseState "" 1 emptyDifficulty)
    , "testEffectRangeP invalid Arbitrary" ~: assertIsFatalError
        (doParse effectRangeP (ParseState "Arbitrary [(1.3,2), (:,:)]" 1 emptyDifficulty))
        "Expected a fatal error for unrecognized effect range type"
    , "Unrecognized" ~: assertIsFatalError
        (doParse effectRangeP (ParseState "InvalidRange 7" 1 emptyDifficulty))
        "Unrecognized effect range type"
    ]

testEffectNameP :: Test
testEffectNameP = TestList
  [ "Valid effect name" ~:
      doParse effectNameP (ParseState "effect_name:StripedRow\n" 1 emptyDifficulty)
      ~?= Right ("StripedRow", ParseState "" 2 emptyDifficulty)
    , "Effect name with spaces" ~:
        doParse effectNameP (ParseState "effect_name:Striped Row\n" 1 emptyDifficulty)
        ~?= Right ("Striped Row", ParseState "" 2 emptyDifficulty)
    , "testEffectNameP Missing effect name" ~: assertIsFatalError
        (doParse effectNameP (ParseState "effect_name:\n" 1 emptyDifficulty))
        "Expected a FatalError for missing effect name"
    , "Missing EOF" ~: assertIsFatalError
        (doParse effectNameP (ParseState "effect_name:StripedRow" 1 emptyDifficulty))
        "Expected a FatalError for missing newline"
  ]
testEffectRangeLineP :: Test
testEffectRangeLineP = TestList
  [ "Valid effect range" ~:
      doParse effectRangeLineP (ParseState "effect_range: Arbitrary [(:,0)]\n" 1 emptyDifficulty)
      ~?= Right (Arbitrary [(All, Coordinate 0)], ParseState "" 2 emptyDifficulty)
  , "Invalid effect range" ~:  assertIsFatalError
    (doParse effectRangeLineP (ParseState "effect_range: InvalidRange\n" 1 emptyDifficulty))
    "Expected a FatalError for invalid effect range"
  ]
testOperatorP :: Test
testOperatorP = TestList
  [ "Valid operators" ~: TestList
      [ doParse operatorP (ParseState "=" 1 emptyDifficulty) ~?= Right (Eq, ParseState "" 1 emptyDifficulty)
        , doParse operatorP (ParseState ">" 1 emptyDifficulty) ~?= Right (Gt, ParseState "" 1 emptyDifficulty)
        , doParse operatorP (ParseState ">=\n" 1 emptyDifficulty) ~?= Right (Ge, ParseState "\n" 1 emptyDifficulty)
        , doParse operatorP (ParseState "<" 1 emptyDifficulty) ~?= Right (Lt, ParseState "" 1 emptyDifficulty)
        , doParse operatorP (ParseState "<=" 1 emptyDifficulty) ~?= Right (Le, ParseState "" 1 emptyDifficulty)
      ]
    , "unknown operator" ~: assertIsFailError
        (doParse operatorP (ParseState "--" 1 emptyDifficulty))
        "Expected a FailError for unknown operator"
  ]
testEffectRequirementP :: Test
testEffectRequirementP = TestList
    [
     "Valid effect requirement with =" ~:
        doParse effectRequirementP (ParseState "effect_requirement: = 5\n" 1 emptyDifficulty)
        ~?= Right (EffectRequirement Eq 5, ParseState "" 2 emptyDifficulty)
    , "Valid effect requirement with >" ~:
        doParse effectRequirementP (ParseState "effect_requirement: > 3\n" 1 emptyDifficulty)
        ~?= Right (EffectRequirement Gt 3, ParseState "" 2 emptyDifficulty)
    , "Invalid operator in requirement" ~:
        assertIsFatalError
            (doParse effectRequirementP (ParseState "effect_requirement: <= 5\n" 1 emptyDifficulty))
            "Expected a FailError for invalid operator"
    , "Invalid negative number in requirement" ~:
        assertIsFatalError
            (doParse effectRequirementP (ParseState "effect_requirement: > -5\n" 1 emptyDifficulty))
            "Expected a FailError for negative number"
    , "Invalid number in requirement" ~:
        assertIsFatalError
            (doParse effectRequirementP (ParseState "effect_requirement: > abc\n" 1 emptyDifficulty))
            "Expected a FailError for invalid number"
    , "Missing number in requirement" ~:
        assertIsFatalError
            (doParse effectRequirementP (ParseState "effect_requirement: >\n" 1 emptyDifficulty))
            "Expected a FailError for missing number"
    , "Missing operator in requirement" ~:
        assertIsFatalError
            (doParse effectRequirementP (ParseState "effect_requirement: 5\n" 1 emptyDifficulty))
            "Expected a FailError for missing operator"
    , "Invalid operator in requirement" ~:
        assertIsFatalError
            (doParse effectRequirementP (ParseState "effect_requirement: >- 5\n" 1 emptyDifficulty))
            "Expected a FatalError for invalid effect requirement"
  ]
testEffectDescriptionP :: Test
testEffectDescriptionP = TestList
  [ "Valid effect description" ~:
      doParse effectDescriptionP (ParseState "effect_description: Clears the row\n" 1 emptyDifficulty)
      ~?= Right ("Clears the row", ParseState "" 2 emptyDifficulty)
    , "Missing effect description" ~: assertIsFatalError
        (doParse effectDescriptionP (ParseState "effect_description: \n" 1 emptyDifficulty))
        "Expected a FatalError for missing effect description"
    , "Missing newline" ~: assertIsFatalError
        (doParse effectDescriptionP (ParseState "effect_description: Clears the row" 1 emptyDifficulty))
        "Expected a FatalError for missing newline"
  ]

  -- 定义标准测试数据
testNormalEffectInput :: String
testNormalEffectInput = "effect_name: Normal\n\
                        \effect_range: Arbitrary [(0,0)]\n\
                        \effect_requirement: =0\n\
                        \effect_description: some str..\n"

testStripedRowEffectInput :: String
testStripedRowEffectInput = "effect_name: StripedRow\n\
                            \effect_range: Arbitrary [(:,0)]\n\
                            \effect_requirement: >0\n\
                            \effect_description: some str..\n"

testWrappedEffectInput :: String
testWrappedEffectInput = "effect_name: Wrapped\n\
                         \effect_range: Circle 3\n\
                         \effect_requirement: >=0\n\
                         \effect_description: some str..\n"

-- 构造对应的标准 Effect
testNormalEffect :: Effect
testNormalEffect = Effect
    { effectName = "Normal"
    , effectRange = Arbitrary [(Coordinate 0, Coordinate 0)]
    , effectRequirement = EffectRequirement Eq 0
    , effectDescription = "some str.."
    }

testStripedRowEffect :: Effect
testStripedRowEffect = Effect
    { effectName = "StripedRow"
    , effectRange = Arbitrary [(All, Coordinate 0)]
    , effectRequirement = EffectRequirement Gt 0
    , effectDescription = "some str.."
    }

testWrappedEffect :: Effect
testWrappedEffect = Effect
    { effectName = "Wrapped"
    , effectRange = Circle 3
    , effectRequirement = EffectRequirement Ge 0
    , effectDescription = "some str.."
    }
testValidEffectP :: Test
testValidEffectP = TestList
  [ "Valid Normal effect" ~:
      doParse effectP (ParseState testNormalEffectInput 1 emptyDifficulty)
      ~?= Right ((), ParseState "" 5 emptyDifficulty
                    { effectMap = Map.fromList [("Normal", testNormalEffect)] })
  , "Valid StripedRow effect" ~:
      doParse effectP (ParseState testStripedRowEffectInput 1 emptyDifficulty)
      ~?= Right ((), ParseState "" 5 emptyDifficulty
                    { effectMap = Map.fromList [("StripedRow", testStripedRowEffect)] })
  , "Valid Wrapped effect" ~:
      doParse effectP (ParseState testWrappedEffectInput 1 emptyDifficulty)
      ~?= Right ((), ParseState "" 5 emptyDifficulty
                    { effectMap = Map.fromList [("Wrapped", testWrappedEffect)] })
  ]
testValidEffectsP :: Test
testValidEffectsP = TestList
  [ "Valid effects definition" ~:
      doParse effectsP (ParseState (testNormalEffectInput ++ "\n" ++ "\n"
                                    ++ testStripedRowEffectInput) 1 emptyDifficulty)
      ~?= Right ((), ParseState "" 11 emptyDifficulty
                    { effectMap = Map.fromList
                        [ ("Normal", testNormalEffect)
                        , ("StripedRow", testStripedRowEffect)
                        ]
                    })
  , "Valid effects with comments and empty lines" ~:
      doParse effectsP (ParseState ("// Comment\n" ++ testNormalEffectInput
                                    ++ "\n\n" ++ testStripedRowEffectInput
                                    ++ "\n//another comment\n" ++ testWrappedEffectInput) 1 emptyDifficulty)
      ~?= Right ((), ParseState "" 18 emptyDifficulty
                    { effectMap = Map.fromList
                        [ ("Normal", testNormalEffect)
                        , ("StripedRow", testStripedRowEffect)
                        , ("Wrapped", testWrappedEffect)
                        ]
                    })
    , "Valid effects without empty lines" ~:
        doParse effectsP (ParseState (testNormalEffectInput ++ testStripedRowEffectInput
                                      ++ testWrappedEffectInput) 1 emptyDifficulty)
        ~?= Right ((), ParseState "" 13 emptyDifficulty
                      { effectMap = Map.fromList
                          [ ("Normal", testNormalEffect)
                          , ("StripedRow", testStripedRowEffect)
                          , ("Wrapped", testWrappedEffect)
                          ]
                      })
    , "Valid update for duplicate effect names" ~:
        doParse effectsP (ParseState (testNormalEffectInput
                                      ++ testWrappedEffectInput
                                      ++ "effect_name: Normal\n\
                                        \effect_range: Rectangle 3 3\n\
                                        \effect_requirement: =0\n\
                                        \effect_description: some str..\n")
                                        1 emptyDifficulty)
        ~?= Right ((), ParseState "" 13 emptyDifficulty
                        { effectMap = Map.fromList
                            [ ("Normal", Effect
                                { effectName = "Normal"
                                , effectRange = Rectangle 3 3
                                , effectRequirement = EffectRequirement Eq 0
                                , effectDescription = "some str.."
                                })
                            , ("Wrapped", testWrappedEffect)
                            ]
                        })
  ]

testInvalidEffectsP :: Test
testInvalidEffectsP = TestList
  [ "Invalid effects definition" ~:
      assertIsFatalError
        (doParse effectsP (ParseState "effect_name: Normal\n\
                                    \effect_range: Arbitrary [(0,0)]\n\
                                    \effect_requirement: =0\n\
                                    \effect_description: some str..\n\
                                    \effect_name: StripedRow\n\
                                    \effect_range: Arbitrary [(:,0)]\n\
                                    \effect_requirement: >0\n\
                                    \effect_description: some str..\n\
                                    \effect_name: Wrapped\n\
                                    \effect_range: Circle --3\n\
                                    \effect_requirement: >=0\n\
                                    \effect_description: some str..\n"
                                    1 emptyDifficulty))
        "Expected a FatalError for invalid effect range --3"
  , "Invalid effects definition with missing effect name" ~:
      assertIsFatalError
        (doParse effectsP (ParseState "effect_name: Normal\n\
                                    \effect_range: Arbitrary [(0,0)]\n\
                                    \effect_requirement: =0\n\
                                    \effect_description: some str..\n\
                                    \effect_range: Arbitrary [(:,0)]\n\
                                    \effect_requirement: >0\n\
                                    \effect_description: some str..\n\
                                    \effect_name: Wrapped\n\
                                    \effect_range: Circle 3\n\
                                    \effect_requirement: >=0\n\
                                    \effect_description: some str..\n"
                                    1 emptyDifficulty))
        "Expected a FatalError for missing effect name"
  ]


-- Invalid effect_name cases
testInvalidEffectNameP :: Test
testInvalidEffectNameP = TestList
  [ "Invalid effect_name tag" ~:
      assertIsFatalError
        (doParse effectP (ParseState "invalid_effect_name: \n\
                                     \effect_range: Arbitrary [(0,0)]\n\
                                     \effect_requirement: =0\n\
                                     \effect_description: some str..\n"
                                     1 emptyDifficulty))
        "Expected a FatalError for invalid effect name"
  , "Invalid effect_name value" ~:
      assertIsFatalError
        (doParse effectP (ParseState "effect_name: \n\
                                     \effect_range: Arbitrary [(0,0)]\n\
                                     \effect_requirement: =0\n\
                                     \effect_description: some str..\n"
                                     1 emptyDifficulty))
        "Expected a FatalError for invalid effect name"
  , "testInvalidEffectNameP Missing effect name" ~:
      assertIsFatalError
        (doParse effectP (ParseState "effect_range: Arbitrary [(0,0)]\n\
                                     \effect_requirement: =0\n\
                                     \effect_description: some str..\n"
                                     1 emptyDifficulty))
        "Expected a FatalError for missing effect name"
  ]

-- Invalid effect_range cases
testInvalidEffectRangeP :: Test
testInvalidEffectRangeP = TestList
  [ "Invalid effect_range tag" ~:
      assertIsFatalError
        (doParse effectP (ParseState "effect_name: Normal\n\
                                     \invalid effect_range: Circle 3\n\
                                     \effect_requirement: =0\n\
                                     \effect_description: some str..\n"
                                     1 emptyDifficulty))
        "Expected a FatalError for invalid effect range"
  , "Invalid effect_range value" ~:
      assertIsFatalError
        (doParse effectP (ParseState "effect_name: Normal\n\
                                     \effect_range: InvalidRange\n\
                                     \effect_requirement: =0\n\
                                     \effect_description: some str..\n"
                                     1 emptyDifficulty))
        "Expected a FatalError for invalid effect range"
  , "Missing effect_range" ~:
      assertIsFatalError
        (doParse effectP (ParseState "effect_name: Normal\n\
                                     \effect_requirement: =0\n\
                                     \effect_description: some str..\n"
                                     1 emptyDifficulty))
        "Expected a FatalError for missing effect range"
  ]

-- Invalid effect_requirement cases
testInvalidEffectRequirementP :: Test
testInvalidEffectRequirementP = TestList
  [ "Invalid effect_requirement value" ~:
      assertIsFatalError
        (doParse effectP (ParseState "effect_name: Normal\n\
                                     \effect_range: Arbitrary [(0,0)]\n\
                                     \effect_requirement: abc\n\
                                     \effect_description: some str..\n"
                                     1 emptyDifficulty))
        "Expected a FatalError for invalid effect requirement"
  , "Invalid effect_requirement value negative" ~:
      assertIsFatalError
        (doParse effectP (ParseState "effect_name: Normal\n\
                                     \effect_range: Arbitrary [(0,0)]\n\
                                     \effect_requirement: >-1\n\
                                     \effect_description: some str..\n"
                                     1 emptyDifficulty))
        "Expected a FatalError for invalid effect requirement"
  , "Missing effect_requirement" ~:
      assertIsFatalError
        (doParse effectP (ParseState "effect_name: Normal\n\
                                     \effect_range: Arbitrary [(0,0)]\n\
                                     \effect_description: some str..\n"
                                     1 emptyDifficulty))
        "Expected a FatalError for missing effect requirement"
  ]

-- Invalid effect_description cases
testInvalidEffectDescriptionP :: Test
testInvalidEffectDescriptionP = TestList
  [ "Invalid effect_description tag" ~:
      assertIsFatalError
        (doParse effectP (ParseState "effect_name: Normal\n\
                                     \effect_range: Arbitrary [(0,0)]\n\
                                     \effect_requirement: =0\n\
                                     \invalid_effect_description: \n"
                                     1 emptyDifficulty))
        "Expected a FatalError for invalid effect description"
  , "Missing effect_description" ~:
      assertIsFatalError
        (doParse effectP (ParseState "effect_name: Normal\n\
                                     \effect_range: Arbitrary [(0,0)]\n\
                                     \effect_requirement: =0\n"
                                     1 emptyDifficulty))
        "Expected a FatalError for missing effect description"
  ]

-- Combined tests
testEffectP :: Test
testEffectP = TestList
  [ testValidEffectP
  , testInvalidEffectNameP
  , testInvalidEffectRangeP
  , testInvalidEffectRequirementP
  , testInvalidEffectDescriptionP
  , testValidEffectsP
  , testInvalidEffectsP
  ]



validConstantInput :: String
validConstantInput =
  "\n// Comment\n\n\n\
  \difficulty_constant\n\
  \dimension: 5\n\
  \max_steps: 10\n"

invalidDimensionTag :: String
invalidDimensionTag =
  "// Comment\n\
  \difficulty_constant\n\
  \invalid_dimension: 3\n\
  \max_steps: 10\n"

invalidDimensionValue :: String
invalidDimensionValue =
  "// Comment\n\
  \difficulty_constant\n\
  \dimension: 2\n\
  \max_steps: 10\n"

invalidMaxStepsTag :: String
invalidMaxStepsTag =
  "// Comment\n\
  \difficulty_constant\n\
  \dimension: 3\n\
  \invalid_max_steps: 1\n"

invalidMaxStepsValue :: String
invalidMaxStepsValue =
  "// Comment\n\
  \difficulty_constant\n\
  \dimension: 3\n\
  \max_steps: 1\n"

invalidBlockInput :: String
invalidBlockInput =
  "// Comment\n\
  \difficulty_constant\n\
  \shape_name: Circle\n\
  \dimension: 3\n\
  \max_steps: 10\n"

invalidSequenceInput :: String
invalidSequenceInput =
  "// Comment\n\
  \difficulty_constant\n\
  \max_steps: 10\n\
  \dimension: 3\n"
-- 测试用例
testDifficultyConstantP :: Test
testDifficultyConstantP = TestList
  [ "Valid input" ~: doParse difficultyConstantP (ParseState validConstantInput 1 emptyDifficulty)
        ~?= Right ((), ParseState "" 8 (Difficulty 5 Map.empty Map.empty 10))
    , "Invalid dimension tag" ~: assertIsFatalError
        (doParse difficultyConstantP (ParseState invalidDimensionTag 1 emptyDifficulty))
        "Expected an error for invalid dimension tag"
    , "Invalid dimension value" ~: assertIsFatalError
        (doParse difficultyConstantP (ParseState invalidDimensionValue 1 emptyDifficulty))
        "Expected an error for invalid dimension value"
    ,  "Invalid max_steps tag" ~: assertIsFatalError
        (doParse difficultyConstantP (ParseState invalidMaxStepsTag 1 emptyDifficulty))
        "Expected an error for invalid max_steps tag"
    , "Invalid max_steps value" ~: assertIsFatalError
        (doParse difficultyConstantP (ParseState invalidMaxStepsValue 1 emptyDifficulty))
        "Expected an error for invalid max_steps value"
    , "Invalid block input" ~: assertIsFatalError
        (doParse difficultyConstantP (ParseState invalidBlockInput 1 emptyDifficulty))
        "Expected to stop parsing at the first invalid character"
    , "Invalid sequence input" ~: assertIsFatalError
        (doParse difficultyConstantP (ParseState invalidSequenceInput 1 emptyDifficulty))
        "Expected to stop parsing at the first invalid character"
  ]


testDifficultyWithSomeEffects :: Difficulty
testDifficultyWithSomeEffects = Difficulty {
    dimension = 5,
    maxSteps = 30,
    effectMap = Map.fromList [("effect1", Effect "effect1" (Circle 1)
                              (EffectRequirement Ge 0) "some descrp"),
                              ("effect2", Effect "effect2" (Diamond 4)
                              (EffectRequirement Gt 0) "some de.scrp"),
                              ("effect3", Effect "effect3" (Arbitrary [(Coordinate 0, All)])
                              (EffectRequirement Eq 0) "some. descrp")],
    candyMap = Map.empty
    }


validCandyInput1 :: String
validCandyInput1 =
  "\n// Comment\n\n\
  \shape_name: CandyShape1\n\
  \shape_icon: 🍬\n\
  \effect_name: effect1\n"

validCandy1 :: Candy
validCandy1 = Candy {
    candyDef = CandyDefinition {
        shapeName = "CandyShape1",
        shapeIcon = "🍬",
        effectNameRef = "effect1"
    },
    candyEffect = Effect {
        effectName = "effect1",
        effectRange = Circle 1,
        effectRequirement = EffectRequirement Ge 0,
        effectDescription = "some descrp"
    }
}

validCandyInput2 :: String
validCandyInput2 =
  "\n// Comment\n\n\
  \shape_name: CandyShape2\n\
  \shape_icon: 🍬\n\
  \effect_name: effect2\n"

validCandy2 :: Candy
validCandy2 = Candy {
    candyDef = CandyDefinition {
        shapeName = "CandyShape2",
        shapeIcon = "🍬",
        effectNameRef = "effect2"
    },
    candyEffect = Effect {
        effectName = "effect2",
        effectRange = Diamond 4,
        effectRequirement = EffectRequirement Gt 0,
        effectDescription = "some de.scrp"
    }
}

validCandy3 :: Candy
validCandy3 = Candy {
    candyDef = CandyDefinition {
        shapeName = "CandyShape3",
        shapeIcon = "🍬",
        effectNameRef = "effect1"
    },
    candyEffect = Effect {
        effectName = "effect1",
        effectRange = Circle 1,
        effectRequirement = EffectRequirement Ge 0,
        effectDescription = "some descrp"
    }
} 
validCandyInput3 :: String
validCandyInput3 =
  "\n// Comment\n\n\
  \shape_name: CandyShape3\n\
  \shape_icon: 🍬\n\
  \effect_name: effect1\n"

testValidCandyP :: Test
testValidCandyP = TestList
  [ "Valid candy input 1" ~:
      doParse candiesP (ParseState validCandyInput1 1 testDifficultyWithSomeEffects)
      ~?= Right ((), ParseState "" 7 (testDifficultyWithSomeEffects { candyMap = Map.singleton "CandyShape1" validCandy1 }))
    , "Valid candy input 2" ~:
      doParse candiesP (ParseState validCandyInput2 1 testDifficultyWithSomeEffects)
      ~?= Right ((), ParseState "" 7 (testDifficultyWithSomeEffects { candyMap = Map.singleton "CandyShape2" validCandy2 }))
    , "Valid candy input 3" ~:
        doParse candiesP (ParseState validCandyInput3 1 testDifficultyWithSomeEffects)
        ~?= Right ((), ParseState "" 7 (testDifficultyWithSomeEffects { candyMap = Map.singleton "CandyShape3" validCandy3 }))
  ]

-- Missing shape_name
invalidCandyMissingShapeNameTag :: String
invalidCandyMissingShapeNameTag =
  "\n// Comment\n\
  \shape_icon: 🍬\n\
  \effect_name: effect1\n"

invalidCandyMissingShapeNameValue :: String
invalidCandyMissingShapeNameValue =
  "\n// Comment\n\
  \shape_name: \n\
  \shape_icon: 🍬\n\
  \effect_name: effect1\n"

-- Missing shape_icon
invalidCandyMissingShapeIconTag :: String
invalidCandyMissingShapeIconTag =
  "\n// Comment\n\
  \shape_name: CandyShape\n\
  \effect_name: effect2\n"

invalidCandyMissingShapeIconValue :: String
invalidCandyMissingShapeIconValue =
  "\n// Comment\n\
  \shape_name: CandyShape\n\
  \shape_icon: \n\
  \effect_name: effect2\n"

-- Missing effect_name
invalidCandyMissingEffectTag :: String
invalidCandyMissingEffectTag =
  "\n// Comment\n\
  \shape_name: CandyShape1\n\
  \shape_icon: 🍬\n"

invalidCandyMissingEffectValue :: String
invalidCandyMissingEffectValue =
  "\n// Comment\n\
  \shape_name: CandyShape2\n\
  \shape_icon: 🍬\n\
  \effect_name: \n"

invalidCandyUndefinedEffectName :: String
invalidCandyUndefinedEffectName =
  "\n// Comment\n\
  \shape_name: CandyShape3\n\
  \shape_icon: 🍬\n\
  \effect_name: UndefinedEffect\n"

testInvalidCandyP :: Test
testInvalidCandyP = TestList
  [ "Missing shape_icon tag" ~: assertIsFatalError
        (doParse candiesP (ParseState invalidCandyMissingShapeIconTag 1 testDifficultyWithSomeEffects))
        "Expected error due to missing shape_icon"
    , "Missing shape_name tag" ~: assertIsFatalError
            (doParse candiesP (ParseState invalidCandyMissingShapeNameTag 1 testDifficultyWithSomeEffects))
            "Expected error due to missing shape_name"
    , "Missing effect_name tag" ~: assertIsFatalError
            (doParse candiesP (ParseState invalidCandyMissingEffectTag 1 testDifficultyWithSomeEffects))
            "Expected error due to missing effect_name"
    , "Missing shape_icon value" ~: assertIsFatalError
            (doParse candiesP (ParseState invalidCandyMissingShapeIconValue 1 testDifficultyWithSomeEffects))
            "Expected error due to missing shape_icon value"
    , "Missing shape_name value" ~: assertIsFatalError
        (doParse candiesP (ParseState invalidCandyMissingShapeNameValue 1 testDifficultyWithSomeEffects))
        "Expected error due to missing shape_name value"
    , "Missing effect_name value" ~: assertIsFatalError
        (doParse candiesP (ParseState invalidCandyMissingEffectValue 1 testDifficultyWithSomeEffects))
        "Expected error due to missing effect_name value"
    , "Undefined effect name" ~: assertIsFatalError
        (doParse candiesP (ParseState invalidCandyUndefinedEffectName 1 testDifficultyWithSomeEffects))
        "Expected error due to undefined effect name"
  ]


-- Invalid candies input: missing effect_name for one candy
invalidCandiesInputMissingEffectName :: String
invalidCandiesInputMissingEffectName =
  "\n// Comment\n\
  \shape_name: CandyShape1\n\
  \shape_icon: 🍬\n\
  \effect_name: effect1\n\
  \shape_name: CandyShape2\n\
  \shape_icon: 🍭\n\
  \effect_name: \n\
  \shape_name: CandyShape3\n\
  \shape_icon: 🍫\n\
  \effect_name: effect3\n"

-- Invalid candies input: undefined effect_name
invalidCandiesInputUndefinedEffectName :: String
invalidCandiesInputUndefinedEffectName =
  "\n// Comment\n\
  \shape_name: CandyShape1\n\
  \shape_icon: 🍬\n\
  \effect_name: effect1\n\
  \shape_name: CandyShape2\n\
  \shape_icon: 🍭\n\
  \effect_name: UndefinedEffect\n\
  \shape_name: CandyShape3\n\
  \shape_icon: 🍫\n\
  \effect_name: effect3\n"


testCandiesP :: Test
testCandiesP = TestList
  [ "Valid candies input" ~:
      doParse candiesP (ParseState (validCandyInput1 ++ "\n\n//\n" 
                                    ++ validCandyInput2 ++ "\n\n//\n"
                                    ++ validCandyInput3) 1 testDifficultyWithSomeEffects)
      ~?= Right ((), ParseState "" 25 testDifficultyWithSomeEffects {
          candyMap = Map.fromList [
              ("CandyShape1", validCandy1),
              ("CandyShape2", validCandy2),
              ("CandyShape3", validCandy3)
          ]
      })

  , "Invalid candies input: missing effect_name" ~:
      assertIsFatalError
        (doParse candiesP (ParseState invalidCandiesInputMissingEffectName 1 testDifficultyWithSomeEffects))
        "Expected a FatalError for missing effect_name value"

  , "Invalid candies input: undefined effect_name" ~:
      assertIsFatalError
        (doParse candiesP (ParseState invalidCandiesInputUndefinedEffectName 1 testDifficultyWithSomeEffects))
        "Expected a FatalError for undefined effect_name reference"
    , "duplicate candy overwrites the previous one" ~:
        doParse candiesP (ParseState (validCandyInput1 ++ "\n\n//\n" 
                                    ++ validCandyInput2 ++ "\n\n//\n"
                                    ++ validCandyInput3 ++ "\n\n//\n"
                                    ++ "shape_name: CandyShape1\n\
                                        \shape_icon: 🍬\n\
                                        \effect_name: effect3\n") 1 testDifficultyWithSomeEffects)
        ~?= Right ((), ParseState "" 31 testDifficultyWithSomeEffects {
            candyMap = Map.fromList [
                ("CandyShape1", validCandy1 { 
                    candyDef = (candyDef validCandy1) { effectNameRef = "effect3" },
                    candyEffect = Effect "effect3" (Arbitrary [(Coordinate 0, All)]) 
                                (EffectRequirement Eq 0) "some. descrp"
                    }),
                ("CandyShape2", validCandy2),
                ("CandyShape3", validCandy3 )
            ]
        })
  ]

testCandies :: Test
testCandies = TestList
  [ testValidCandyP
  , testInvalidCandyP
  , testCandiesP
  ]

allParserUnitTests :: Test
allParserUnitTests = TestList
    [ testIntP
    , testBetween
    , testSepBy
    , testSepBy1
    , testString
    , testStringP
    , testStripP
    , testParens
    , testEmptyLine
    , testCommentLine
    , testSkipCommentOrEmptyLines
    , testBrackets
    , testConstP
    , testCoordinateP
    , testCoordinatePairP
    , testCoordinateListP
    , testCirclePIgnoreCase
    , testRectanglePIgnoreCase
    , testDiamondPIgnoreCase
    , testArbitraryPIgnoreCase
    , testEffectRangeP
    , testEffectRangeLineP
    , testEffectNameP
    , testOperatorP
    , testEffectRequirementP
    , testEffectDescriptionP
    , testEffectP
    , testDifficultyConstantP
    , testValidCandyP
    ]

runAllParserUnitTests :: IO Counts
runAllParserUnitTests = runTestTT allParserUnitTests