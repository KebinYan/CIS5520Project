module CandyCrushParserTest where
import CandyCrushParser
import GeneralStateParser
import Phd
import TestUtils
import Test.HUnit
import Data.Map (empty)
import Data.Map qualified as Map


-- assert that the result is a success and the expected value is returned
assertIsSuccess :: (Show a, Eq b, Show b) => Either a b -> b -> String -> IO ()
assertIsSuccess result expected errMsg =
  case result of
    -- parsing successful and expected value returned
    Right x | x == expected -> return () 
            | otherwise -> error $ errMsg ++ ", but got: " ++ show x
    Left e -> error $ errMsg ++ ", but got error: " ++ show e

-- assert that the result is a fatal error
assertIsFatalError :: (Show b) => Either ParseError b -> String -> IO ()
assertIsFatalError result errMsg =
  case result of
    Left (FatalError _ _) -> return () -- fatal error, test passed
    Left (FailError e line) -> error $ errMsg ++ ", but got fail error at line " 
        ++ show line ++ ": " ++ e
    Right x -> error $ errMsg ++ ", but got: " ++ show x

-- assert that the result is a fail error
assertIsFailError :: (Show b) => Either ParseError b -> String -> IO ()
assertIsFailError result errMsg =
  case result of
    Left (FailError _ _) -> return () -- fail error, test passed
    Left (FatalError e line) -> error $ errMsg ++ 
        ", but got fatal error at line " ++ show line ++ ": " ++ e
    Right x -> error $ errMsg ++ ", but got: " ++ show x


testIntP :: Test
testIntP = TestList
  [ "Valid positive integer" ~: TestCase $ assertIsSuccess
        (doParse intP (CandyFileParser "123 abc" 1 defaultGameConst))
        (123, CandyFileParser " abc" 1 defaultGameConst)
        "Expected to parse a valid positive integer"
  , "Valid negative integer" ~:TestCase $ assertIsSuccess
        (doParse intP (CandyFileParser "-456 xyz" 1 defaultGameConst))
        (-456, CandyFileParser " xyz" 1 defaultGameConst)
        "Expected to parse a valid negative integer"
  , "Empty input" ~: assertIsFatalError
        (doParse intP (CandyFileParser "" 1 defaultGameConst))
        "Expected an error for empty input"
  , "Invalid integer with only '-'" ~: assertIsFatalError
        (doParse intP (CandyFileParser "- xyz" 1 defaultGameConst))
        "Expected an error for invalid '-' input"
  , "Trailing invalid characters" ~: assertIsFatalError
        (doParse intP (CandyFileParser "123.45 xyz" 1 defaultGameConst))
        "Expected an error for trailing invalid characters"
  , "Extra characters" ~: assertIsFatalError
        (doParse intP (CandyFileParser "123abc" 1 defaultGameConst))
        "Expected to stop parsing at the first invalid character"
  ]

testBetween :: Test
testBetween = TestList [
    "Between parentheses" ~:
        doParse (between (stringP "(") (string "content") (stringP ")"))
        (CandyFileParser "(content)" 1 defaultGameConst)
        ~?= Right ("content", CandyFileParser "" 1 defaultGameConst),
    "Between brackets with space" ~:
        doParse (between (stringP "[") (string "content") (stringP "]"))
        (CandyFileParser "[ content ]" 1 defaultGameConst)
        ~?= Right ("content", CandyFileParser "" 1 defaultGameConst),
    "inalid between mismatched brackets" ~:
        TestCase $ case doParse (between (stringP "[") 
            (string "content") (stringP "]")) 
            (CandyFileParser "[content)" 1 defaultGameConst) of
            Left (FailError _ _) -> return ()
            _ -> assertFailure "Expected a FailError for mismatched brackets"
    ]
testSepBy :: Test
testSepBy = TestList [
    "Comma-separated values" ~:
        doParse (sepBy (string "item") (stringP ","))
        (CandyFileParser "item,item,item" 1 defaultGameConst)
        ~?= Right (["item", "item", "item"], 
                    CandyFileParser "" 1 defaultGameConst),
    "Single item" ~:
        doParse (sepBy (string "item") (stringP ","))
        (CandyFileParser "item" 1 defaultGameConst)
        ~?= Right (["item"], CandyFileParser "" 1 defaultGameConst),
    "Empty list" ~:
        doParse (sepBy (string "item") (stringP ","))
        (CandyFileParser "" 1 defaultGameConst)
        ~?= Right ([], CandyFileParser "" 1 defaultGameConst)
    ]

testSepBy1 :: Test
testSepBy1 = TestList [
    "Comma-separated values" ~:
        doParse (sepBy1 (string "item") (stringP ","))
        (CandyFileParser "item,item,item" 1 defaultGameConst)
        ~?= Right (["item", "item", "item"], 
                    CandyFileParser "" 1 defaultGameConst),
    "Single item" ~:
        doParse (sepBy1 (string "item") (stringP ","))
        (CandyFileParser "item" 1 defaultGameConst)
        ~?= Right (["item"], CandyFileParser "" 1 defaultGameConst),
    "Empty list" ~:
        TestCase $ case doParse (sepBy1 (string "item") (stringP ","))
                (CandyFileParser "" 1 defaultGameConst) of
            Left (FailError _ _) -> return ()
            _ -> assertFailure "Expected a FailError for an empty list"
    ]

testBrackets :: Test
testBrackets = TestList [
    "Value inside brackets" ~:
        doParse (brackets (string "inner"))
        (CandyFileParser "[inner]" 1 defaultGameConst)
        ~?= Right ("inner", CandyFileParser "" 1 defaultGameConst),
    "Mismatched brackets" ~:
        TestCase $ case doParse (brackets (string "inner"))
                (CandyFileParser "[inner)" 1 defaultGameConst) of
            Left (FailError _ _) -> return ()
            _ -> assertFailure "Expected a FailError for mismatched brackets"
    ]

testParens :: Test
testParens = TestList [
    "Value inside parentheses" ~:
        doParse (parens (string "inner"))
        (CandyFileParser "(inner)" 1 defaultGameConst)
        ~?= Right ("inner", CandyFileParser "" 1 defaultGameConst),
    "Missing closing parenthesis" ~:
        TestCase $ case doParse (parens (string "inner"))
                (CandyFileParser "(inner" 1 defaultGameConst) of
            Left (FailError _ _) -> return ()
            _ -> assertFailure 
                "Expected a FailError for missing closing parenthesis"
    ]

testConstP :: Test
testConstP = TestList [
    "Constant parsing with exact match (no trailing whitespaces)" ~:
        doParse (constP "constant" 42)
        (CandyFileParser "constant followed by text" 1 defaultGameConst)
        ~?= Right (42, CandyFileParser "followed by text" 1 defaultGameConst),
    "Mismatch for constant" ~:
        TestCase $ case doParse (constP "constant" 42)
                (CandyFileParser "wrong input" 1 defaultGameConst) of
            Left (FailError _ _) -> return ()
            _ -> assertFailure "Expected a FailError for mismatched constant"
    ]

testStripP :: Test 
testStripP = TestList
    [ "stripP" ~: do
        let input = "  hello \n "
        let expected = "hello"
        let actual = doParse (stripP (pure input)) 
                    (CandyFileParser "" 1 defaultGameConst)
        actual ~?= Right (expected, CandyFileParser "" 1 defaultGameConst)
    ]

testString :: Test
testString = TestList [
    "Exact match (no trailing whitespaces)" ~:
        doParse (string "hello")
        (CandyFileParser "hello world" 1 defaultGameConst)
        ~?= Right ("hello", CandyFileParser " world" 1 defaultGameConst),
    "Mismatch" ~:
        TestCase $ case doParse (string "hello")
                (CandyFileParser "hi world" 1 defaultGameConst) of
            Left (FailError _ _) -> return ()
            _ -> assertFailure "Expected a FailError for mismatch"
    ]

testStringP :: Test
testStringP = TestList [
    "Exact match with whitespace" ~:
        doParse (stringP "hello")
        (CandyFileParser "  hello  world" 1 defaultGameConst)
        ~?= Right ((), CandyFileParser "world" 1 defaultGameConst),
    "Mismatch" ~:
        TestCase $ case doParse (stringP "hello")
                (CandyFileParser "  hi world" 1 defaultGameConst) of
            Left (FailError _ _) -> return ()
            _ -> assertFailure "Expected a FailError for mismatch"
    ]

testEmptyLine :: Test
testEmptyLine = TestList
  [ "empty line should parse correctly" ~:
      doParse emptyLine (CandyFileParser "  \n" 1 defaultGameConst)
      ~?= Right ((), CandyFileParser "" 2 defaultGameConst)
    , "parse one empty line a time 1" ~:
      doParse emptyLine (CandyFileParser "  \n  \n" 1 defaultGameConst)
      ~?= Right ((), CandyFileParser "\n" 2 defaultGameConst)
    , "parse one empty line a time 2" ~:
    doParse emptyLine (CandyFileParser "\n\n" 1 defaultGameConst)
      ~?= Right ((), CandyFileParser "\n" 2 defaultGameConst)
    , "no empty line should fail" ~: assertIsFailError
        (doParse emptyLine 
            (CandyFileParser "This is not an empty line" 1 defaultGameConst))
        "Expected a fail error whe no empty line"
  ]
testCommentLine :: Test
testCommentLine = TestList
  [ "single-line comment should parse correctly" ~:
      doParse commentLine 
        (CandyFileParser "// This is a comment\n" 1 defaultGameConst)
      ~?= Right ((), CandyFileParser "" 2 defaultGameConst)
    , "parse one empty line a time" ~:
    doParse commentLine 
        (CandyFileParser "// This is a comment\n// another comment\n" 1 
        defaultGameConst)
    ~?= Right ((), CandyFileParser "// another comment\n" 2 defaultGameConst)
  , "non-comment line should fail" ~: assertIsFailError
      (doParse commentLine 
        (CandyFileParser "This is not a comment" 1 defaultGameConst))
      "Expected a fail error when no comment line"
  ]
testSkipCommentOrEmptyLines :: Test
testSkipCommentOrEmptyLines = TestList
  [ "skip empty lines and comments" ~:
      doParse skipCommentOrEmptyLines 
        (CandyFileParser "\n// comment\n\n// another comment\n" 1 
            defaultGameConst)
      ~?= Right ((), CandyFileParser "" 5 defaultGameConst)
  , "skip empty lines and comment with non-empty content after" ~:
      doParse skipCommentOrEmptyLines 
        (CandyFileParser "// comment\nSome content\n" 1 defaultGameConst)
      ~?= Right ((), CandyFileParser "Some content\n" 2 defaultGameConst)
  , "skip comments and empty lines with content after" ~:
      doParse skipCommentOrEmptyLines 
        (CandyFileParser "// comment\n\nSome content\n" 1 defaultGameConst)
      ~?= Right ((), CandyFileParser "Some content\n" 3 defaultGameConst)
  , "should not skip non-comment or non-empty lines" ~:
      doParse skipCommentOrEmptyLines 
        (CandyFileParser "Non-comment line" 1 defaultGameConst)
      ~?= Right ((), CandyFileParser "Non-comment line" 1 defaultGameConst)
  ]
testCoordinateP :: Test
testCoordinateP = TestList [
    "Specific Pos coordinate" ~: assertIsSuccess
        (doParse coordinateP ( CandyFileParser "5" 1 defaultGameConst))
        (Coordinate 5, CandyFileParser "" 1 defaultGameConst)
        "Expected to parse a valid positive integer"
    , "Specific Neg coordinate" ~: assertIsSuccess
        (doParse coordinateP ( CandyFileParser "-5" 1 defaultGameConst))
        (Coordinate (-5), CandyFileParser "" 1 defaultGameConst)
        "Expected to parse a valid negative integer"
    , "Wildcard coordinate" ~: assertIsSuccess
        (doParse coordinateP ( CandyFileParser ":" 1 defaultGameConst))
        (All, CandyFileParser "" 1 defaultGameConst)
        "Expected to parse a wildcard coordinate"
    , "Invalid coordinate" ~: assertIsFatalError
        (doParse coordinateP ( CandyFileParser "abc" 1 defaultGameConst))
        "Expected a fatal error for invalid input"
    ]
testCoordinatePairP :: Test
testCoordinatePairP = TestList
    [
        "Valid coordinate pair 1" ~: doParse coordinatePairP 
            (CandyFileParser "( -100 , 2 )" 1 defaultGameConst)
            ~?= Right ((Coordinate (-100), Coordinate 2), 
                        CandyFileParser "" 1 defaultGameConst)
        ,"Valid coordinate pair 2" ~: doParse coordinatePairP 
            (CandyFileParser "(1,1)" 1 defaultGameConst)
            ~?= Right ((Coordinate 1, Coordinate 1), 
                        CandyFileParser "" 1 defaultGameConst)
        ,"Valid coordinate pair 3" ~: doParse coordinatePairP 
            (CandyFileParser "(-1, 1000)" 1 defaultGameConst)
            ~?= Right ((Coordinate (-1), Coordinate 1000), 
                        CandyFileParser "" 1 defaultGameConst)
        ,"Valid coordinate pair 4" ~: doParse coordinatePairP 
            (CandyFileParser "( 1000,-1)" 1 defaultGameConst)
            ~?= Right ((Coordinate 1000, Coordinate (-1)), 
                        CandyFileParser "" 1 defaultGameConst)
         ,"Valid coordinate pair 5" ~: doParse coordinatePairP 
            (CandyFileParser "(-1,-1)" 1 defaultGameConst)
            ~?= Right ((Coordinate (-1), Coordinate (-1)), 
                        CandyFileParser "" 1 defaultGameConst)
        , "Wildcard Pos Pair" ~: doParse coordinatePairP 
            (CandyFileParser "( 1  , :)" 1 defaultGameConst)
            ~?= Right ((Coordinate 1, All), 
                        CandyFileParser "" 1 defaultGameConst)
        , "Wildcard Neg Pair" ~: doParse coordinatePairP 
            (CandyFileParser "( :, -2  )" 1 defaultGameConst)
            ~?= Right ((All, Coordinate (-2)), 
                        CandyFileParser "" 1 defaultGameConst)
        , "Wildcard pair" ~: doParse coordinatePairP 
            (CandyFileParser "( : , : )" 1 defaultGameConst)
            ~?= Right ((All, All), CandyFileParser "" 1 defaultGameConst)
        , "Invalid coordinate pair 1" ~: assertIsFatalError
            (doParse coordinatePairP 
                (CandyFileParser "(1, abc)" 1 defaultGameConst))
            "Expected a fatal error for invalid input"
        ,  "Valid brackets with single pair" ~: doParse coordinateListP 
            (CandyFileParser "[(1,2)]" 1 defaultGameConst)
            ~?= Right ([(Coordinate 1, Coordinate 2)], 
                        CandyFileParser "" 1 defaultGameConst)
        ,  "Valid brackets with multiple pairs" ~: doParse coordinateListP 
            (CandyFileParser "[(1,2),(3,4)]" 1 defaultGameConst)
            ~?= Right ([(Coordinate 1, Coordinate 2), 
                        (Coordinate 3, Coordinate 4)], 
                        CandyFileParser "" 1 defaultGameConst)
        , "Empty brackets" ~:
            doParse coordinateListP (CandyFileParser "[]" 1 defaultGameConst)
            ~?= Right ([], CandyFileParser "" 1 defaultGameConst)
        , "testCoordinatePairP Missing closing bracket" ~: assertIsFailError
            (doParse coordinateListP 
                (CandyFileParser "[(1,2),(3,4)" 1 defaultGameConst))
            "Expected a fatal error for missing closing bracket"
        , "Invalid coordinate pair 2" ~: assertIsFailError
            (doParse coordinateListP 
                (CandyFileParser "[(1,2),(3)]" 1 defaultGameConst))
            "Expected a fatal error for invalid coordinate pair"
    ]
testArbitraryPIgnoreCase :: Test
testArbitraryPIgnoreCase = TestList
    [ "Arbitrary with single coordinate pair" ~:
        doParse arbitraryP 
            (CandyFileParser "Arbitrary [(1,2)]" 1 defaultGameConst)
        ~?= Right (Arbitrary [(Coordinate 1, Coordinate 2)], 
                    CandyFileParser "" 1 defaultGameConst)
    , "Arbitrary with wildcard coordinates" ~:
        doParse arbitraryP 
            (CandyFileParser "Arbitrary [(0,:)]" 1 defaultGameConst)
        ~?= Right (Arbitrary [(Coordinate 0, All)], 
                    CandyFileParser "" 1 defaultGameConst)
    , "Arbitrary with multiple coordinate pairs" ~: doParse arbitraryP 
        (CandyFileParser "Arbitrary [(1,2),(:,:),(-2,12)]" 1 defaultGameConst)
        ~?= Right (Arbitrary [(Coordinate 1, Coordinate 2), 
                                (All, All), 
                                (Coordinate (-2), Coordinate 12)], 
                                CandyFileParser "" 1 defaultGameConst)
    , "testArbitraryPIgnoreCase Arbitrary with missing closing bracket" ~: 
        assertIsFatalError
            (doParse arbitraryP (CandyFileParser 
                "Arbitrary [(1,2),(:,:),(-2,12)" 1 defaultGameConst))
            "Expected a fatal error for missing closing bracket"
    , "Arbitrary with invalid coordinate tuple" ~: assertIsFatalError
        (doParse arbitraryP 
            (CandyFileParser "Arbitrary [(1,2),(1,2,3)]" 1 defaultGameConst))
        "Expected a fatal error for invalid tuple"
    ]

testCoordinateListP :: Test
testCoordinateListP = TestList
    [
        "Empty list" ~: doParse coordinateListP 
            (CandyFileParser "[]" 1 defaultGameConst)
            ~?= Right ([], CandyFileParser "" 1 defaultGameConst)
        , "Single pair" ~: doParse coordinateListP 
            (CandyFileParser "[ (1 , 2 ) ]" 1 defaultGameConst)
            ~?= Right ([(Coordinate 1, Coordinate 2)], 
                        CandyFileParser "" 1 defaultGameConst)
        , "Multiple pairs" ~: doParse coordinateListP 
            (CandyFileParser "[ ( 1,2 ), (:,:), (-2, 12)]" 1 defaultGameConst)
            ~?= Right ([(Coordinate 1, Coordinate 2), (All, All), 
                        (Coordinate (-2), Coordinate 12)], 
                        CandyFileParser "" 1 defaultGameConst)
    ]

testCirclePIgnoreCase :: Test
testCirclePIgnoreCase = TestList
    [ "circle 3" ~: doParse circleP 
        (CandyFileParser "Circle 3" 1 defaultGameConst)
        ~?= Right (Circle 3, CandyFileParser "" 1 defaultGameConst)
    , "cirCle 0" ~: doParse circleP 
        (CandyFileParser "cirCle 0" 1 defaultGameConst)
        ~?= Right (Circle 0, CandyFileParser "" 1 defaultGameConst)
    , "cIrcLe -3" ~: doParse circleP 
        (CandyFileParser "cIrcLe -3" 1 defaultGameConst)
        ~?= Right (Circle (-3), CandyFileParser "" 1 defaultGameConst)
    , "CIRCLE 100" ~: doParse circleP 
        (CandyFileParser "CIRCLE 100" 1 defaultGameConst)
        ~?= Right (Circle 100, CandyFileParser "" 1 defaultGameConst)
    , "circle 3.15" ~: assertIsFatalError
        (doParse circleP 
            (CandyFileParser "Circle 3.15" 1 defaultGameConst))
        "Expected a fatal error for invalid decimal radius"
    , "CIRCLE 3 4 5" ~: doParse circleP 
        (CandyFileParser "CIRCLE 3 4 5" 1 defaultGameConst)
        ~?= Right (Circle 3, CandyFileParser "4 5" 1 defaultGameConst)
    , "CIRCLE 3 4" ~: doParse circleP 
        (CandyFileParser "CIRCLE 3 4" 1 defaultGameConst)
        ~?= Right (Circle 3, CandyFileParser "4" 1 defaultGameConst)
    , "circle a" ~: assertIsFatalError
        (doParse circleP (CandyFileParser "circle a" 1 defaultGameConst))
        "Expected a fatal error for invalid input"
    ]
testRectanglePIgnoreCase :: Test
testRectanglePIgnoreCase = TestList
    [ "Rectangle with positive dimensions" ~:
        doParse rectangleP (CandyFileParser "Rectangle 3 4" 1 defaultGameConst)
        ~?= Right (Rectangle 3 4, CandyFileParser "" 1 defaultGameConst)
    , "Rectangle with zero dimensions" ~:
        doParse rectangleP (CandyFileParser "ReCtAnGlE 0 0" 1 defaultGameConst)
        ~?= Right (Rectangle 0 0, CandyFileParser "" 1 defaultGameConst)
    , "Rectangle with mixed dimensions" ~:
        doParse rectangleP (CandyFileParser "rEctAngLe -3 4" 1 defaultGameConst)
        ~?= Right (Rectangle (-3) 4, CandyFileParser "" 1 defaultGameConst)
    , "Rectangle with trailing input" ~:
        doParse rectangleP 
            (CandyFileParser "RECTANGLE 3 4 5 6 7" 1 defaultGameConst)
        ~?= Right (Rectangle 3 4, CandyFileParser "5 6 7" 1 defaultGameConst)
    , "Rectangle with missing dimension" ~: assertIsFatalError
        (doParse rectangleP (CandyFileParser "rectangle 3" 1 defaultGameConst))
        "Expected a fail error for missing dimension"
    , "Rectangle with invalid input" ~: assertIsFatalError
        (doParse rectangleP 
            (CandyFileParser "rectangle a 4" 1 defaultGameConst))
        "Expected a fatal error for invalid input"
    ]
testDiamondPIgnoreCase :: Test
testDiamondPIgnoreCase = TestList
    [ "Diamond with positive radius" ~:
        doParse diamondP (CandyFileParser "Diamond 3" 1 defaultGameConst)
        ~?= Right (Diamond 3, CandyFileParser "" 1 defaultGameConst)
    , "Diamond with zero radius" ~:
        doParse diamondP (CandyFileParser "DIAMOND 0" 1 defaultGameConst)
        ~?= Right (Diamond 0, CandyFileParser "" 1 defaultGameConst)
    , "Diamond with negative radius" ~:
        doParse diamondP (CandyFileParser "dIaMoNd -3" 1 defaultGameConst)
        ~?= Right (Diamond (-3), CandyFileParser "" 1 defaultGameConst)
    , "Diamond with trailing input" ~:
        doParse diamondP (CandyFileParser "DIAMOND 3 4" 1 defaultGameConst)
        ~?= Right (Diamond 3, CandyFileParser "4" 1 defaultGameConst)
    , "Diamond with invalid input" ~: assertIsFatalError
        (doParse diamondP (CandyFileParser "diamond a" 1 defaultGameConst))
        "Expected a fatal error for invalid input"
    ]

testEffectRangeP :: Test
testEffectRangeP = TestList
    [ "testEffectRangeP valid Circle" ~: doParse effectRangeP 
        (CandyFileParser "cirCle 3" 1 defaultGameConst)
          ~?= Right (Circle 3, CandyFileParser "" 1 defaultGameConst)
    ,"testEffectRangeP invalid Circle 1" ~: assertIsFatalError
        (doParse effectRangeP 
            (CandyFileParser "Circle 3.15" 1 defaultGameConst))
        "Expected a fatal error for invalid decimal radius"
    , "testEffectRangeP valid Rectangle" ~: doParse effectRangeP 
        (CandyFileParser "Rectangle 3 4" 1 defaultGameConst)
          ~?= Right (Rectangle 3 4, CandyFileParser "" 1 defaultGameConst)
    , "testEffectRangeP valid Diamond" ~: doParse effectRangeP 
        (CandyFileParser "Diamond 5" 1 defaultGameConst)
          ~?= Right (Diamond 5, CandyFileParser "" 1 defaultGameConst)
    , "testEffectRangeP valid Arbitrary" ~: doParse effectRangeP
        (CandyFileParser "Arbitrary [(1,2), (:,:)]" 1 defaultGameConst)
          ~?= Right (Arbitrary [(Coordinate 1, Coordinate 2), (All, All)], 
                                CandyFileParser "" 1 defaultGameConst)
    , "testEffectRangeP invalid Arbitrary" ~: assertIsFatalError
        (doParse effectRangeP 
            (CandyFileParser "Arbitrary [(1.3,2), (:,:)]" 1 defaultGameConst))
        "Expected a fatal error for unrecognized effect range type"
    , "Unrecognized" ~: assertIsFatalError
        (doParse effectRangeP 
            (CandyFileParser "InvalidRange 7" 1 defaultGameConst))
        "Unrecognized effect range type"
    ]

testEffectNameP :: Test
testEffectNameP = TestList
  [ "Valid effect name" ~:
      doParse effectNameP 
        (CandyFileParser "effect_name:StripedRow\n" 1 defaultGameConst)
      ~?= Right ("StripedRow", CandyFileParser "" 2 defaultGameConst)
    , "Effect name with spaces" ~:
        doParse effectNameP 
            (CandyFileParser "effect_name:Striped Row\n" 1 defaultGameConst)
        ~?= Right ("Striped Row", CandyFileParser "" 2 defaultGameConst)
    , "testEffectNameP Missing effect name" ~: assertIsFatalError
        (doParse effectNameP 
            (CandyFileParser "effect_name:\n" 1 defaultGameConst))
        "Expected a FatalError for missing effect name"
    , "Missing EOF" ~: assertIsFatalError
        (doParse effectNameP 
            (CandyFileParser "effect_name:StripedRow" 1 defaultGameConst))
        "Expected a FatalError for missing newline"
  ]
testEffectRangeLineP :: Test
testEffectRangeLineP = TestList
  [ "Valid effect range" ~:
      doParse effectRangeLineP 
        (CandyFileParser "effect_range: Arbitrary [(:,0)]\n" 1 defaultGameConst)
      ~?= Right 
        (Arbitrary [(All, Coordinate 0)], CandyFileParser "" 2 defaultGameConst)
  , "Invalid effect range" ~:  assertIsFatalError
    (doParse effectRangeLineP 
        (CandyFileParser "effect_range: InvalidRange\n" 1 defaultGameConst))
    "Expected a FatalError for invalid effect range"
  ]
testOperatorP :: Test
testOperatorP = TestList
  [ "Valid operators" ~: TestList
      [ doParse operatorP (CandyFileParser "=" 1 defaultGameConst) ~?= 
            Right (Eq, CandyFileParser "" 1 defaultGameConst)
        , doParse operatorP (CandyFileParser ">" 1 defaultGameConst) ~?= 
            Right (Gt, CandyFileParser "" 1 defaultGameConst)
        , doParse operatorP (CandyFileParser ">=\n" 1 defaultGameConst) ~?= 
            Right (Ge, CandyFileParser "\n" 1 defaultGameConst)
        , doParse operatorP (CandyFileParser "<" 1 defaultGameConst) ~?= 
            Right (Lt, CandyFileParser "" 1 defaultGameConst)
        , doParse operatorP (CandyFileParser "<=" 1 defaultGameConst) ~?= 
            Right (Le, CandyFileParser "" 1 defaultGameConst)
      ]
    , "unknown operator" ~: assertIsFailError
        (doParse operatorP (CandyFileParser "--" 1 defaultGameConst))
        "Expected a FailError for unknown operator"
  ]
testEffectRequirementP :: Test
testEffectRequirementP = TestList
    [
     "Valid effect requirement with =" ~:
        doParse effectRequirementP 
            (CandyFileParser "effect_requirement: = 5\n" 1 defaultGameConst)
        ~?= Right 
            (EffectRequirement Eq 5, CandyFileParser "" 2 defaultGameConst)
    , "Valid effect requirement with >" ~:
        doParse effectRequirementP 
        (CandyFileParser "effect_requirement: > 3\n" 1 defaultGameConst)
        ~?= Right 
            (EffectRequirement Gt 3, CandyFileParser "" 2 defaultGameConst)
    , "Invalid operator in requirement" ~:
        assertIsFatalError
            (doParse effectRequirementP 
            (CandyFileParser "effect_requirement: <= 5\n" 1 defaultGameConst))
            "Expected a FailError for invalid operator"
    , "Invalid negative number in requirement" ~:
        assertIsFatalError
            (doParse effectRequirementP 
            (CandyFileParser "effect_requirement: > -5\n" 1 defaultGameConst))
            "Expected a FailError for negative number"
    , "Invalid number in requirement" ~:
        assertIsFatalError
            (doParse effectRequirementP 
            (CandyFileParser "effect_requirement: > abc\n" 1 defaultGameConst))
            "Expected a FailError for invalid number"
    , "Missing number in requirement" ~:
        assertIsFatalError
            (doParse effectRequirementP 
            (CandyFileParser "effect_requirement: >\n" 1 defaultGameConst))
            "Expected a FailError for missing number"
    , "Missing operator in requirement" ~:
        assertIsFatalError
            (doParse effectRequirementP 
            (CandyFileParser "effect_requirement: 5\n" 1 defaultGameConst))
            "Expected a FailError for missing operator"
    , "Invalid operator in requirement" ~:
        assertIsFatalError
            (doParse effectRequirementP 
            (CandyFileParser "effect_requirement: >- 5\n" 1 defaultGameConst))
            "Expected a FatalError for invalid effect requirement"
  ]
testEffectDescriptionP :: Test
testEffectDescriptionP = TestList
  [ "Valid effect description" ~: doParse effectDescriptionP 
    (CandyFileParser "effect_description: Clears the row\n" 1 defaultGameConst)
    ~?= Right ("Clears the row", CandyFileParser "" 2 defaultGameConst)
    , "Missing effect description" ~: assertIsFatalError
    (doParse effectDescriptionP 
    (CandyFileParser "effect_description: \n" 1 defaultGameConst))
    "Expected a FatalError for missing effect description"
    , "Missing newline" ~: assertIsFatalError (doParse effectDescriptionP 
    (CandyFileParser "effect_description: Clears the row" 1 defaultGameConst))
     "Expected a FatalError for missing newline"
  ]

-- define the test data
normalEffectInput1 :: String
normalEffectInput1 = "effect_name: Normal\n\
                        \effect_range: Arbitrary [(0,0)]\n\
                        \effect_requirement: =0\n\
                        \effect_description: some str..\n"

stripedRowEffectStr :: String
stripedRowEffectStr = "effect_name: StripedRow\n\
                            \effect_range: Arbitrary [(:,0)]\n\
                            \effect_requirement: >0\n\
                            \effect_description: some str..\n"

wrappedEffectStr :: String
wrappedEffectStr = "effect_name: Wrapped\n\
                         \effect_range: Circle 3\n\
                         \effect_requirement: >=0\n\
                         \effect_description: some str..\n"

-- define the test data
normalEffect1 :: Effect
normalEffect1 = Effect
    { effectName = "Normal"
    , effectRange = Arbitrary [(Coordinate 0, Coordinate 0)]
    , effectRequirement = EffectRequirement Eq 0
    , effectDescription = "some str.."
    }

stripedRowEffect1 :: Effect
stripedRowEffect1 = Effect
    { effectName = "StripedRow"
    , effectRange = Arbitrary [(All, Coordinate 0)]
    , effectRequirement = EffectRequirement Gt 0
    , effectDescription = "some str.."
    }

wrappedEffect1 :: Effect
wrappedEffect1 = Effect
    { effectName = "Wrapped"
    , effectRange = Circle 3
    , effectRequirement = EffectRequirement Ge 0
    , effectDescription = "some str.."
    }
testValidEffectP :: Test
testValidEffectP = TestList
    [ "Valid Normal effect" ~:
        doParse effectP (CandyFileParser normalEffectInput1 1 defaultGameConst)
        ~?= Right ((), CandyFileParser "" 5 defaultGameConst
                    { effectMap = Map.insert "Normal" 
                    normalEffect1 (effectMap defaultGameConst) })
    , "Valid StripedRow effect" ~:
            doParse effectP 
                (CandyFileParser stripedRowEffectStr 1 defaultGameConst)
            ~?= Right ((), CandyFileParser "" 5 defaultGameConst
                        { effectMap = Map.insert "StripedRow" 
                        stripedRowEffect1 (effectMap defaultGameConst) })
    , "Valid Wrapped effect" ~:
            doParse effectP 
                (CandyFileParser wrappedEffectStr 1 defaultGameConst)
            ~?= Right ((), CandyFileParser "" 5 defaultGameConst
                        { effectMap = Map.insert "Wrapped" 
                        wrappedEffect1 (effectMap defaultGameConst) })
  ]
testValidEffectsP :: Test
testValidEffectsP = TestList
  [ "Valid effects definition" ~:
      doParse effectsP (CandyFileParser (normalEffectInput1 ++ "\n" ++ "\n"
                                    ++ stripedRowEffectStr) 1 defaultGameConst)
      ~?= Right ((), CandyFileParser "" 11 defaultGameConst
                    { effectMap = Map.fromList
                        [ ("Normal", normalEffect1)
                        , ("StripedRow", stripedRowEffect1)
                        ]
                    })
  , "Valid effects with comments and empty lines" ~:
      doParse effectsP (CandyFileParser ("// Comment\n" ++ normalEffectInput1
                                    ++ "\n\n" ++ stripedRowEffectStr
                                    ++ "\n//another comment\n" 
                                    ++ wrappedEffectStr) 1 defaultGameConst)
      ~?= Right ((), CandyFileParser "" 18 defaultGameConst
                    { effectMap = Map.fromList
                        [ ("Normal", normalEffect1)
                        , ("StripedRow", stripedRowEffect1)
                        , ("Wrapped", wrappedEffect1)
                        ]
                    })
    , "Valid effects without empty lines" ~:
        doParse effectsP (CandyFileParser (normalEffectInput1 
                                    ++ stripedRowEffectStr
                                    ++ wrappedEffectStr) 1 defaultGameConst)
        ~?= Right ((), CandyFileParser "" 13 defaultGameConst
                      { effectMap = Map.fromList
                          [ ("Normal", normalEffect1)
                          , ("StripedRow", stripedRowEffect1)
                          , ("Wrapped", wrappedEffect1)
                          ]
                      })
    , "Valid update for duplicate effect names" ~:
        doParse effectsP (CandyFileParser (normalEffectInput1
                                      ++ wrappedEffectStr
                                      ++ "effect_name: Normal\n\
                                        \effect_range: Rectangle 3 3\n\
                                        \effect_requirement: =0\n\
                                        \effect_description: some str..\n")
                                        1 defaultGameConst)
        ~?= Right ((), CandyFileParser "" 13 defaultGameConst
                        { effectMap = Map.fromList
                            [ ("Normal", Effect
                                { effectName = "Normal"
                                , effectRange = Rectangle 3 3
                                , effectRequirement = EffectRequirement Eq 0
                                , effectDescription = "some str.."
                                })
                            , ("Wrapped", wrappedEffect1)
                            ]
                        })
  ]

testInvalidEffectsP :: Test
testInvalidEffectsP = TestList
  [ "Invalid effects definition" ~:
      assertIsFatalError
        (doParse effectsP (CandyFileParser "effect_name: Normal\n\
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
                                    1 defaultGameConst))
        "Expected a FatalError for invalid effect range --3"
  , "Invalid effects definition with missing effect name" ~:
      assertIsFatalError
        (doParse effectsP (CandyFileParser "effect_name: Normal\n\
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
                                    1 defaultGameConst))
        "Expected a FatalError for missing effect name"
  ]

-- Invalid effect_name cases
testInvalidEffectNameP :: Test
testInvalidEffectNameP = TestList
  [ "Invalid effect_name tag" ~:
      assertIsFatalError
        (doParse effectP (CandyFileParser "invalid_effect_name: \n\
                                     \effect_range: Arbitrary [(0,0)]\n\
                                     \effect_requirement: =0\n\
                                     \effect_description: some str..\n"
                                     1 defaultGameConst))
        "Expected a FatalError for invalid effect name"
  , "Invalid effect_name value" ~:
      assertIsFatalError
        (doParse effectP (CandyFileParser "effect_name: \n\
                                     \effect_range: Arbitrary [(0,0)]\n\
                                     \effect_requirement: =0\n\
                                     \effect_description: some str..\n"
                                     1 defaultGameConst))
        "Expected a FatalError for invalid effect name"
  , "testInvalidEffectNameP Missing effect name" ~:
      assertIsFatalError
        (doParse effectP (CandyFileParser "effect_range: Arbitrary [(0,0)]\n\
                                     \effect_requirement: =0\n\
                                     \effect_description: some str..\n"
                                     1 defaultGameConst))
        "Expected a FatalError for missing effect name"
  ]

-- Invalid effect_range cases
testInvalidEffectRangeP :: Test
testInvalidEffectRangeP = TestList
  [ "Invalid effect_range tag" ~:
      assertIsFatalError
        (doParse effectP (CandyFileParser "effect_name: Normal\n\
                                     \invalid effect_range: Circle 3\n\
                                     \effect_requirement: =0\n\
                                     \effect_description: some str..\n"
                                     1 defaultGameConst))
        "Expected a FatalError for invalid effect range"
  , "Invalid effect_range value" ~:
      assertIsFatalError
        (doParse effectP (CandyFileParser "effect_name: Normal\n\
                                     \effect_range: InvalidRange\n\
                                     \effect_requirement: =0\n\
                                     \effect_description: some str..\n"
                                     1 defaultGameConst))
        "Expected a FatalError for invalid effect range"
  , "Missing effect_range" ~:
      assertIsFatalError
        (doParse effectP (CandyFileParser "effect_name: Normal\n\
                                     \effect_requirement: =0\n\
                                     \effect_description: some str..\n"
                                     1 defaultGameConst))
        "Expected a FatalError for missing effect range"
  ]

-- Invalid effect_requirement cases
testInvalidEffectRequirementP :: Test
testInvalidEffectRequirementP = TestList
  [ "Invalid effect_requirement value" ~:
      assertIsFatalError
        (doParse effectP (CandyFileParser "effect_name: Normal\n\
                                     \effect_range: Arbitrary [(0,0)]\n\
                                     \effect_requirement: abc\n\
                                     \effect_description: some str..\n"
                                     1 defaultGameConst))
        "Expected a FatalError for invalid effect requirement"
  , "Invalid effect_requirement value negative" ~:
      assertIsFatalError
        (doParse effectP (CandyFileParser "effect_name: Normal\n\
                                     \effect_range: Arbitrary [(0,0)]\n\
                                     \effect_requirement: >-1\n\
                                     \effect_description: some str..\n"
                                     1 defaultGameConst))
        "Expected a FatalError for invalid effect requirement"
  , "Missing effect_requirement" ~:
      assertIsFatalError
        (doParse effectP (CandyFileParser "effect_name: Normal\n\
                                     \effect_range: Arbitrary [(0,0)]\n\
                                     \effect_description: some str..\n"
                                     1 defaultGameConst))
        "Expected a FatalError for missing effect requirement"
  ]

-- Invalid effect_description cases
testInvalidEffectDescriptionP :: Test
testInvalidEffectDescriptionP = TestList
  [ "Invalid effect_description tag" ~:
      assertIsFatalError
        (doParse effectP (CandyFileParser "effect_name: Normal\n\
                                     \effect_range: Arbitrary [(0,0)]\n\
                                     \effect_requirement: =0\n\
                                     \invalid_effect_description: \n"
                                     1 defaultGameConst))
        "Expected a FatalError for invalid effect description"
  , "Missing effect_description" ~:
      assertIsFatalError
        (doParse effectP (CandyFileParser "effect_name: Normal\n\
                                     \effect_range: Arbitrary [(0,0)]\n\
                                     \effect_requirement: =0\n"
                                     1 defaultGameConst))
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

-- Test Action Parser
testParseAction :: Test
testParseAction = TestList [
    "Parse swap action" ~:
        parseAction hard "swap 1 2 3 4" ~?=
            Right (Swap (Coordinate 1, Coordinate 2) 
                        (Coordinate 3, Coordinate 4)),
    "Parse click action" ~:
        parseAction hard "click 1 2" ~?= 
            Right (Click (Coordinate 1, Coordinate 2)),
    "Parse undo action" ~:
        parseAction easy "undo" ~?= Right Undo,
    "Parse quit action" ~:
        parseAction medium "quit" ~?= Right Quit,
    "Parse invalid action" ~:
        parseAction easy "invalid" ~?= 
            Left (FailError "Expected string \"hint\" at line 1" 1)
  ]



validConstantInput :: String
validConstantInput =
  "\n// Comment\n\n\n\
  \game_constant\n\
  \dimension: 5\n\
  \max_steps: 10\n\
  \score_per_candy: 0\n\
  \score_per_effect: 0\n"

invalidDimensionTag :: String
invalidDimensionTag =
  "// Comment\n\
  \game_constant\n\
  \invalid_dimension: 3\n\
  \max_steps: 10\n\
  \score_per_candy: 0\n\
  \score_per_effect: 0\n"

invalidDimensionValue :: String
invalidDimensionValue =
  "// Comment\n\
  \game_constant\n\
  \dimension: 2\n\
  \max_steps: 10\n\
  \score_per_candy: 0\n\
  \score_per_effect: 0\n"

invalidMaxStepsTag :: String
invalidMaxStepsTag =
  "// Comment\n\
  \game_constant\n\
  \dimension: 3\n\
  \invalid_max_steps: 1\n\
  \score_per_candy: 0\n\
  \score_per_effect: 0\n"

invalidMaxStepsValue :: String
invalidMaxStepsValue =
  "// Comment\n\
  \game_constant\n\
  \dimension: 3\n\
  \max_steps: 1\n\
  \score_per_candy: 0\n\
  \score_per_effect: 0\n"

invalidBlockInput :: String
invalidBlockInput =
  "// Comment\n\
  \game_constant\n\
  \shape_name: Circle\n\
  \dimension: 3\n\
  \max_steps: 10\n"

invalidSequenceInput :: String
invalidSequenceInput =
  "// Comment\n\
  \game_constant\n\
  \max_steps: 10\n\
  \dimension: 3\n"

-- Test cases for gameConstantP
testGameConstantP :: Test
testGameConstantP = TestList
  [ "Valid input" ~: doParse gameConstantP 
    (CandyFileParser validConstantInput 1 defaultGameConst)
        ~?= Right ((), CandyFileParser "" 10 defaultGameConst 
        { dimension = 5, maxSteps = 10, scorePerCandy = 0, scorePerEffect = 0 })
    , "Invalid dimension tag" ~: assertIsFatalError
        (doParse gameConstantP 
        (CandyFileParser invalidDimensionTag 1 defaultGameConst))
        "Expected an error for invalid dimension tag"
    , "Invalid dimension value" ~: assertIsFatalError
        (doParse gameConstantP 
        (CandyFileParser invalidDimensionValue 1 defaultGameConst))
        "Expected an error for invalid dimension value"
    ,  "Invalid max_steps tag" ~: assertIsFatalError
        (doParse gameConstantP 
        (CandyFileParser invalidMaxStepsTag 1 defaultGameConst))
        "Expected an error for invalid max_steps tag"
    , "Invalid max_steps value" ~: assertIsFatalError
        (doParse gameConstantP 
        (CandyFileParser invalidMaxStepsValue 1 defaultGameConst))
        "Expected an error for invalid max_steps value"
    , "Invalid block input" ~: assertIsFatalError
        (doParse gameConstantP 
        (CandyFileParser invalidBlockInput 1 defaultGameConst))
        "Expected to stop parsing at the first invalid character"
    , "Invalid sequence input" ~: assertIsFatalError
        (doParse gameConstantP 
        (CandyFileParser invalidSequenceInput 1 defaultGameConst))
        "Expected to stop parsing at the first invalid character"
  ]


testGameConstWithSomeEffects :: GameConst
testGameConstWithSomeEffects = GameConst {
    dimension = 5,
    maxSteps = 30,
    effectMap = Map.fromList [("effect1", Effect "effect1" (Circle 1)
                              (EffectRequirement Ge 0) "some descrp"),
                              ("effect2", Effect "effect2" (Diamond 4)
                              (EffectRequirement Gt 0) "some de.scrp"),
                              ("effect3", Effect "effect3" 
                                (Arbitrary [(Coordinate 0, All)])
                              (EffectRequirement Eq 0) "some. descrp")],
    candyMap = Map.empty,
    scorePerCandy = 10,
    scorePerEffect = 50
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
  [ "Valid candy input 1" ~: doParse candiesP 
        (CandyFileParser validCandyInput1 1 testGameConstWithSomeEffects)
      ~?= Right ((), CandyFileParser "" 7 (testGameConstWithSomeEffects 
        { candyMap = Map.singleton "CandyShape1" validCandy1 }))
    , "Valid candy input 2" ~: doParse candiesP 
        (CandyFileParser validCandyInput2 1 testGameConstWithSomeEffects)
      ~?= Right ((), CandyFileParser "" 7 (testGameConstWithSomeEffects
       { candyMap = Map.singleton "CandyShape2" validCandy2 }))
    , "Valid candy input 3" ~: doParse candiesP 
        (CandyFileParser validCandyInput3 1 testGameConstWithSomeEffects)
      ~?= Right ((), CandyFileParser "" 7 (testGameConstWithSomeEffects 
        { candyMap = Map.singleton "CandyShape3" validCandy3 }))
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
        (doParse candiesP 
        (CandyFileParser invalidCandyMissingShapeIconTag 1 
            testGameConstWithSomeEffects))
        "Expected error due to missing shape_icon"
    , "Missing shape_name tag" ~: assertIsFatalError
        (doParse candiesP 
        (CandyFileParser invalidCandyMissingShapeNameTag 1 
            testGameConstWithSomeEffects))
        "Expected error due to missing shape_name"
    , "Missing effect_name tag" ~: assertIsFatalError
        (doParse candiesP (CandyFileParser invalidCandyMissingEffectTag 1 
            testGameConstWithSomeEffects))
        "Expected error due to missing effect_name"
    , "Missing shape_icon value" ~: assertIsFatalError
        (doParse candiesP (CandyFileParser invalidCandyMissingShapeIconValue 1 
            testGameConstWithSomeEffects))
        "Expected error due to missing shape_icon value"
    , "Missing shape_name value" ~: assertIsFatalError
        (doParse candiesP (CandyFileParser invalidCandyMissingShapeNameValue 1 
            testGameConstWithSomeEffects))
        "Expected error due to missing shape_name value"
    , "Missing effect_name value" ~: assertIsFatalError
        (doParse candiesP (CandyFileParser invalidCandyMissingEffectValue 1 
            testGameConstWithSomeEffects))
        "Expected error due to missing effect_name value"
    , "Undefined effect name" ~: assertIsFatalError
        (doParse candiesP (CandyFileParser invalidCandyUndefinedEffectName 1 
            testGameConstWithSomeEffects))
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
      doParse candiesP (CandyFileParser (validCandyInput1 ++ "\n\n//\n" 
                                    ++ validCandyInput2 ++ "\n\n//\n"
                                    ++ validCandyInput3) 1 
                                    testGameConstWithSomeEffects)
      ~?= Right ((), CandyFileParser "" 25 testGameConstWithSomeEffects {
          candyMap = Map.fromList [
              ("CandyShape1", validCandy1),
              ("CandyShape2", validCandy2),
              ("CandyShape3", validCandy3)
          ]
      })

  , "Invalid candies input: missing effect_name" ~:
      assertIsFatalError
        (doParse candiesP (CandyFileParser invalidCandiesInputMissingEffectName 
                            1 testGameConstWithSomeEffects))
        "Expected a FatalError for missing effect_name value"

  , "Invalid candies input: undefined effect_name" ~:
      assertIsFatalError
        (doParse candiesP (CandyFileParser invalidCandiesInputUndefinedEffectName 
                            1 testGameConstWithSomeEffects))
        "Expected a FatalError for undefined effect_name reference"
    , "duplicate candy overwrites the previous one" ~:
        doParse candiesP (CandyFileParser (validCandyInput1 ++ "\n\n//\n" 
                                    ++ validCandyInput2 ++ "\n\n//\n"
                                    ++ validCandyInput3 ++ "\n\n//\n"
                                    ++ "shape_name: CandyShape1\n\
                                        \shape_icon: 🍬\n\
                                        \effect_name: effect3\n") 1 
                                        testGameConstWithSomeEffects)
        ~?= Right ((), CandyFileParser "" 31 testGameConstWithSomeEffects {
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
    [ 
        TestLabel "testIntP" testIntP
        , TestLabel "testBetween" testBetween
        , TestLabel "testSepBy" testSepBy
        , TestLabel "testSepBy1" testSepBy1
        , TestLabel "testString" testString
        , TestLabel "testStringP" testStringP
        , TestLabel "testStripP" testStripP
        , TestLabel "testParens" testParens
        , TestLabel "testCommentLine" testCommentLine
        , TestLabel "testSkipCommentOrEmptyLines" testSkipCommentOrEmptyLines
        , TestLabel "testBrackets" testBrackets
        , TestLabel "testConstP" testConstP
        , TestLabel "testCoordinateP" testCoordinateP
        , TestLabel "testCoordinatePairP" testCoordinatePairP
        , TestLabel "testCoordinateListP" testCoordinateListP
        , TestLabel "testCirclePIgnoreCase" testCirclePIgnoreCase
        , TestLabel "testRectanglePIgnoreCase" testRectanglePIgnoreCase
        , TestLabel "testDiamondPIgnoreCase" testDiamondPIgnoreCase
        , TestLabel "testArbitraryPIgnoreCase" testArbitraryPIgnoreCase
        , TestLabel "testEffectRangeP" testEffectRangeP
        , TestLabel "testEffectRangeLineP" testEffectRangeLineP
        , TestLabel "testEffectNameP" testEffectNameP
        , TestLabel "testOperatorP" testOperatorP
        , TestLabel "testEffectRequirementP" testEffectRequirementP
        , TestLabel "testEffectDescriptionP" testEffectDescriptionP
        , TestLabel "testEffectP" testEffectP
        , TestLabel "testGameConstantP" testGameConstantP
        , TestLabel "testActionP" testParseAction
        , TestLabel "testCandies" testCandies
    ]

runAllParserUnitTests :: IO Counts
runAllParserUnitTests = runTestTT allParserUnitTests