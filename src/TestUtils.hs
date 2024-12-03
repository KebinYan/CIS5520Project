-- -- This fils is used to create test data and arbitrary quickcheck instances for testing
-- module TestUtils where

-- import Test.QuickCheck

-- import Candy
-- import GameUtils
-- import GameState
-- import GameController
-- import qualified Data.Map as Map
-- import Control.Monad

-- -- Sample candies for testing
-- candy1 :: Candy
-- candy1 = Candy {
--     candyDef = CandyDefinition {
--         shapeName = "Triangle",
--         shapeIcon = "▲",
--         effectNameRef = "Normal"
--     },
--     candyEffect = normalEffect
-- }
-- candy2 :: Candy
-- candy2 = Candy {
--     candyDef = CandyDefinition {
--         shapeName = "Square",
--         shapeIcon = "■",
--         effectNameRef = "Normal"
--     },
--     candyEffect = normalEffect
-- }
-- candy3 :: Candy
-- candy3 = Candy {
--     candyDef = CandyDefinition {
--         shapeName = "Spade",
--         shapeIcon = "♠",
--         effectNameRef = "Normal"
--     },
--     candyEffect = normalEffect
-- }
-- candy4 :: Candy
-- candy4 = Candy {
--     candyDef = CandyDefinition {
--         shapeName = "Heart",
--         shapeIcon = "♥",
--         effectNameRef = "Normal"
--     },
--     candyEffect = normalEffect
-- }
-- candy5 :: Candy
-- candy5 = Candy {
--     candyDef = CandyDefinition {
--         shapeName = "Asterisk",
--         shapeIcon = "*",
--         effectNameRef = "Bomb"
--     },
--     candyEffect = Effect {
--         effectName = "Bomb",
--         effectRange = Rectangle 3 3,
--         effectRequirement = Requirement Eq 5,
--         effectDescription = "placeholder"
--     }
-- }
-- candy6 :: Candy
-- candy6 = Candy {
--     candyDef = CandyDefinition {
--         shapeName = "Cross",
--         shapeIcon = "+",
--         effectNameRef = "StripedCross"
--     },
--     candyEffect = Effect {
--         effectName = "StripedCross",
--         effectRange = Arbitrary [(Coordinate 0, All), (All, Coordinate 0)],
--         effectRequirement = Requirement Ge 5,
--         effectDescription = "placeholder"
--     }
-- }

-- candy7 :: Candy
-- candy7 = Candy {
--     candyDef = CandyDefinition {
--         shapeName = "Minus",
--         shapeIcon = "-",
--         effectNameRef = "StripedRow"
--     },
--     candyEffect = Effect {
--         effectName = "StripedRow",
--         effectRange = Arbitrary [(Coordinate 0, All)],
--         effectRequirement = Requirement Eq 4,
--         effectDescription = "placeholder"
--     }
-- }

-- -- Test grids
-- initialGrid :: GameGrid
-- initialGrid = GameGrid
--     {   board =
--         [ [candy2, candy2, candy3]
--         , [candy4, candy1, candy2]
--         , [candy3, candy5, candy4]
--         ],
--         normalCandies = [candy1, candy2, candy3, candy4],
--         specialCandies = Map.fromList [
--             (4, [candy6]),
--             (5, [candy5])
--         ]
--     }

-- crushableGrid :: GameGrid
-- crushableGrid = GameGrid
--     {   board =
--         [ [candy1, candy1, candy1, candy3]
--         , [candy2, candy2, candy2, candy2]
--         , [candy2, candy6, candy5, candy3]
--         , [candy3, candy2, candy1, candy1]
--         ],
--         normalCandies = [candy1, candy2, candy3],
--         specialCandies = Map.fromList [
--             (4, [candy7]),
--             (5, [candy5]),
--             (5, [candy6]),
--             (6, [candy6]),
--             (7, [candy6])
--         ]
--     }

-- gridWithEmptyCandy :: GameGrid
-- gridWithEmptyCandy = GameGrid
--     {
--         board =
--         [ [candy1, candy1, candy3]
--         , [candy2, EmptyCandy, EmptyCandy]
--         , [candy2, EmptyCandy, EmptyCandy]
--         ],
--         normalCandies = [candy1, candy2, candy3],
--         specialCandies = Map.fromList [
--             (4, [candy6]),
--             (5, [candy5])
--         ]
--     }


-- -- Arbitrary instances for quickcheck
-- instance Arbitrary Difficulty where
--     arbitrary :: Gen Difficulty
--     arbitrary = elements [easy, medium, hard]

-- instance Arbitrary Operator where
--     arbitrary :: Gen Operator
--     arbitrary = elements [Eq, Gt, Ge]

-- instance Arbitrary Requirement where
--     arbitrary :: Gen Requirement
--     arbitrary = Requirement <$> arbitrary <*> choose (3, 7)

-- instance Arbitrary CandyDefinition where
--     arbitrary :: Gen CandyDefinition
--     arbitrary = CandyDefinition <$> arbitrary <*> arbitrary <*> arbitrary

-- genNormalDef :: Gen CandyDefinition
-- genNormalDef = CandyDefinition <$> arbitrary <*> arbitrary <*> pure "Normal"

-- genArbCoord :: Difficulty -> Gen Coordinate
-- genArbCoord d =
--     frequency [
--         (1, return All),
--         (1, Coordinate <$> choose (0, dimension d - 1))
--     ]

-- genArbIntCoord :: Difficulty -> Gen Coordinate
-- genArbIntCoord d = Coordinate <$> choose (0, dimension d - 1)

-- genArbCoordPair :: Difficulty -> Gen CoordinatePair
-- genArbCoordPair d = (,) <$> genArbCoord d <*> genArbCoord d

-- genArbIntCoordPair :: Difficulty -> Gen CoordinatePair
-- genArbIntCoordPair d = (,) <$> genArbIntCoord d <*> genArbIntCoord d

-- genArbNormalCandy :: Difficulty -> Gen Candy
-- genArbNormalCandy d = Candy <$> genNormalDef <*> pure normalEffect

-- genArbSpecialCandy :: Difficulty -> Gen Candy
-- genArbSpecialCandy d = do
--     candyDef <- arbitrary `suchThat` isSpecialCandyDef
--     effectRange <- genArbEffectRange d
--     effectRequirement <- arbitrary
--     effectDescription <- arbitrary
--     return $ Candy {
--         candyDef = candyDef,
--         candyEffect = Effect {
--             effectName = shapeName candyDef,
--             effectRange = effectRange,
--             effectRequirement = effectRequirement,
--             effectDescription = effectDescription
--         }
--     }

-- genArbEffectRange :: Difficulty -> Gen EffectRange
-- genArbEffectRange d =
--     frequency [
--         (1, Circle <$> choose (0, 3)),
--         (1, Rectangle <$> choose (0, 3) <*> choose (0, 3)),
--         (1, Diamond <$> choose (0, 3)),
--         (1, Arbitrary <$> listOf (genArbCoordPair d))
--     ]

-- genGameBoard :: Difficulty -> Gen [Candy] -> Gen [[Candy]]
-- genGameBoard d candies = do
--     let dim = dimension d
--     candiesList <- candies
--     vectorOf dim (vectorOf dim (elements candiesList))

-- genGameBoardWithEmpty :: Gen[Candy] -> Gen [[Candy]]
-- genGameBoardWithEmpty candies = do
--     n <- choose (1, 10)
--     m <- choose (1, 10)
--     candiesList <- candies
--     vectorOf n (vectorOf m (oneof [elements candiesList, pure EmptyCandy]))

-- genGameGrid :: Difficulty -> Gen GameGrid
-- genGameGrid d =
--     let normalCandies =
--             liftM2 (++) (replicateM (dimension d) (genArbNormalCandy d)) 
--             (listOf (genArbNormalCandy d))
--     in GameGrid <$>
--         genGameBoard d normalCandies <*>
--         normalCandies <*>
--         genSpecialCandyMap d

-- genGameState :: Difficulty -> Gen GameState
-- genGameState d = GameState <$>
--     genGameGrid d <*>
--     return d <*>
--     oneof [pure Nothing, Just <$> genGameGrid d] <*>
--     choose (0, 100) <*>
--     choose (0, 100)

-- genSpecialCandyMap :: Difficulty -> Gen (Map.Map Int [Candy])
-- genSpecialCandyMap d = do
--     mapSize <- choose (1, 5)
--     num <- vectorOf mapSize (choose (1, 5))
--     candiesList <- vectorOf mapSize (listOf1 (genArbSpecialCandy d))
--     return $ Map.fromList $ zip num candiesList

-- genEmptyCandyCoords :: Difficulty -> Gen [CoordinatePair]
-- genEmptyCandyCoords d = listOf (genArbCoordPair d)

-- genArbAction :: Difficulty -> Gen Action
-- genArbAction d =
--     oneof [
--         Swap <$> genArbIntCoordPair d <*> genArbIntCoordPair d,
--         Click <$> genArbIntCoordPair d,
--         pure Undo,
--         pure Quit,
--         Disappear <$> listOf1 (genArbIntCoordPair d),
--         Trigger <$> ((,) <$> genArbIntCoordPair d <*> genArbSpecialCandy d)
--     ]

-- genArbUserAction :: Difficulty -> Gen Action
-- genArbUserAction d =
--     oneof [
--         Swap <$> genArbIntCoordPair d <*> genArbIntCoordPair d,
--         Click <$> genArbIntCoordPair d,
--         pure Undo,
--         pure Quit
--     ]

-- genArbReversibleAction :: Difficulty -> Gen Action
-- genArbReversibleAction d =
--     oneof [
--         Swap <$> genArbIntCoordPair d <*> genArbIntCoordPair d,
--         Click <$> genArbIntCoordPair d
--     ]

-- -- Helper functions
-- isSpecialCandyDef :: CandyDefinition -> Bool
-- isSpecialCandyDef candyDef = effectNameRef candyDef /= "Normal"




