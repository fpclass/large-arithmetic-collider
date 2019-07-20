--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 1: Large Arithmetic Collider                                    --
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Data.List (nub)

import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.Basic
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.Tasty.Runners.AntXML

import Game as G
import Level

--------------------------------------------------------------------------------

class HasOperand a where
    getOperand :: a -> Int 

instance HasOperand Action where 
    getOperand (Add n) = n 
    getOperand (Sub n) = n

instance HasOperand Cell where 
    getOperand (Cell _ a) = getOperand a

--------------------------------------------------------------------------------

instance Arbitrary Action where 
    arbitrary = frequency [ (1, Add <$> arbitrarySizedNatural)
                          , (1, Sub <$> arbitrarySizedNatural)
                          ]

instance Arbitrary Cell where 
    arbitrary = Cell <$> arbitrary <*> arbitrary

newtype AddCell = AddCell { unAddCell :: Cell }
newtype SubCell = SubCell { unSubCell :: Cell }
newtype DisCell = DisCell { unDisCell :: Cell }

instance Show AddCell where 
    show = show . unAddCell

instance Show SubCell where 
    show = show . unSubCell

instance Show DisCell where 
    show = show . unDisCell

instance HasOperand AddCell where 
    getOperand = getOperand . unAddCell

instance HasOperand SubCell where 
    getOperand = getOperand . unSubCell

instance Arbitrary AddCell where 
    arbitrary = AddCell . Cell True . Add <$> arbitrarySizedNatural

instance Arbitrary SubCell where 
    arbitrary = SubCell . Cell True . Sub <$> arbitrarySizedNatural
    
instance Arbitrary DisCell where 
    arbitrary = DisCell . Cell False <$> arbitrary

instance Arbitrary Row where 
    arbitrary = do 
        cells <- arbitrary 
        pure $ Row (G.result cells) cells

instance Arbitrary Grid where 
    arbitrary = Grid <$> undefined <*> arbitrary

--------------------------------------------------------------------------------

-- | `loadGrids` @fp@ loads grids from the directory pointed at by @fp@.
loadGrids :: FilePath -> IO [Grid]
loadGrids fp = do 
    r <- loadLevels fp
    case r of 
        Left err -> fail err 
        Right ls -> pure (map levelGrid ls)

-- | `loadSmplGrids` loads simple grids from disk.
loadSmplGrids :: IO [Grid]
loadSmplGrids = loadGrids "levels/1-simple-grids"

-- | `loadAdvGrids` loads advanced grids from disk.
loadAdvGrids :: IO [Grid]
loadAdvGrids = loadGrids "levels/3-adv-grids"

--------------------------------------------------------------------------------

-- | `columnCount` @grid@ determines how many columns there are in @grid@.
columnCount :: Grid -> Int 
columnCount (Grid cts _) = length cts 

-- | `rowCount` @grid@ determines how many rows there are in @grid@. 
rowCount :: Grid -> Int 
rowCount (Grid _ rs) = length rs 

-- | `structureEq` @g1 g2@ checks whether @g1@ and @g2@ are structurally 
-- equivalent. That is, equivalent except for the states of cells.
structureEq :: Grid -> Grid -> Bool 
structureEq (Grid cts rs) (Grid cts' rs') =
    cts==cts' && all (uncurry testRow) (zip rs rs')
    where testRow (Row t cs) (Row t' cs') = 
            t==t' && all (uncurry testCell) (zip cs cs')
          testCell (Cell _ a) (Cell _ a') = a==a'

--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Game" 
    [
        evalTests
    ,   resultTests
    ,   solveRowTests
    ,   solveTests
    ,   rotationsTests
    ,   stepsTests
    ]

evalTests :: TestTree 
evalTests = testGroup "eval" 
    [
        QC.testProperty "Add adds the number to the accumulator" $
            \(Positive n) acc -> G.eval (Add n) acc === n+acc 
    ,   QC.testProperty "Sub subtracts the number from the accumulator" $ 
            \(Positive n) acc -> G.eval (Sub n) acc === acc-n
    ]

resultTests :: TestTree 
resultTests = testGroup "result"
    [
        testCase "returns 0 for the empty list" $ 
            G.result [] @?= 0
    ,   QC.testProperty "returns 0 if all cells are disabled" $
            \(xs :: [DisCell]) -> G.result (map unDisCell xs) === 0
    ,   QC.testProperty "returns the sum if all cells are add cells" $ 
            \(xs :: [AddCell]) -> G.result (map unAddCell xs) === 
                                  sum (map getOperand xs)
    ,   QC.testProperty "returns the sum*(-1) if all cells are sub cells" $ 
            \(xs :: [SubCell]) -> G.result (map unSubCell xs) === 
                                  (-1) * sum (map getOperand xs)
    ,   QC.testProperty "works correctly with a mix of cell types" $
            \(xs :: [DisCell]) (ys :: [AddCell]) (zs :: [SubCell]) ->
                let as = map unDisCell xs
                    bs = map unAddCell ys 
                    cs = map unSubCell zs
                    x  = sum (map getOperand ys)
                    y  = (-1) * sum (map getOperand zs)
                in G.result (as ++ bs ++ cs ++ bs ++ as) === x + y + x
    ]

solveRowTests :: TestTree 
solveRowTests = localOption (QuickCheckMaxSize 25) $ testGroup "solveRow" 
    [
        QC.testProperty "finds at least one solution for rows that have a solution" $ 
            \row -> G.solveRow row =/= []
    ,   QC.testProperty "all results have the same target and number of cells as the input" $
            \row@(Row t cs) -> all (\(Row t' cs') -> t==t' && length cs == length cs') (G.solveRow row)
    ,   QC.testProperty "all results evaluate to to the target (via result)" $
            \row@(Row t _) -> all (\(Row _ cs) -> result cs == t) (G.solveRow row)
    ]

solveTests :: TestTree 
solveTests = withResource loadSmplGrids (const $ pure ()) $ 
    \getGrids -> testGroup "solve"
        [
            testCase "returned grids are structurally the same as the inputs" $ 
                getGrids >>= \grids ->
                all (\(g,sols) -> all (structureEq g) sols) (zip grids (map G.solve grids))
                @?= True
        ,   testCase "returned grids whose rows result in their targets (via result)" $ 
                getGrids >>= \grids -> 
                all (\sols -> all (\(Grid _ rs) -> all (\(Row t cs) -> t==result cs) rs) sols) (map G.solve grids)
                @?= True
        ,   testCase "can solve all of the examples" $ 
                getGrids >>= \grids ->
                all (not . null . G.solve) grids 
                @?= True
        ]

rotationsTests :: TestTree 
rotationsTests = withResource loadAdvGrids (const $ pure ()) $ 
    \getGrids -> testGroup "rotations"
        [
            testCase "returns rows+columns many grids (for grids 2x2 and up)" $
                getGrids >>= \grids -> 
                all (\g -> length (G.rotations g) == columnCount g + rowCount g) grids 
                @?= True
        ]

stepsTests :: TestTree 
stepsTests = withResource loadAdvGrids (const $ pure ()) $ 
    \getGrids -> testGroup "steps"
        [
            testCase "is never empty for grids that can be solved" $ 
                getGrids >>= \grids ->
                all (not . null) (map G.steps grids)
                @?= True
        ,   testCase "does not contain the same grid more than once" $ 
                getGrids >>= \grids -> 
                all (\xs -> length xs == length (nub xs)) (map G.steps grids)
                @?= True  
        ,   testCase "the last grid has at least one solution" $ 
                getGrids >>= \grids ->
                (all (not . null) . map (G.solve . last . G.steps)) grids 
                @?= True
        ]

--------------------------------------------------------------------------------

-- | The list of tasty ingredients. Note: the order seems to matter, 
-- anyXMLRunner won't work at all if placed last in the list.
ingredients :: [Ingredient]
ingredients = [antXMLRunner, listingTests, consoleTestReporter]

-- | The main entry point to the test suite.
main :: IO ()
main = defaultMainWithIngredients ingredients tests

--------------------------------------------------------------------------------