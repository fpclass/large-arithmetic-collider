--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 1: Large Arithmetic Collider                                    --
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.List (nub)

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

-- | `loadGrids` @fp continuation@ loads grids from the directory pointed at
-- by @fp@ and then passes the results to the @continuation@.
loadGrids :: FilePath -> ([Grid] -> IO ()) -> IO ()
loadGrids fp cont = do 
    r <- loadLevels fp
    case r of 
        Left err -> fail err 
        Right ls -> cont (map levelGrid ls)

-- | `loadSmplGrids` @continuation@ loads simple grids from disk and then 
-- passes the results to the @continuation@.
loadSmplGrids :: ([Grid] -> IO ()) -> IO ()
loadSmplGrids = loadGrids "levels/1-simple-grids"

-- | `loadAdvGrids` @continuation@ loads advanced grids from disk and then 
-- passes the results to the @continuation@.
loadAdvGrids :: ([Grid] -> IO ()) -> IO ()
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

-- | `main` is the main entry point to the test suite.
main :: IO ()
main = hspec $ do 
    describe "eval" $ do 
        prop "Add adds the number to the accumulator" $
            \(Positive n) acc -> G.eval (Add n) acc === n+acc 
        prop "Sub subtracts the number from the accumulator" $ 
            \(Positive n) acc -> G.eval (Sub n) acc === acc-n
    describe "result" $ do 
        it "returns 0 for the empty list" $ 
            G.result [] `shouldBe` 0
        prop "returns 0 if all cells are disabled" $
            \(xs :: [DisCell]) -> G.result (map unDisCell xs) === 0
        prop "returns the sum if all cells are add cells" $ 
            \(xs :: [AddCell]) -> G.result (map unAddCell xs) === 
                                  sum (map getOperand xs)
        prop "returns the sum*(-1) if all cells are sub cells" $ 
            \(xs :: [SubCell]) -> G.result (map unSubCell xs) === 
                                  (-1) * sum (map getOperand xs)
        prop "works correctly with a mix of cell types" $
            \(xs :: [DisCell]) (ys :: [AddCell]) (zs :: [SubCell]) ->
                let as = map unDisCell xs
                    bs = map unAddCell ys 
                    cs = map unSubCell zs
                    x  = sum (map getOperand ys)
                    y  = (-1) * sum (map getOperand zs)
                in G.result (as ++ bs ++ cs ++ bs ++ as) === x + y + x
    describe "solveRow" $ 
        -- modify the max size so we don't generate rows that are too large
        modifyMaxSize (const 25) $ do 
            prop "finds at least one solution for rows that have a solution" $ 
                \row -> G.solveRow row =/= []
            prop "all results have the same target and number of cells as the input" $
                \row@(Row t cs) -> all (\(Row t' cs') -> t==t' && length cs == length cs') (G.solveRow row)
            prop "all results evaluate to to the target (via result)" $
                \row@(Row t _) -> all (\(Row _ cs) -> result cs == t) (G.solveRow row)
    around loadSmplGrids $ 
        describe "solve" $ do 
            it "returned grids are structurally the same as the inputs" $ \grids -> 
                zip grids (map G.solve grids) `shouldSatisfy` 
                all (\(g,sols) -> all (structureEq g) sols)
            it "returned grids whose rows result in their targets (via result)" $ \grids -> 
                map G.solve grids `shouldSatisfy` 
                all (\sols -> all (\(Grid _ rs) -> all (\(Row t cs) -> t==result cs) rs) sols)
            it "can solve all of the examples" $ \grids ->
                grids `shouldSatisfy` all (not . null . G.solve)
    describe "rotations" $ do 
        around loadAdvGrids $ it "returns rows+columns many grids (for grids 2x2 and up)" $ \grids ->
            grids `shouldSatisfy` all (\g -> length (G.rotations g) == columnCount g + rowCount g) 
    around loadAdvGrids $ do
        describe "steps" $ do
            it "is never empty for grids that can be solved" $ \grids ->
                map G.steps grids `shouldSatisfy` all (not . null) 
            it "does not contain the same grid more than once" $ \grids -> 
                map G.steps grids `shouldSatisfy` all (\xs -> length xs == length (nub xs))  
            it "the last grid has at least one solution" $ \grids ->
                grids `shouldSatisfy` all (not . null) . map (G.solve . last . G.steps)

--------------------------------------------------------------------------------
