--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 1: Large Arithmetic Collider                                    --
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

--------------------------------------------------------------------------------

module Main ( main ) where

--------------------------------------------------------------------------------

import Control.Monad

import Data.List (nub, transpose)

import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.Basic
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog as H
import Test.Tasty.Runners.AntXML

import Hedgehog hiding (Action, eval)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

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

-- | `natural` randomly generates a positive integer.
natural :: (MonadGen m, Integral a, Bounded a) => m a
natural = Gen.integral $ Range.constant 0 maxBound

-- | `action` randomly generates an `Action`.
action :: MonadGen m => m Action 
action = Gen.frequency 
    [ (1, Add <$> natural)
    , (1, Sub <$> natural)
    ]

-- | `cell` randomly generates a `Cell`.
cell :: MonadGen m => m Cell
cell = Cell <$> Gen.bool <*> action

-- | `addCell` randomly generates an enabled `Cell` whose action is `Add`.
addCell :: MonadGen m => m Cell 
addCell = Cell True . Add <$> natural

-- | `subCell` randomly generates an enabled `Cell` whose action is `Sub`.
subCell :: MonadGen m => m Cell 
subCell = Cell True . Sub <$> natural

-- | `disCell` randomly generates a disabled `Cell`.
disCell :: MonadGen m => m Cell 
disCell = Cell False <$> action

-- | `row` randomly generates a `Row` with up to 100 cells. Note that this uses
-- the `result` function that students have to implement so the rows produced
-- may not be solvable if `result` doesn't work correctly.
row :: MonadGen m => m Row 
row = do
    cells <- Gen.list (Range.constant 0 100) cell
    pure $ Row (result cells) [Cell False op | Cell _ op <- cells]

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

-- | `prop_eval_adds` tests that `eval` works correctly for `Add`.
prop_eval_adds :: Property 
prop_eval_adds = property $ do 
    n <- forAll $ Gen.integral $ Range.constant 0 maxBound 
    acc <- forAll $ Gen.integral $ Range.constantBounded
    eval (Add n) acc === n+acc

-- | `prop_eval_subs` tests that `eval` works correctly for `Sub`.
prop_eval_subs :: Property 
prop_eval_subs = property $ do 
    n <- forAll $ Gen.integral $ Range.constant 0 maxBound 
    acc <- forAll $ Gen.integral $ Range.constantBounded
    eval (Sub n) acc === acc-n

evalTests :: TestTree 
evalTests = testGroup "eval" 
    [
        testProperty "Add adds the number to the accumulator" $
            prop_eval_adds
    ,   testProperty "Sub subtracts the number from the accumulator" $ 
            prop_eval_subs
    ]

--------------------------------------------------------------------------------

-- | `prop_apply_disabled` tests that disabled cells have no effect in `apply`.
prop_apply_disabled :: Property
prop_apply_disabled = property $ do 
    a <- forAll action
    acc <- forAll $ Gen.integral Range.constantBounded
    apply (Cell False a) acc === acc

-- | `prop_apply_add` tests that enabled `Add` cells affect the result 
-- of `apply`.
prop_apply_add :: Property
prop_apply_add = property $ do 
    n <- forAll natural
    acc <- forAll $ Gen.integral Range.constantBounded
    apply (Cell True (Add n)) acc === n+acc

-- | `prop_apply_sub` tests that enabled `Sub` cells affect the result 
-- of `apply`.
prop_apply_sub :: Property 
prop_apply_sub = property $ do 
    n <- forAll natural
    acc <- forAll $ Gen.integral Range.constantBounded
    apply (Cell True (Sub n)) acc === acc-n

applyTests :: TestTree 
applyTests = testGroup "apply"
    [
        testProperty "Disabled cells have no effect on the accumulator" $ 
            prop_apply_disabled
    ,   testProperty "Enabled cells have the expected effect (Add)" $ 
            prop_apply_add
    ,   testProperty "Enabled cells have the expected effect (Sub)" $ 
            prop_apply_sub 
    ]

--------------------------------------------------------------------------------

-- | `prop_result_disabled` tests that `result` is 0 for disabled cells.
prop_result_disabled :: Property
prop_result_disabled = property $ do 
    xs <- forAll $ Gen.list (Range.constant 0 100) disCell
    result xs === 0

-- | `prop_result_add` tests that `result` for enabled `Add` cells is the sum
-- of all cell operands.
prop_result_add :: Property
prop_result_add = property $ do 
    xs <- forAll $ Gen.list (Range.constant 0 100) addCell
    result xs === sum (map getOperand xs)

-- | `prop_result_sub` tests that `result` for enabled `Sub` cells is the
-- negated sum of all cell operands.
prop_result_sub :: Property
prop_result_sub = property $ do 
    xs <- forAll $ Gen.list (Range.constant 0 100) subCell
    result xs === (-1) * sum (map getOperand xs)

-- | `prop_result_mix` tests that `result` produces the correct results for a
-- mix of enabled/disabled cells.
prop_result_mix :: Property
prop_result_mix = property $ do 
    xs <- forAll $ Gen.list (Range.constant 0 100) disCell
    ys <- forAll $ Gen.list (Range.constant 0 100) addCell
    zs <- forAll $ Gen.list (Range.constant 0 100) subCell
    let x = sum (map getOperand ys)
    let y = (-1) * sum (map getOperand zs)
    result (xs++ys++zs++ys++xs) === x + y + x

resultTests :: TestTree 
resultTests = testGroup "result"
    [
        testCase "returns 0 for the empty list" $ 
            result [] @?= 0
    ,   testProperty "returns 0 if all cells are disabled" $
            prop_result_disabled
    ,   testProperty "returns the sum if all cells are add cells" $ 
            prop_result_add
    ,   testProperty "returns the sum*(-1) if all cells are sub cells" $ 
            prop_result_sub
    ,   testProperty "works correctly with a mix of cell types" $
            prop_result_mix
    ]

--------------------------------------------------------------------------------

-- | `prop_solveRow_solvable` tests that `solveRow` produces at least some
-- solution for a `Row` that is solvable (see note about `row`).
prop_solveRow_solvable :: Property 
prop_solveRow_solvable = property $ do 
    row <- forAll row
    solveRow row /== []

-- | `prop_solveRow_sameTarget` test that the solutions returned by `solveRow`
-- all have the same target and same number of cells as the input `Row`.
prop_solveRow_sameTarget :: Property
prop_solveRow_sameTarget = property $ do 
    -- randomly generate a row
    row@(Row t cs) <- forAll row

    -- calculate all solutions using `solveRow` and display them
    let solutions = solveRow row 
    annotateShow solutions 

    -- check that there is at least one solution
    length solutions /== 0

    -- for every solution returned by `solveRow`, check that it has the same
    -- target and the same number of cells as the input
    forM_ solutions $ \sol@(Row t' cs') -> do 
        t === t' 
        length cs === length cs' 

prop_solveRow_evaluate :: Property 
prop_solveRow_evaluate = property $ do 
    row@(Row t _) <- forAll row
    let solutions = solveRow row 
    length solutions /== 0
    forM_ solutions $ \(Row _ cs) -> 
        result cs === t

solveRowTests :: TestTree 
solveRowTests = localOption (HedgehogTestLimit $ Just 25) $ testGroup "solveRow" 
    [
        testCase "the solution for Row 0 [] is Row 0 []" $ 
            solveRow (Row 0 []) @?= [Row 0 []]
    ,   testProperty "finds at least one result for rows that have a solution" $ 
            prop_solveRow_solvable
    ,   testProperty "all results have the same target and number of cells as the input" $
            prop_solveRow_sameTarget
    ,   testProperty "all results evaluate to to the target (via result)" $
            prop_solveRow_evaluate
    ]

--------------------------------------------------------------------------------

solveTests :: TestTree 
solveTests = withResource (loadSmplGrids >>= \grids -> pure $ zip grids (map solve grids)) (const $ pure ()) $ 
    \getResults -> testGroup "solve" 
        [
            testCase "returned grids are structurally the same as the inputs (same dimensions and cells)" $
                getResults >>= \results -> mapM_ (\(g,sols) -> mapM_ (\s -> 
                    structureEq g s @?= True) sols
                ) results 
        ,   testCase "returned grids whose rows result in their targets (via result)" $ 
                getResults >>= \results -> mapM_ (\sols -> mapM_ (\(Grid _ rs) -> 
                    mapM_ (\(Row t cs) -> t @?= result cs) rs) sols
                ) (map snd results)
        ,   testCase "returned grids whose columns result in their targets (via result)" $
                getResults >>= \results -> mapM_ (\sols -> mapM_ (\(Grid cs rs) -> 
                    mapM_ (\(t,col) -> t @?= result col) $ 
                        zip cs $ transpose (map (\(Row _ cells) -> cells) rs)) sols
                ) (map snd results)
        ,   testCase "finds results for all of the examples" $
                getResults >>= \results -> mapM_ (\sols -> 
                    assertBool "solve found no results" 
                        (not $ null sols)
                 ) (map snd results) 
        ]

--------------------------------------------------------------------------------

rotationsTests :: TestTree 
rotationsTests = withResource loadAdvGrids (const $ pure ()) $ 
    \getGrids -> testGroup "rotations"
        [
            testCase "returns rows+columns many grids (for grids 2x2 and up)" $ 
                getGrids >>= \grids -> mapM_ (\(g,rs) -> 
                    length rs @?= columnCount g + rowCount g
                ) (zip grids (map rotations grids))
        ]

--------------------------------------------------------------------------------

stepsTests :: TestTree 
stepsTests = 
    withResource 
        (loadAdvGrids >>= \grids -> pure $ map steps grids) 
        (const $ pure ()) $ 
    \getResults -> testGroup "steps" 
        [
            testCase "returns results for all test grids" $ 
                getResults >>= \results -> mapM_ (\r -> 
                    assertBool "grid has no results" 
                            (not $ null r)
                ) results
        ,   testCase "results are all unique" $ 
                getResults >>= \results -> mapM_ (\r -> 
                    length r @?= length (nub r)
                ) results
        ,   testCase "the last step in the sequence is a solution (via solve)" $ 
                getResults >>= \results -> mapM_ (\r -> 
                    assertBool "solve returns an empty list" 
                        (not $ null $ solve (last r))
                ) results
        ] 

--------------------------------------------------------------------------------

tests :: TestTree
tests = localOption (HedgehogShowReplay False) $ testGroup "Game" 
    [
        evalTests
    ,   applyTests
    ,   resultTests
    ,   solveRowTests
    ,   solveTests
    ,   rotationsTests
    ,   stepsTests
    ]

--------------------------------------------------------------------------------

-- | The list of tasty ingredients. Note: the order seems to matter, 
-- antXMLRunner won't work at all if placed last in the list.
ingredients :: [Ingredient]
ingredients = [antXMLRunner, listingTests, consoleTestReporter]

-- | The main entry point to the test suite.
main :: IO ()
main = defaultMainWithIngredients ingredients tests

--------------------------------------------------------------------------------