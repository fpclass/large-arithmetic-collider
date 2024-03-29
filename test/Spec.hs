--------------------------------------------------------------------------------
-- Functional Programming - Large Arithmetic Collider Project
--------------------------------------------------------------------------------
-- Copyright (c) 2022 Michael B. Gale (michael@fpclass.online)
--
-- This source code is subject to the terms and conditions found in the LICENSE
-- file in the root directory of this source tree.
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

--------------------------------------------------------------------------------

module Main ( main ) where

--------------------------------------------------------------------------------

import Control.Monad

import Data.List

import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.Basic
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog as H
import Test.Tasty.Runners.AntXML

import Hedgehog hiding (Action, eval)
import Hedgehog.Internal.Show (showPretty)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Game as G
import Level

--------------------------------------------------------------------------------

-- | A convenience class for types which have operands: actions or cells.
class HasOperand a where
    -- | Retrieves the operand of the argument.
    getOperand :: a -> Int

instance HasOperand Action where
    getOperand (Add n) = n
    getOperand (Sub n) = n

instance HasOperand Cell where
    getOperand (MkCell _ a) = getOperand a

-- | `getAction` @cell@ extracts the `Action` from a `Cell`.
getAction :: Cell -> Action
getAction (MkCell _ a) = a

-- | `getCells` @row@ extracts the cells from @row@.
getCells :: Row -> [Cell]
getCells (MkRow _ cs) = cs

-- | `gridSize` @grid@ calculates how many cells there are in a grid (for
-- classification purposes).
gridSize :: Grid -> Int
gridSize (MkGrid ts rs) = length ts * length rs

-- | A convenience class for types which have cells that can be disabled.
class CanDisable a where
    -- | `disable` @item@ marks all cells in @item@ as disabled.
    disable :: a -> a

instance CanDisable Cell where
    disable (MkCell _ a) = MkCell False a

instance CanDisable Row where
    disable (MkRow t cs) = MkRow t (map disable cs)

instance CanDisable Grid where
    disable (MkGrid cs rs) = MkGrid cs (map disable rs)

--------------------------------------------------------------------------------

-- | `natural` randomly generates a positive integer up to 255.
natural :: (MonadGen m, Integral a, Bounded a) => m a
natural = Gen.integral $ Range.constant 0 255

-- | `action` randomly generates an `Action`.
action :: MonadGen m => m Action
action = Gen.frequency
    [ (1, Add <$> natural)
    , (1, Sub <$> natural)
    ]

-- | `cell` randomly generates a `Cell`.
cell :: MonadGen m => m Cell
cell = MkCell <$> Gen.bool <*> action

-- | `addCell` randomly generates an enabled `Cell` whose action is `Add`.
addCell :: MonadGen m => m Cell
addCell = MkCell True . Add <$> natural

-- | `subCell` randomly generates an enabled `Cell` whose action is `Sub`.
subCell :: MonadGen m => m Cell
subCell = MkCell True . Sub <$> natural

-- | `disCell` randomly generates a disabled `Cell`.
disCell :: MonadGen m => m Cell
disCell = MkCell False <$> action

-- | `row` randomly generates a `Row` with up to 10 cells. Note that this uses
-- the `result` function that students have to implement so the rows produced
-- may not be solvable if `result` doesn't work correctly.
row :: MonadGen m => m Row
row = do
    cells <- Gen.list (Range.constant 0 10) cell
    pure $ MkRow (result cells) (map disable cells)

-- | `grid` @size@ randomly generates a grid of up to @size@x@size@ cells.
-- Note that this uses the `result` function that students have to implement
-- so the grids produced may not be solvable if `result` doesn't work correctly.
grid :: MonadGen m => Range.Range Int -> m Grid
grid range = do
    rowSize <- Gen.integral range
    cells <- Gen.list range $ Gen.list (Range.singleton rowSize) cell

    let columns = map result $ transpose cells
    let rows = map (\cs -> MkRow (result cs) (map disable cs)) cells

    pure $ MkGrid columns rows

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
columnCount (MkGrid cts _) = length cts

-- | `rowCount` @grid@ determines how many rows there are in @grid@.
rowCount :: Grid -> Int
rowCount (MkGrid _ rs) = length rs

-- | `structureEq` @g1 g2@ checks whether @g1@ and @g2@ are structurally
-- equivalent. That is, equivalent except for the states of cells.
structureEq :: Grid -> Grid -> Bool
structureEq (MkGrid cts rs) (MkGrid cts' rs') =
    cts==cts' && length rs==length rs' && all (uncurry testRow) (zip rs rs')
    where testRow (MkRow t cs) (MkRow t' cs') =
            t==t' && length cs == length cs' && all (uncurry testCell) (zip cs cs')
          testCell (MkCell _ a) (MkCell _ a') = a==a'

-- | `allActions` @rows@ returns a list of all `Action`s in @rows@.
allActions :: [Row] -> [Action]
allActions = map getAction . concat . map getCells

-- | `rotationEq` @a b@ checks that two grids @a@ and @b@ are equivalent for
-- rotation purposes. That is, they have the same dimensions and the same
-- row and column targets, but no checks are performed on cells.
rotationEq :: Grid -> Grid -> Bool
rotationEq (MkGrid ts rs) (MkGrid ts' rs') =
    ts == ts' &&
    length rs == length rs' &&
    and (zipWith checkRow rs rs') &&
    sort (allActions rs) == sort (allActions rs')
    where checkRow (MkRow t cs) (MkRow t' cs') =
            t==t' && length cs == length cs'

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
        testProperty
            "adds the operand to the accumulator for Add"
            "prop_eval_adds"
            prop_eval_adds
    ,   testProperty
            "subtracts the operand from the accumulator for Sub"
            "prop_eval_subs"
            prop_eval_subs
    ]

--------------------------------------------------------------------------------

-- | `prop_apply_disabled` tests that disabled cells have no effect in `apply`.
prop_apply_disabled :: Property
prop_apply_disabled = property $ do
    a <- forAll action
    acc <- forAll $ Gen.integral Range.constantBounded
    apply (MkCell False a) acc === acc

-- | `prop_apply_add` tests that enabled `Add` cells affect the result
-- of `apply`.
prop_apply_add :: Property
prop_apply_add = property $ do
    n <- forAll natural
    acc <- forAll $ Gen.integral Range.constantBounded
    apply (MkCell True (Add n)) acc === n+acc

-- | `prop_apply_sub` tests that enabled `Sub` cells affect the result
-- of `apply`.
prop_apply_sub :: Property
prop_apply_sub = property $ do
    n <- forAll natural
    acc <- forAll $ Gen.integral Range.constantBounded
    apply (MkCell True (Sub n)) acc === acc-n

applyTests :: TestTree
applyTests = testGroup "apply"
    [
        testProperty
            "disabled cells have no effect on the accumulator"
            "prop_apply_disabled"
            prop_apply_disabled
    ,   testProperty
            "enabled cells have the expected effect (Add)"
            "prop_apply_add"
            prop_apply_add
    ,   testProperty
            "enabled cells have the expected effect (Sub)"
            "prop_apply_sub"
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
    ,   testProperty
            "returns 0 if all cells are disabled"
            "prop_result_disabled"
            prop_result_disabled
    ,   testProperty
            "returns the sum if all cells are add cells"
            "prop_result_add"
            prop_result_add
    ,   testProperty
            "returns the sum*(-1) if all cells are sub cells"
            "prop_result_sub"
            prop_result_sub
    ,   testProperty
            "works correctly with a mix of cell types"
            "prop_result_mix"
            prop_result_mix
    ]

--------------------------------------------------------------------------------

-- | `prop_states_length` tests that `states` returns a list with exactly
-- two elements.
prop_states_length :: Property
prop_states_length = property $ do
    -- generate a random cell
    c <- forAll cell

    -- check that the length of the resulting list is exactly 2
    length (states c) === 2

-- | `prop_states_sameAction` tests that `states` returns a list where all
-- of the results have the same action as the input cell.
prop_states_sameAction :: Property
prop_states_sameAction = property $ do
    -- generate a random cell
    c@(MkCell _ a) <- forAll cell

    -- use states to get a list of cell states
    let results = states c

    -- check that there is at least one result
    length results /== 0

    -- for every item in the result, check that its action is the same
    forM_ results $ \(MkCell _ a') -> a' === a

-- | `prop_states_noDuplicates` checks that `states` returns a list with no
-- duplicates. Combined with the other properties, this enforces that one
-- resulting cell is enabled and the other disabled.
prop_states_noDuplicates :: Property
prop_states_noDuplicates = property $ do
    -- generate a random cell
    c <- forAll cell

    -- use states to get a list of cell states
    let results = states c

    -- check that there is at least one result
    length results /== 0

    -- check that the result contains no duplicates by comparing it to a
    -- list with all duplicates removed
    results === nub results

statesTests :: TestTree
statesTests = testGroup "states"
    [
        testProperty
            "returns a list with exactly two elements"
            "prop_states_size"
            prop_states_length
    ,   testProperty
            "returns a list where all cells have the same action"
            "prop_states_sameAction"
            prop_states_sameAction
    ,   testProperty
            "returns a list which contains no duplicates"
            "prop_states_noDuplicates"
            prop_states_noDuplicates
    ]

--------------------------------------------------------------------------------

prop_candidates_length :: Property
prop_candidates_length = property $ do
    -- generate a list of random cells
    cells <- forAll $ Gen.list (Range.constant 0 10) cell

    -- print the result of candidates in case of test failure
    annotateShow (candidates cells)

    -- check that there are as many results as we would expect
    length (candidates cells) === 2^(length cells)

-- | `prop_candidates_actions` checks that all candidates returned by
-- `candidates` have the same actions as the input.
prop_candidates_actions :: Property
prop_candidates_actions = property $ do
    -- generate a list of random cells
    cells <- forAll $ Gen.list (Range.constant 0 10) cell

    -- check that there is at least one result
    length (candidates cells) /== 0

    -- check that all resulting lists have the same actions
    forM_ (candidates cells) $ \candidate ->
        map getAction candidate === map getAction cells

-- | `prop_candidates_noDuplicates` checks that `candidates` returns no
-- duplicate candidates.
prop_candidates_noDuplicates :: Property
prop_candidates_noDuplicates = property $ do
    -- generate a list of random cells
    cells <- forAll $ Gen.list (Range.constant 0 10) cell

    -- check that there is at least one result
    length (candidates cells) /== 0

    -- check that there are no duplicates
    candidates cells === nub (candidates cells)

candidatesTests :: TestTree
candidatesTests = testGroup "candidates"
    [
        testCase "candidates [] has a candidate solution" $
            candidates [] @?= [[]]
    ,   testProperty
            "returns 2^n results for n inputs"
            "prop_candidates_length"
            prop_candidates_length
    ,   testProperty
            "candidates have the same actions as the input"
            "prop_candidates_actions"
            prop_candidates_actions
    ,   testProperty
            "returns no duplicates"
            "prop_candidates_noDuplicates"
            prop_candidates_noDuplicates
    ]

--------------------------------------------------------------------------------

-- | `prop_solveRow_solvable` tests that `solveRow` produces at least some
-- solution for a `Row` that is solvable (see note about `row`).
prop_solveRow_solvable :: Property
prop_solveRow_solvable = property $ do
    -- randomly generate a row
    r@(MkRow _ cs) <- forAll row

    -- classify the row
    classify "small rows" $ length cs < 5
    classify "large rows" $ length cs >= 5

    -- check that the row has solutions
    solveRow r /== []

-- | `prop_solveRow_sameTarget` tests that the solutions returned by `solveRow`
-- all have the same target and same number of cells as the input `Row`.
prop_solveRow_sameTarget :: Property
prop_solveRow_sameTarget = property $ do
    -- randomly generate a row
    r@(MkRow t cs) <- forAll row

    -- classify the row
    classify "small rows" $ length cs < 5
    classify "large rows" $ length cs >= 5

    -- calculate all solutions using `solveRow` and display them
    let solutions = solveRow r
    annotateShow solutions

    -- check that there is at least one solution
    length solutions /== 0

    -- for every solution returned by `solveRow`, check that it has the same
    -- target and the same number of cells as the input
    forM_ solutions $ \(MkRow t' cs') -> do
        t === t'
        length cs === length cs'

-- | `prop_solveRow_evaluate` tests that the solutions returned by `solveRow`
-- all evaluate to the row target.
prop_solveRow_evaluate :: Property
prop_solveRow_evaluate = property $ do
    -- randomly generate a row
    r@(MkRow t xs) <- forAll row

    -- classify the row
    classify "small rows" $ length xs < 5
    classify "large rows" $ length xs >= 5

    -- calculate the solutions for the row
    let solutions = solveRow r

    -- check that there is at least one solution
    length solutions /== 0

    -- for every solution, check that it evaluates to the target
    forM_ solutions $ \(MkRow _ cs) ->
        result cs === t

solveRowTests :: TestTree
solveRowTests = testGroup "solveRow"
    [
        testCase "the solution for MkRow 0 [] is MkRow 0 []" $
            solveRow (MkRow 0 []) @?= [MkRow 0 []]
    ,   testProperty
            "finds at least one result for rows that have a solution"
            "prop_solveRow_solvable"
            prop_solveRow_solvable
    ,   testProperty
            "results have the same row target and number of cells"
            "prop_solveRow_sameTarget"
            prop_solveRow_sameTarget
    ,   testProperty "all results evaluate to to the target (via result)"
            "prop_solveRow_evaluate"
            prop_solveRow_evaluate
    ]

--------------------------------------------------------------------------------

-- | `prop_solve_structure` checks that `solve` returns solutions which are
-- structurally equivalent (same dimensions, targets, and actions) as the input.
prop_solve_structure :: Property
prop_solve_structure = property $ do
    -- generate a random grid
    input <- forAll $ grid (Range.constant 0 10)

    -- classify the grid
    classify "small grids" $ gridSize input < 50
    classify "large grids" $ gridSize input >= 50

    -- generate the solutions
    let solutions = solve input

    -- there should be at least one solution
    length solutions /== 0

    -- check that all solutions are structurally equivalent to the input
    forM_ (solve input) $ \solution ->
        diff solution structureEq input

-- | `prop_solve_rows` checks that the rows in solutions returned by `solve`
-- evaluate to their targets (using `result`).
prop_solve_rows :: Property
prop_solve_rows = property $ do
    -- generate a random grid
    input <- forAll $ grid (Range.constant 0 10)

    -- classify the grid
    classify "small grids" $ gridSize input < 50
    classify "large grids" $ gridSize input >= 50

    -- generate the solutions
    let solutions = solve input

    -- there should be at least one solution
    length solutions /== 0

    -- check every solution
    forM_ solutions $ \solution@(MkGrid _ rows) -> do
        -- check for structural equivalence with the input
        diff solution structureEq input

        -- check that the result of the rows are the corresponding row targets
        forM_ rows $ \(MkRow target cells) ->
            result cells === target

-- | `prop_solve_columns` checks that the columns in solutions returned by
-- `solve` evaluate to their targets (using `result`).
prop_solve_columns :: Property
prop_solve_columns = property $ do
    -- generate a random grid
    input <- forAll $ grid (Range.constant 0 10)

    -- classify the grid
    classify "small grids" $ gridSize input < 50
    classify "large grids" $ gridSize input >= 50

    -- generate the solutions
    let solutions = solve input

    -- there should be at least one solution
    length solutions /== 0

    -- check every solution
    forM_ solutions $ \solution@(MkGrid cts rows) -> do
        -- check for structural equivalence with the input
        diff solution structureEq input

        -- check that the result of the columns are the corresponding
        -- column targets
        let columns = zip cts (transpose $ map getCells rows)
        forM_ columns $ \(target, column) ->
            result column === target

solveTests :: TestTree
solveTests = withResource
    (loadSmplGrids >>= \grids -> pure $ zip [1..] (map solve grids))
    (const $ pure ()) $
    \getResults -> testGroup "solve"
        [
            testProperty
                "results have the same structure as the inputs"
                "prop_solve_structure"
                prop_solve_structure
        ,   testProperty
                "rows of solutions result in their targets (via result)"
                "prop_solve_rows"
                prop_solve_rows
        ,   testProperty
                "columns of solutions result in their targets (via result)"
                "prop_solve_columns"
                prop_solve_columns
        ,   testCaseSteps "solves grids from the simple campaign" $ \step -> do
                results <- getResults
                forM_ results $ \(lvl, sols) ->
                    when (null sols) $ do
                        step ("simple grid #" <> show (lvl :: Int))
                        assertFailure "solve found no solutions for this grid"
        ]

--------------------------------------------------------------------------------

-- | `oppositeDir` @direction@ returns the opposite of @direction@.
oppositeDir :: Direction -> Direction
oppositeDir L = R
oppositeDir R = L

-- | `prop_rotate_singleton` checks that rotating a singleton list has no
-- effect and just returns the singleton list.
prop_rotate_singleton :: Property
prop_rotate_singleton = property $ do
    -- generate an element
    (x :: Int) <- forAll natural

    -- pick a direction
    dir <- forAll $ Gen.element [L, R]

    -- rotating a singleton list has no effect
    rotate dir [x] === [x]

-- | `prop_rotate` checks that for a given list, rotating it in one direction
-- and then in the other afterwards yields the same list we started with.
prop_rotate :: Property
prop_rotate = property $ do
    -- generate a list
    (xs :: [Int]) <- forAll $ Gen.list (Range.constant 0 100) natural

    -- pick an initial direction
    dir <- forAll $ Gen.element [L, R]

    -- rotating the list in one direction and then rotating it back into the
    -- opposite direction should take us to where we started
    rotate (oppositeDir dir) (rotate dir xs) === xs

-- | `prop_rotate_left` checks that `rotate` @L@ has the desired effects.
prop_rotate_left :: Property
prop_rotate_left = property $ do
    -- generate a list
    (xs :: [Int]) <- forAll $ Gen.list (Range.constant 2 100) natural

    -- check that left rotations have the desired effects
    last (rotate L xs) === head xs
    init (rotate L xs) === tail xs

-- | `prop_rotate_right` checks that `rotate` @R@ has the desired effects.
prop_rotate_right :: Property
prop_rotate_right = property $ do
    -- generate a list
    (xs :: [Int]) <- forAll $ Gen.list (Range.constant 2 100) natural

    -- check that right rotations have the desired effects
    head (rotate R xs) === last xs
    tail (rotate R xs) === init xs

rotateTests :: TestTree
rotateTests = testGroup "rotate" $
    [
        testCase "rotate L [] = []" $
            rotate L [] @?= ([] :: [Int])
    ,   testCase "rotate R [] = []" $
            rotate R [] @?= ([] :: [Int])
    ,   testProperty
            "rotate dir [x] = [x]"
            "prop_rotate_singleton"
            prop_rotate_singleton
    ,   testProperty
            "rotating in one direction, then the other yields the input"
            "prop_rotate"
            prop_rotate
    ,   testProperty
            "rotating left works correctly"
            "prop_rotate_left"
            prop_rotate_left
    ,   testProperty
            "rotating right works correctly"
            "prop_rotate_right"
            prop_rotate_right
    ]

--------------------------------------------------------------------------------

-- | `prop_rotations_length` tests that grids with at least 2x2 cells have
-- rows + columns many rotations
prop_rotations_length :: Property
prop_rotations_length = property $ do
    -- generate a random grid with at least 2x2 cells
    input <- forAll $ grid (Range.constant 2 10)

    -- classify the grid
    classify "small grids" $ gridSize input < 50
    classify "large grids" $ gridSize input >= 50

    -- check that the number of rotations returned is as expected
    length (rotations input) === rowCount input + columnCount input

-- | `prop_rotations_structure` checks that the structure of grids returned by
-- `rotations` is the same as that of the input grid.
prop_rotations_structure :: Property
prop_rotations_structure = property $ do
    -- generate a random grid
    input <- forAll $ grid (Range.constant 0 10)

    -- classify the grid
    classify "small grids" $ gridSize input < 50
    classify "large grids" $ gridSize input >= 50

    -- for every rotation returned by the function, check that it is
    -- structurally the same according to `rotationEq`.
    forM_ (rotations input) $ \rotation ->
        diff rotation rotationEq input

-- | `unsuitableRotations` @grid@ detects whether `rotations` of @grid@ would
-- return rotations that are unsuitable for the `prop_rotations_different`
-- property test.
unsuitableRotations :: Grid -> Bool
unsuitableRotations (MkGrid _ rs) =
    any unsuitableRow rs || any unsuitable (transpose $ map getCells rs)
    where unsuitableRow = unsuitable . getCells
          unsuitable cs = head cs == last cs

-- | `checkRotations` @original rotated@ checks that at least one of the rows
-- in @rotated@ is a rotation from the @original@. This check uses the
-- `rotate` function that students need to implement, so its results may be
-- unreliable if `rotate` is implemented incorrectly.
checkRotations :: [Row] -> [Row] -> Bool
checkRotations rs rs' =
    any (\(xs,xs') -> rotate L xs == xs') (rows ++ columns)
    where cs = map getCells rs
          cs' = map getCells rs'
          rows = zip cs cs'
          columns = zip (transpose cs) (transpose cs')

-- | `prop_rotations_rotated` checks that every grid returned by `rotations`
-- has at least one rotation according to `rotate`.
prop_rotations_rotated :: Property
prop_rotations_rotated = property $ do
    -- generate a random grid with at least 2x2 cells
    input@(MkGrid _ rows) <- forAll $ grid (Range.constant 2 10)

    -- check that the generated grid is suitable for this test:
    -- if any of the rows or columns contain the same actions in positions
    -- that would be rotated, then a rotation would not have any observable
    -- effect and we discard the grid
    when (unsuitableRotations input) discard

    -- classify the grid
    classify "small grids" $ gridSize input < 50
    classify "large grids" $ gridSize input >= 50

    -- check that each of the returned grids has at least one rotation
    forM_ (rotations input) $ \rotation@(MkGrid _ rs) ->
        unless (checkRotations rows rs) $ do
            -- show the result that failed the rotations check
            annotate (
                "The following result is not a valid rotation:\n" <>
                showPretty rotation
             )

            -- fail the test
            failure

rotationsTests :: TestTree
rotationsTests = testGroup "rotations"
    [
        testProperty
            "returns rows+columns many grids (for grids 2x2 and up)"
            "prop_rotations_length"
            prop_rotations_length
    ,   testProperty
            "returns grids with the same structure as the input"
            "prop_rotations_structure"
            prop_rotations_structure
    ,   testProperty
            "returns grids where a row/column has been rotated"
            "prop_rotations_rotated"
            prop_rotations_rotated
    ]

--------------------------------------------------------------------------------

-- | `followsFrom` @grid parent@ determines whether @grid@ is a grid that is
-- a rotation of @parent@ (via `rotations`).
followsFrom :: Grid -> Grid -> Bool
followsFrom step = (disable step `elem`) . map disable . rotations

stepsTests :: TestTree
stepsTests =
    withResource
        (loadAdvGrids >>= \grids -> pure $ zip3 [1..] grids (map steps grids))
        (const $ pure ()) $
    \getResults -> testGroup "steps"
        [
            testCaseSteps "results are a valid sequence (via rotations)" $
                \step -> getResults >>= \results ->
                -- do the following for every grid in the campaign
                forM_ results $ \(idx,input,moves) ->
                -- pair moves with their previous state
                forM_ (zip moves (input:moves)) $ \(move, previous) ->
                -- check that the state follows from the previous one
                unless (followsFrom move previous) $ do
                    step ("advanced grid #" <> show (idx :: Int))
                    assertFailure $
                        "\nA step in the sequence returned by `steps` for this grid:\n\n" <>
                        showPretty move <>
                        "\n\ndoes not follow from the previous grid\n\n" <>
                        showPretty previous <>
                        "\n\naccording to `rotations`."
        ,   testCaseSteps "sequence does not contain repeated steps" $
                \step -> getResults >>= \results ->
                -- do the following for every grid in the campaign
                forM_ results $ \(idx,_,r) ->
                -- check for duplicates
                when (length r /= length (nub r)) $ do
                    -- if we fail, calculate the actual duplicate steps
                    let duplicates = [xs | xs <- subsequences r
                                         , length xs > 1
                                         , all (==head xs) xs]
                    -- output information about the failure
                    step ("advanced grid #" <> show (idx :: Int))
                    assertFailure $ concat
                        [ "\nThere are repeated steps in the sequence for this grid:\n\n"
                        , intercalate ", and\n\n" $
                          map ((<> "\n\nappears multiple times") . showPretty . head) duplicates
                        ]
        ,   testCaseSteps "the last step in the sequence is a solution (via solve)" $
                \step -> getResults >>= \results ->
                -- do the following for every grid in the campaign
                forM_ results $ \(idx,_,r) ->
                -- check that there are solutions for the last step
                when (null $ solve (last r)) $ do
                    step ("advanced grid #" <> show (idx :: Int))
                    assertFailure $
                        "\nThe last step returned by `steps` for this grid is\n\n" <>
                        showPretty (last r) <>
                        "\n\nbut it has no solutions according to `solve`!"
        ]

--------------------------------------------------------------------------------

tests :: TestTree
tests = localOption (HedgehogShowReplay True)
      $ testGroup "Game"
        [   evalTests
        ,   after AllSucceed "Game.eval" applyTests
        ,   after AllSucceed "Game.apply" resultTests
        ,   statesTests
        ,   after AllSucceed "Game.states" candidatesTests
        ,   after AllSucceed "Game.result" $
            after AllSucceed "Game.states" solveRowTests
        ,   after AllSucceed "Game.solveRow" solveTests
        ,   rotateTests
        ,   after AllSucceed "Game.result" $
            after AllSucceed "Game.rotate" rotationsTests
        ,   after AllSucceed "Game.solve" $
            after AllSucceed "Game.rotations" stepsTests
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