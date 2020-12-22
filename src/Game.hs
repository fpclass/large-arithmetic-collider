--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 1: Large Arithmetic Collider                                    --
--------------------------------------------------------------------------------

module Game where 

--------------------------------------------------------------------------------

-- | Represents different actions (including their parameters) that a cell can 
-- have on a row or column total.
data Action 
    = Add Int 
    | Sub Int 
    deriving (Eq, Ord, Show)

-- | Represents a cell including whether it is enabled and its action.
data Cell = MkCell Bool Action
    deriving (Eq, Show)

-- | A row has a target number and consists of zero or more cells.
data Row = MkRow Int [Cell]
    deriving (Eq, Show)

-- | A grid is comprised of the target numbers for all columns and the rows.
data Grid = MkGrid [Int] [Row]
    deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | `eval` @action total@ applies @action@ to the running @total@. 
-- For example:
--
-- >>> eval (Add 5) 3
-- 8
--
-- >>> eval (Sub 1) 3
-- 2
--
---------------------------------------
-- [Add your explanation of how your implementation works here]
---------------------------------------
eval :: Action -> Int -> Int 
eval = undefined

-- | `apply` @cell total@ applies the action of @cell@ to the running @total@ 
-- if @cell@ is enabled. For example:
--
-- >>> apply (MkCell True (Add 5)) 3
-- 8
--
-- >>> apply (MkCell False (Add 5)) 3
-- 3
--
---------------------------------------
-- [Add your explanation of how your implementation works here]
---------------------------------------
apply :: Cell -> Int -> Int 
apply = undefined

-- | `result` @cells@ calculates the total produced by the actions of all 
-- enabled cells in @cells@ starting from 0. For example:
--
-- >>> result []
-- 0
--
-- >>> result [MkCell True (Add 5), MkCell False (Add 5), MkCell True (Sub 1)]
-- 4
--
---------------------------------------
-- [Add your explanation of how your implementation works here]
---------------------------------------
result :: [Cell] -> Int 
result = undefined 

-- | `states` @cell@ is a function which returns a list with _exactly_ two
-- elements that represent the two different states @cell@ can be in. For
-- example:
--
-- >>> states (MkCell False (Add 5))
-- [MkCell True (Add 5), MkCell False (Add 5)]
--
---------------------------------------
-- [Add your explanation of how your implementation works here]
---------------------------------------
states :: Cell -> [Cell]
states = undefined

-- | `candidates` @cells@ is a function which, given a list of cells in a row,
-- produces all possible combinations of states for those cells. For example:
-- 
-- >>> candidates [MkCell False (Add 5), MkCell False (Sub 1)]
-- [ [MkCell False (Add 5), MkCell False (Sub 1)]
-- , [MkCell False (Add 5), MkCell True (Sub 1)]
-- , [MkCell True (Add 5), MkCell False (Sub 1)]
-- , [MkCell True (Add 5), MkCell True (Sub 1)]
-- ]
--
---------------------------------------
-- [Add your explanation of how your implementation works here]
---------------------------------------
candidates :: [Cell] -> [[Cell]]
candidates = undefined

-- | `solveRow` @row@ finds solutions for @row@. For example:
--
-- >>> solveRow (MkRow 5 [MkCell False (Add 5), MkCell False (Sub 1)])
-- [[MkCell True (Add 5), MkCell False (Sub 1)]]
--
-- >>> solveRow (MkRow 5 [MkCell False (Add 5), MkCell False (Add 5)])
-- [ MkRow 5 [MkCell True (Add 5), MkCell False (Add 5)] 
-- , MkRow 5 [MkCell False (Add 5), MkCell True (Add 5)]
-- ]
--
---------------------------------------
-- [Add your explanation of how your implementation works here]
---------------------------------------
solveRow :: Row -> [Row]
solveRow = undefined

-- | `solve` @grid@ finds all solutions for @grid@. For example:
--
-- >>> let row0 = MkRow 3 [MkCell False (Add 3), MkCell False (Add 5)]
-- >>> let row1 = MkRow 4 [MkCell False (Add 2), MkCell False (Add 2)]
-- >>> solve (MkGrid [5, 2] [row0, row1])
-- [ MkGrid [5,2] [ MkRow 3 [MkCell True (Add 3), MkCell False (Add 5)]
--                , MkRow 4 [MkCell True (Add 2), MkCell True (Add 2)]
--                ]
-- ]
--
---------------------------------------
-- [Add your explanation of how your implementation works here]
---------------------------------------
solve :: Grid -> [Grid]
solve = undefined

-- | `rotations` @grid@ returns a list of grids containing all possible ways 
-- to rotate @grid@. This means the resulting list should normally have 
-- rows + columns many elements. For example:
--
-- >>> let row0 = MkRow 3 [MkCell False (Add 3), MkCell False (Add 5)]
-- >>> let row1 = MkRow 4 [MkCell False (Add 2), MkCell False (Add 2)]
-- >>> rotations (MkGrid [5, 2] [row0, row1])
-- [ MkGrid [5,2] [ MkRow 3 [MkCell False (Add 5), MkCell False (Add 3)]
--                , MkRow 4 [MkCell False (Add 2), MkCell False (Add 2)]
--                ]
-- , MkGrid [5,2] [ MkRow 3 [MkCell False (Add 3), MkCell False (Add 5)]
--                , MkRow 4 [MkCell False (Add 2), MkCell False (Add 2)]
--                ]
-- , MkGrid [5,2] [ MkRow 3 [MkCell False (Add 2), MkCell False (Add 5)]
--                , MkRow 4 [MkCell False (Add 3), MkCell False (Add 2)]
--                ]
-- , MkGrid [5,2] [ MkRow 3 [MkCell False (Add 3), MkCell False (Add 2)]
--                , MkRow 4 [MkCell False (Add 2), MkCell False (Add 5)]
--                ]
-- ]
--
---------------------------------------
-- [Add your explanation of how your implementation works here]
---------------------------------------
rotations :: Grid -> [Grid]
rotations = undefined

-- | `steps` @grid@ finds the sequence of rotations that lead to a solution 
-- for @grid@ in the fewest number of rotations. The resulting list includes 
-- the solution as the last element. You may assume that this function will
-- never be called on a @grid@ for which there are solutions returned by
-- `solve`. The states of intermediate grids in the resulting list
-- are irrelevant - only the ones of the final grid need to be set correctly.
--
-- >>> let row0 = MkRow 3 [MkCell False (Add 2), MkCell False (Add 3)]
-- >>> let row1 = MkRow 4 [MkCell False (Add 5), MkCell False (Add 2)]
-- >>> steps (MkGrid [5, 2] [row0, row1])
-- [ MkGrid [5, 2] [ MkRow 3 [ MkCell False (Add 5), MkCell False (Add 3)] 
--                 , MkRow 4 [ MkCell False (Add 2), MkCell False (Add 2)]
--                 ]  
-- , MkGrid [5, 2] [ MkRow 3 [ MkCell True (Add 3), MkCell False (Add 5)] 
--                 , MkRow 4 [ MkCell True (Add 2), MkCell True (Add 2)]
--                 ] 
-- ]
--
---------------------------------------
-- [Add your explanation of how your implementation works here]
---------------------------------------
steps :: Grid -> [Grid]
steps = undefined

--------------------------------------------------------------------------------
