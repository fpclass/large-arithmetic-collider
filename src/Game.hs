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
    deriving (Eq, Show)

-- | Represents a cell including whether it is enabled and its action.
data Cell = Cell Bool Action
    deriving (Eq, Show)

-- | A row has a target number and consists of zero or more cells.
data Row = Row Int [Cell]
    deriving (Eq, Show)

-- | A grid is comprised of the target numbers for all columns and the rows.
data Grid = Grid [Int] [Row]
    deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | `eval` @action total@ applies @action@ to the running @total@.
eval :: Action -> Int -> Int 
eval = undefined

-- | `apply` @cell total@ applies the action of @cell@ to the running @total@ 
-- if @cell@ is enabled.
apply :: Cell -> Int -> Int 
apply = undefined

-- | `result` @cells@ calculates the total produced by the actions of all 
-- enabled cells in @cells@ starting from 0.
result :: [Cell] -> Int 
result = undefined 

-- | `solveRow` @row@ finds solutions for @row@.
solveRow :: Row -> [Row]
solveRow = undefined

-- | `solve` @grid@ finds all solutions for @grid@.
solve :: Grid -> [Grid]
solve = undefined

-- | `rotations` @grid@ returns a list of 
rotations :: Grid -> [Grid]
rotations = undefined

-- | `steps` @grid@ finds the sequence of rotations that lead to a solution 
-- for @grid@ in the fewest number of rotations. The resulting list includes 
-- the solution as the last element.  
steps :: Grid -> [Grid]
steps = undefined

--------------------------------------------------------------------------------
