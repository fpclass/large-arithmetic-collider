--------------------------------------------------------------------------------
-- Functional Programming - Large Arithmetic Collider Project
--------------------------------------------------------------------------------
-- Copyright (c) 2022 Michael B. Gale (michael@fpclass.online)
--
-- This source code is subject to the terms and conditions found in the LICENSE
-- file in the root directory of this source tree.
--------------------------------------------------------------------------------

-- | This module contains computations which can be used to convert grids and
-- related data types to textual representation, suitable for terminal output.
module Render (
    renderAction,
    renderCell,
    renderRow,
    renderGrid
) where

--------------------------------------------------------------------------------

import Data.List (intercalate)

import Game

--------------------------------------------------------------------------------

-- | `pad` @length string@ pads @string@ with whitespace on the left so that
-- the result has @length@-many characters.
pad :: Int -> String -> String
pad l xs = replicate (l-length xs) ' ' ++ xs

-- | `renderAction` @action@ converts @action@ to a string representation.
renderAction :: Action -> String
renderAction (Add n) = '+' : show n
renderAction (Sub n) = '-' : show n

-- | `renderCell` @force cell@ converts @cell@ to a string. If @force@ is true,
-- the cell is always rendered in the enabled state regardless of its actual
-- state.
renderCell :: Bool -> Cell -> String
renderCell fc (MkCell e a)
    | fc || e   = renderAction a
    | otherwise = replicate (length $ renderAction a) ' '

-- | `renderRow` @force maxLabelWidth maxCellWidth row@ converts @row@ to a
-- string representation. Cells will always be shown regardless of state if
-- @force@ is `True`. @maxLabelWidth@ determines the minimum number of
-- characters that should be occupied by the row label. @maxCellWidth@
-- determines the minimum number of characters that should be occupied by the
-- cell contents.
renderRow :: Bool -> Int -> Int -> Row -> String
renderRow fc maxLabelWidth maxCellWidth (MkRow t cs) =
    intercalate " | " $ pad maxLabelWidth (show t) :
    map (pad maxCellWidth . renderCell fc) cs

-- | `renderGrid` @force grid@ draws @grid@ to the standard output. If @force@
-- is true, cells will always be rendered as if they were active, even when
-- they are not.
renderGrid :: Bool -> Grid -> IO ()
renderGrid fc (MkGrid cts rs) = do
    let -- convert the column labels to strings
        labels = map show cts
        -- calculate the max. width of the column labels
        maxLW  = maximum (map length labels)
        -- convert the row labels to strings
        rowLS  = map (\(MkRow t _) -> show t) rs
        -- calculate the max. width of the row labels
        maxRW  = maximum (map length rowLS)
        -- determine the max. width of cells
        cells  = map (\(MkRow _ cs) -> maximum (map (length . renderCell fc) cs)) rs
        cellW  = max (maximum cells) maxLW
        -- generate the header
        header = intercalate " | " $ replicate maxRW ' ' : map (pad cellW) labels
        -- convert the rows to strings
        rows   = map (renderRow fc maxRW cellW) rs
    -- print the header to the standard output
    putStrLn header
    -- draw a line underneath the header
    putStrLn (replicate (length header) '-')
    -- print the rows to the standard output
    mapM_ putStrLn rows

--------------------------------------------------------------------------------
