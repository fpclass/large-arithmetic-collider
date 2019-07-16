--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 1: Large Arithmetic Collider                                    --
--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import Control.Monad 

import Data.Char (isDigit)

import Text.Printf

import System.Directory (listDirectory)
import System.IO
import System.FilePath
import System.Clock

import Game
import Render
import Level

--------------------------------------------------------------------------------

-- | `promptInt` @prompt@ is a computation which outputs @prompt@ to the 
-- standard output and waits for the user to provide some input. If the input 
-- is comprised entirely of digits, it will be converted to an integer and 
-- returned. Otherwise, the user will be re-prompted.
promptInt :: String -> IO Int 
promptInt prompt = do 
    -- display the input prompt and read from the standard input
    putStr prompt
    xs <- getLine 

    -- check that all characters in the input are digits
    if all isDigit xs && not (null xs)
    then pure (read xs)
    else do 
        putStrLn "Not a number! Try again, please."
        promptInt prompt

-- | `playLevel` @level@ is a computation which uses the `solve` and `steps`
-- functions to play @level@.
playLevel :: Level -> IO ()
playLevel (Level p g) = do 
    -- render the grid to the standard output
    renderGrid True g

    -- obtain the current time
    start <- getTime ProcessCPUTime

    -- find solutions for the grid
    let sols = solve g 

    -- check whether solutions were found via `solve` or not
    if null sols 
    then do 
        -- stop measuring time
        end <- getTime ProcessCPUTime 

        -- calculate how long `solve` took to run
        let time = fromIntegral (sec end - sec start)

        -- report that we were not able to find a solution
        printf "No solution found. Trying to rotate the grid...\n"

        -- start measuring time again
        start' <- getTime ProcessCPUTime 

        -- use the `steps` function to obtain a list of steps that
        -- are needed to arrive at a solution
        let gs = steps g
            n  = length gs

        -- report how many steps are in the sequence
        printf "Found a solution via %d rotation(s):\n" n

        -- stop measuring time
        end' <- getTime ProcessCPUTime 

        -- render all the steps
        forM_ gs $ \step -> renderGrid True step

        -- render the solution 
        putStrLn "The solution is:"
        renderGrid False (last gs)

        -- calculate how long `nextMove` took to run
        let nextMoveTime = time + fromIntegral (sec end' - sec start')

        printf "The time taken was %d seconds and the par is %d rotation(s) (%d needed).\n" 
            (nextMoveTime :: Integer) p n
    else do 
        -- print the first solution found
        putStrLn "The computer found a solution:" 
        renderGrid False (head sols)

        -- stop measuring time
        end <- getTime ProcessCPUTime
        
        -- calculate how long `solve` took to run
        let solveTime = fromIntegral (sec end - sec start)

        printf "This required no rotations, the par is %d.\n" p
        printf "The time taken was %d seconds.\n" (solveTime :: Integer)

    putStr "Press enter to advance to the next level..."
    void getLine

-- | `startCampaign` @campaign@.
startCampaign :: Campaign -> IO ()
startCampaign (Campaign _ _ dir) = do 
    -- try to load the levels for this campaign
    mlevels <- loadLevels ("levels" </> dir)

    -- determine whether we successfully loaded the levels
    case mlevels of 
        Left err -> printf "Unable to load levels:\n%s\n\n" err
        Right rs -> go 1 rs
            where go :: Int -> [Level] -> IO ()
                  go _ []     = do 
                    printf "All levels in this campaign were completed!\n\n"
                  go n (l:ls) = do 
                    printf "\nLevel %d\n\n" n
                    playLevel l
                    go (n+1) ls

-- | `selectCampaign` is a computation which implements the campaign 
-- selection menu.
selectCampaign :: IO ()
selectCampaign = do 
    -- load the list of campaigns from disk
    mcs <- listCampaigns

    -- determine whether loading the campaign list was successful
    case mcs of
        Nothing -> putStrLn "Unable to load campaigns!"
        Just cs -> do 
            -- display a menu listing all the avail. campaigns
            putStrLn "Please select a campaign:\n" 
            forM_ (zip [0..] cs) $ \(i,c) -> do 
                -- attempt to determine how many levels there are in this 
                -- campaign by listing all the files in the directory 
                -- containing the levels for this campaign
                ls <- listDirectory ("levels" </> campaignPath c)

                -- print the campaign header to the standard output
                printf "%d. %s - %d levels\n" 
                    (i :: Int) (campaignTitle c) (length ls)
                
                -- print the campaign description
                printf "%s\n\n" (campaignDesc c)

            -- prompt the user to enter the ID of a campaign
            n <- promptInt "Select campaign ID: "

            -- check that the entered ID is valid and, if so,
            -- play the selected campaign. otherwise, re-prompt
            -- the user to select a campaign
            if n >= 0 && n < length cs 
            then startCampaign (cs !! n)
            else putStrLn "Not a valid campaign ID."

            selectCampaign

-- | The main entry point for this program.
main :: IO ()
main = do 
    -- disable input and output buffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin  NoBuffering

    -- let the user select a campaign
    selectCampaign

--------------------------------------------------------------------------------
