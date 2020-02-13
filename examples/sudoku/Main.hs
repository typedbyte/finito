module Main where

-- base
import Control.Monad ( forM_, replicateM, mapM_, when, zipWithM_ )
import Data.List     ( transpose )

-- split
import Data.List.Split ( chunksOf )

import AI.Search.FiniteDomain.Int

-- This is our test scenario.
testSudoku :: [Int]
testSudoku =
  [ 0, 0, 4,   0, 0, 0,    5, 0, 0,
    0, 0, 0,   0, 0, 0,    0, 4, 9,
    6, 0, 3,   0, 0, 0,    8, 0, 0,
    
    0, 0, 2,   0, 0, 0,    0, 9, 6,
    7, 0, 5,   0, 0, 0,    2, 0, 0,
    0, 9, 0,   8, 4, 0,    0, 0, 0,
    
    0, 0, 0,   6, 3, 0,    0, 0, 5,
    0, 0, 0,   2, 8, 0,    0, 0, 0,
    3, 7, 0,   1, 0, 0,    0, 0, 2 ]

-- Some helper functions to prepare and print Sudoku values.
rows :: [a] -> [[a]]
rows = chunksOf 9

columns :: [a] -> [[a]]
columns = transpose . rows

blocks :: [a] -> [[a]]
blocks
  = concat
  . fmap (fmap concat . transpose)
  . chunksOf 3
  . chunksOf 3
  . chunksOf 3

printSudoku :: [Int] -> IO ()
printSudoku = mapM_ (putStrLn . fmap replace . show) . rows
  where replace '0' = '_'
        replace ',' = ' '
        replace c   = c

-- This function transforms a given Sudoku puzzle into a constraint.
toConstraint :: [Int] -> FD (Labeling [Int])
toConstraint sudoku = do
  vars <- replicateM 81 newVar
  zipWithM_ (\v n -> when (n > 0) (v #= int n)) vars sudoku
  mapM_ (between 1 9) vars
  mapM_ allDifferent (rows vars)
  mapM_ allDifferent (columns vars)
  mapM_ allDifferent (blocks vars)
  labeling vars

-- Put it all together.
main :: IO ()
main = do
  putStrLn "Test Sudoku puzzle:"
  printSudoku testSudoku
  case runFD (toConstraint testSudoku) of
    Unsolvable _ -> putStrLn "Sudoku is unsolvable."
    Unbounded  _ -> putStrLn "The constraint formulation is wrong."
    Solutions xs ->
      forM_ xs $ \solution -> do
        putStrLn "Found a solution:"
        printSudoku solution