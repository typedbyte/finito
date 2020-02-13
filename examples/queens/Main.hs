module Main where

-- base
import Control.Monad      ( forM_, replicateM )
import Data.List          ( find )
import System.Environment ( getArgs )

import AI.Search.FiniteDomain.Int

-- A helper function to print the chess board.
printChess :: Int -> Int -> [(Int,Int)] -> IO ()
printChess width height solutions = do
  forM_ [1..height] $ \row -> do
    putStr "[ "
    case find ((== row) . fst) solutions of
      Nothing ->
        forM_ [1..width] line
      Just (_,c) -> do
        forM_ [1..c-1] line
        putStr "Q "
        forM_ [c+1..width] line
    putStrLn "]"
  where
    line _ = putStr "_ "

-- This function transforms a given Queens puzzle into a constraint.
toConstraint :: Int -> Int -> Int -> FD (Labeling [(Int,Int)])
toConstraint count width height = do
  rows    <- replicateM count newVar
  columns <- replicateM count newVar
  forM_ rows    $ between 1 (int height)
  forM_ columns $ between 1 (int width)
  secureQueens (zip rows columns)
  result <- labeling (rows ++ columns)
  pure $ do
    solution <- result
    pure (zip (take count solution) (drop count solution))
  where
    secureQueens [] = pure ()
    secureQueens (q:qs) = do
      secureLines qs q
      secureQueens qs
    secureLines [] _ = pure ()
    secureLines ((row,col):qs) queen@(r,c) = do
      col #/= c
      row #>  r
      abs (col - c) #/= row - r
      secureLines qs queen

-- Put it all together.
main :: IO ()
main = do
  putStrLn "Expecting [queen count, board width, board height] as command line arguments."
  putStrLn "Default is [8, 8, 8]."
  args <- getArgs
  let (count, width, height) =
        case args of
          [qc, w, h] -> (read qc, read w, read h)
          _          -> ( 8, 8, 8)
  putStrLn $ "Placing " ++ show count ++ " queen(s) ..."
  case runFD (toConstraint count width height) of
    Unsolvable _ -> putStrLn "Queens puzzle is unsolvable."
    Unbounded  _ -> putStrLn "The constraint formulation is wrong."
    Solutions xs -> do
      forM_ xs $ \solution -> do
        putStrLn "Found a solution:"
        printChess width height solution