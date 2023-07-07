--
-- Sudoku.hs
--

--
-- This program reads a Sudoku problem from a file and
-- outputs the solution to stdout.
--

module Main where

import Control.Monad
import Data.Array.IO
import Data.Char
import Data.List
import System.Environment
import System.Exit
import System.IO

usage :: IO ()
usage = hPutStrLn stderr $ "usage: sudoku filename"

type Sudoku = IOArray (Int, Int) Int


-- Read a file's contents into a Sudoku array.
readSudoku :: FilePath -> IO Sudoku
readSudoku f = do
  s <- readFile f
  let ls = lines s in
    if okLines ls
       then newListArray ((1, 1), (9, 9)) (map charToInt (concat ls))
       else error "readSudoku: invalid string input"
  where
    -- Check that the string input is a valid representation of a Sudoku board.
    okLines :: [String] -> Bool
    okLines ss =
      and [length ss == 9,
           all (\s -> length s == 9) ss,
           all okChar (concat ss)]

    okChar :: Char -> Bool
    okChar '.' = True
    okChar c | ord c >= ord '0' && ord c <= ord '9' = True
    okChar _ = False

    charToInt :: Char -> Int
    charToInt '.' = 0
    charToInt c   = ord c - ord '0'
  

solveSudoku :: Sudoku -> IO Bool
solveSudoku s = iter s (1, 1)
  where
    -- Solve a Sudoku board starting from location (i, j).
    -- All "previous" locations are assumed to have been filled.
    -- If the board is solveable, return True; if not, return False.
    -- In the latter case the board will not have changed.
    iter :: Sudoku -> (Int, Int) -> IO Bool
    iter s (i, j) =
      case (i, j) of
        (10, _) -> return True
        (_, 10) -> iter s (i+1, 1)
        _ -> do
          c <- readArray s (i, j)
          if c == 0
          then do
            okValues <- getOKValues s (i, j)
            if null okValues
              then return False
            else do
              solve <- iter' s (i, j) okValues
              if solve
                then return True  -- Solution found
              else do
                writeArray s (i, j) 0  -- Reset the cell to zero (unmake the move)
                return False  -- No solution found 
          else do 
            iter s (i, j + 1)

    iter' :: Sudoku -> (Int, Int) -> [Int] -> IO Bool
    iter' s (i, j) values = 
      case values of
      [] -> return False  -- No valid values to try, board is unsolvable
      (x:xs) -> do
        writeArray s (i, j) x  -- Set the cell to a valid value
        solve <- iter s (i, j + 1)  -- Try to solve the board from the next cell
        if solve
          then return True  -- Solution found
          else iter' s (i, j) xs  -- Try the next valid value

    getRow :: Sudoku -> Int -> IO [Int]
    getRow s i = sequence [readArray s (i,j) | j <- [1..9]]

    -- Return the ith column in a Sudoku board as a list of Ints.
    getCol :: Sudoku -> Int -> IO [Int]    
    getCol s j = sequence [readArray s (i,j) | i <- [1..9]]

    -- Return the box covering location (i, j) as a list of Ints.
    getBox :: Sudoku -> (Int, Int) -> IO [Int]
    getBox s (i,j) = do
      let boxStartI = (i-1) `div` 3 * 3 + 1
          boxStartJ = (j-1) `div` 3 * 3 + 1
      sequence [readArray s (boxStartI+di, boxStartJ+dj) | di <- [0,1,2], dj <- [0,1,2]]

    getOKValues :: Sudoku -> (Int, Int) -> IO [Int]
    getOKValues s (i,j) = do
      rowValues <- getRow s i
      colValues <- getCol s j
      boxValues <- getBox s (i,j)
      let usedValues = nub (rowValues ++ colValues ++ boxValues)
      let validValues = foldl (\res val -> if val `notElem` usedValues then res ++ [val] else res) [] [1..9]
      return validValues

    

-- Print a Sudoku board to stdout.
printSudoku :: Sudoku -> IO ()
printSudoku s = iter s 1 1
  where
    iter :: Sudoku -> Int -> Int -> IO ()
    iter s i j = 
      unless (i > 9)
        (do c <- readArray s (i, j)
            putChar $ intToChar c
            if j == 9 
               then putChar '\n' >> iter s (i + 1) 1
               else iter s i (j + 1))

    intToChar :: Int -> Char
    intToChar 0 = '.'
    intToChar n | n >= 1 && n <= 9 = intToDigit n
    intToChar m = error $ "printSudoku: invalid integer in array: " ++ show m


main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
     then usage >> exitFailure
     else
       do sudoku <- readSudoku (head args) -- read board contents into array
          solved <- solveSudoku sudoku
          if solved
             then printSudoku sudoku >> exitSuccess
             else putStrLn "No solution exists." >> exitFailure

