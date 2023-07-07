module Main where
import System.Environment
import System.Exit
import Prelude
import Control.Monad
import Data.Char


-- if the inputs are valid, it returns a tuple with a list of ints representing
-- the columns and the last argument is the filename
parseArgs :: [String] -> Maybe ([Int], String)
parseArgs args = do
    let n = length args
    guard (n >= 2)
    let cols = take (n-1) args
    let validCols = all (\col -> all (\digit -> isDigit digit && (read col :: Integer) > 0) col ) cols 
    guard validCols
    return (map (read :: String -> Int) cols, last args)

getWord :: [Int] -> [String] -> String
getWord cols line =
    unwords (foldl (\res col -> if (col-1) < length line then res ++ [line !! (col -1)] else res) [] cols)

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Nothing -> die "Invalid column inputs, valid inputs must be numbers greater than 0"
        Just (cols, filename) -> do
            if filename == "-"
            then do
                contents <- getContents
                let fileLines =map words (lines contents)
                mapM_ (\line -> putStrLn (getWord cols line)) fileLines
            else do
                file <- readFile filename
                let fileLines = map words (lines file)
                mapM_ (\line -> putStrLn (getWord cols line)) fileLines
