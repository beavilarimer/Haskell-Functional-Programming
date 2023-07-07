module Main where
import System.Environment
import System.Exit
import Prelude



main :: IO ()
main  = do
    -- return command line arguments
    args <- getArgs
    case args of
        [filename] -> do 
            file <-readFile filename
            let fileLines = lines file
            let reversedLines = reverse fileLines
            mapM_ putStrLn reversedLines
            exitSuccess
        _ -> die "usage: reverse filename"
        
    