-- module Lab4a where
import Data.Char


-- A.1
myPutStrLn :: String -> IO ()
myPutStrLn "" = putChar '\n'
myPutStrLn (c:cs) =
  putChar c >> myPutStrLn cs

-- A.2
greet :: String -> IO ()
greet name = putStrLn ("Hello, " ++ name ++ "!")

-- A.3
-- Ask the user for his/her name, then print a greeting.
greet2a :: IO ()
greet2a = 
  putStr "Enter your name: " >> 
  getLine >>= \name ->
  putStr "Hello, " >>
  putStr name >>
  putStrLn "!"

greet2b :: IO ()
greet2b = do
  putStr "Enter your name: " >> getLine >>=
    \name -> case name of 
        [] -> fail "Pattern match failure in do block"
        _-> putStr "Hello, " >> putStr name >> putStrLn "!"
        

{-These two functions behave the same except when there is an empty string
the greet2a functiong simply prints out "Hello, !". While the greet2b function
raises the exception "Exception: user error (Pattern match failure in do block)"
-} 

-- A.4
-- Ask the user for his/her name, then print a greeting.
-- Capitalize the first letter of the name.
greet3 :: IO ()
greet3 = do
  putStr "Enter your name: "
  (n:ns) <- getLine
  let name = toUpper n : ns
  putStr "Hello, "
  putStr name
  putStrLn "!"

greet3a :: IO ()
greet3a =
  putStr "Enter your name: " >>
  getLine >>= \name ->
    let upperName = toUpper (head name) : tail name
    in putStr "Hello, " >>
       putStr upperName >>
       putStrLn "!"


greet3b :: IO ()
greet3b =
  putStr "Enter your name: " >>
  getLine >>= \input->
    case input of
      (n:ns) 
        ->let name = toUpper n : ns
        in putStr "Hello, " >>
           putStr name >>
           putStrLn "!"
      _ -> fail "Pattern match failure in do block" 


{-These two functions have the same output except when we have the empty string
greet3a has the exception "Exception: Prelude.head: empty list". While, greet3b
has the exception "Exception: user error (Pattern match failure in do block)"-}