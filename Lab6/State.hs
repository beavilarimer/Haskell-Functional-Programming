module State where

import Control.Monad
import Control.Monad.State
import Data.IORef


--
-- Part A.
--

-- While loop in the IO monad.
whileIO :: IO Bool -> IO () -> IO () 
whileIO test block = 
  do b <- test 
     when b (do block
                whileIO test block)

-- While loop in the state monad.
whileState :: (s -> Bool) -> State s () -> State s ()
whileState test body = 
  do s0 <- get
     when (test s0)
          (do body
              whileState test body)

-- A.1
factIO :: Integer -> IO Integer
factIO n
  | n < 0 = error "Input should not be negative."
  | otherwise = do
    count <- newIORef n
    total <- newIORef 1
    whileIO
      (do c <- readIORef count
          return (c > 0))
      (do c <- readIORef count
          t <- readIORef total
          writeIORef count (c - 1)
          writeIORef total (t * c))
    readIORef total

-- A.2
factState :: Integer -> Integer
factState n = evalState factStateHelper (n, 1)
  
factStateHelper :: State (Integer, Integer) Integer
factStateHelper = 
  do
    whileState (\(count, _) -> count > 0)
      (do 
        (count, total) <- get
        put (count - 1, total * count))
    (_, total) <- get
    return total

-- A.3
fibIO :: Integer -> IO Integer
fibIO n
  | n < 0 = error "Input should not be negative."
  | otherwise = do
    count <- newIORef n
    prev1 <- newIORef 0
    prev2 <- newIORef 1
    whileIO
      (do c <- readIORef count
          return (c > 0))
      (do c <- readIORef count
          p1 <- readIORef prev1
          p2 <- readIORef prev2
          writeIORef count (c - 1)
          writeIORef prev1 p2
          writeIORef prev2 (p1 + p2))
    readIORef prev1
    
-- A.4
fibState :: Integer -> Integer
fibState n = evalState fibStateHelper (0, 1, n)

fibStateHelper :: State (Integer, Integer, Integer) Integer
fibStateHelper = do
  whileState (\(_, _, count) -> count > 0)
    (do 
      (prev1, prev2, count) <- get
      put (prev2, prev1 + prev2, count - 1))
  (prev1, _, _) <- get
  return prev1

--
-- Part B.
--

-- >>=
{-
Looking at the derivation of >>= from Lecture we adjust to fit the Reader
f' :: (a, r) -> b
g' :: (b, r) -> c

Curried versions of the signatures where instead of the state function we have
the reader, thus we have only one output, whereas in the derivation presented 
in lecture we would have also had the state as an output

f'' :: a -> r -> b
f'' x r = f' (x, y)
-- Wrapping the right hand side of the f'' function in a Reader cosntructor
f x = Reader (\r -> f' x)

g'' :: b -> r -> c
g'' y r = g' (y, x)
-- Follows from above
g y = Reader (\r -> g' y)

Likely, we define the monadic composition of f and g(h) in terms of f' and g' 
h :: a -> Reader r c
h x = Reader (\r -> h' x)

Recall the following
h = f >=> g
h x = f x >>= g
f x >>= g = h x
f x >>= g = Reader (\r -> h' x)

Let us calculate:
f x >>= g 
  = Reader (\r -> h' x)
  = Reader (\r ->                 we can expand by using the definition of h'
      let y = f' x
          z = g' y
      in z)
  = Reader (\r ->
      let y = f' x in
        g' y)

f x >>= g 
  = Reader (\r ->
      let y = f' x in
          g' y)

  = Reader (\r -> 
      let (Reader ff) = f x
          y = ff r
      in g' y
        
eliminating dependence on g'
f x >>= g 
  = Reader (\r ->
      let (Reader ff) = f x
          y = ff r
          (Reader gg) = g y
      in gg)

Substituting mx for f x to get:
mx >>= g
  = Reader (\r ->
      let (Reader ff) = mx
          y = ff r
          (Reader gg) = g y
      in gg r)

we can substitute the variables names to show the equivalency
switch g to f, y to x and so on

let us use the helper funtion
runReader :: Reader r a -> r -> a
runReader (Reader f) = f

mx >>= f
  = runReader(Reader (\r ->
      let runReader (Reader ff) = mx
          x = ff r
          runReader (Reader gg) = f x
      in gg r))

mx >>= f
  = (\r ->
      let ff = mx
          x = ff r
          gg = f x
      in gg r)

which is equivalent to: 
mx >>= f = Reader (\r ->
             let (Reader g) = mx
                 x = g r
                 (Reader h) = f x
             in h r)
-}

-- return
{-
(a, s) -> b

the identity function in this form would be:
id_reader (x, r) = x
id_reader' x r = x
id_reader' x = \r -> x

id_reader_monad :: a -> Reader r a
id_reader_monad x = Reader (\r -> x)

return :: a -> Reader r a
return x = Reader (\r -> x)
-}
