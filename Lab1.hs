module Lab1 where
import GHC.Data.FastString (zString)
import Data.Map (takeWhileAntitone)
import GHC.Core (Unfolding(BootUnfolding))
import Data.List
-- import qualified GHC.Num as A.8

-- A.1.a sum of squares
infixl 7 +*
(+*) :: Double -> Double -> Double
x +* y = (x * x) + (y * y)

-- A.1.b exclusive or
infixr 3 ^||
(^||) :: Bool -> Bool -> Bool
(^|| ) True x = not x
(^|| ) False x = x

-- A.2
rangeProduct :: Integer -> Integer -> Integer
rangeProduct a b
    | a > b = error "first is greater than second"
    | a == b = a
    | otherwise = a * rangeProduct (a + 1) b

-- A.3
prod :: [Integer] -> Integer
prod = foldr (*) 1

rangeProduct2 :: Integer-> Integer -> Integer
rangeProduct2 a b 
    | a > b = error "first is greater than second"
    | otherwise = prod [a..b]

-- A.4.a
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ [] _ = []
map2 _ _ [] = []
map2 f (x:xs) (y:ys) =  f x y : map2 f xs ys

-- A.4.b
map3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
map3 _ [] _ _ = []
map3 _ _ [] _ = []
map3 _ _ _ [] = []
map3 f (x:xs) (y:ys) (z:zs) = f x y z : map3 f xs ys zs

-- A.5
{-
    (sum .) . map2 (*)
    ((sum . x) . map2 (x)) lst1 lst2
    ((sum . x) . (map2 (*) lst1)) lst2
    ((sum . x) (map2 (*) lst1)) lst2
    ((sum . (map2 (x) lst1))) lst2
    ((sum (map2 (x) lst1))) lst2
    (sum map2 (*) lst1) lst2
    sum (map2 (*) lst1 lst2)
-}

-- A.6
-- sum [x | x <- [1..999], mod x 3 == 0 || mod x 5 == 0]
-- 233168

-- A.7
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : sieve [y | y <-xs, mod y x /= 0]

primes :: [Integer]
primes = sieve [2..]

-- sum (takeWhile (< 10000) primes)
-- 5736396

-- A.8.a
balancedParentheses :: String -> Bool
balancedParentheses xs = iter xs 0
    where
        iter :: String -> Int -> Bool 
        iter string n
            | null string = n == 0
            | n < 0 = False    
            | head string == '(' = iter (tail string) (n + 1)
            | head string == ')' = iter (tail string) (n - 1)
            | otherwise = iter (tail string) n

-- A.8.b
balancedParentheses2 :: String -> Bool
balancedParentheses2 xs
    | foldl' helper 0 xs == 0 = True
    | otherwise = False
    where
        helper :: Int -> Char -> Int
        helper n char
            | n < 0 = n
            | char =='(' = n + 1
            | char == ')' = n - 1
            | otherwise = n
        

-- A.8.c
balancedParentheses3 :: String -> Bool
balancedParentheses3 s = test (scanl' (+) 0 (map ctoi s))
  where
    test :: [Int] -> Bool
    test string
        | null string == True = True
        | all (>= 0) string  && last string == 0 = True
        | otherwise = False
    
    ctoi :: Char -> Int
    ctoi char
        | char == '(' = 1
        | char == ')' = -1
        | otherwise = 0
    
    
    

-- B.1
{-
sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + sumList (xs)

The original code is redundant, it is poor style to not decompose when
recursively working with lists.
-}

-- B.2
{-
largest :: [Integer] -> Integer
largest [] = error "empty list"
largest [x] = x
largest (x:xs) = max x (largest xs)

The original code could be more efficient because these could avoid making a
call to length by using list decomposition. For example, in the first case it
is unnecessary to call length because we could simply match against an empty
list

largest xs
    | length xs == 0 = error "empty list"
    | length xs == 1 = head xs

-}

-- C.1
{-
    fib 3
    fib (3 - 1) + fib (3 - 2)
    fib 2 + fib (3 - 2)
    (fib (2 - 1) + fib (2 - 2)) + fib (3 - 2)
    (fib 1 + fib (2-2)) + fib (3 - 2)
    (1 + fib (2-2)) + fib (3 - 2)
    (1 + fib 0) + fib (3 - 2)
    (1 + 0) + fib (3 - 2)
    1 + fib (3 - 2)
    1 + fib 1
    1 + 1
    2
-}

-- C.2
-- given definition
{-
    fact :: Integer -> Integer
    fact n = n * fact (n - 1)
    fact 0 = 1

    fact 3
    3 * fact (3 - 1)
    3 * ((3 - 1) * fact (3 - 1 - 1)) 
    3 * ((3 - 1) * ((3 - 1 - 1) * fact (3 - 1 - 1 - 1)))
    
    Because of lazy evaluation it is not needed to evaluate the argument of the
    recursive call to the function. As we can see this causes the function to
    infinetely loop. 
-}

-- fixed definition
fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

{-
    fact 3
    3 * fact (3 - 1)
    3 * fact 2
    3 * (2 * fact (2 - 1))
    3 * (2 * fact 1)
    3 * (2 * (1 * (fact (1 - 1))))
    3 * (2 * (1 * (fact 0)))
    3 * (2 * (1 * 1))
    3 * (2 * 1))
    3 * 2
    6
-}

-- C.3
{-
    reverse [1, 2, 3]
    iter [1, 2, 3] []
    iter [2, 3] (1 : [])
    iter [3] (2 : (1 : []))
    iter []  (3 : (2 : (1 : [])))
    3 : (2 : (1 : []))
    3 : (2 : [1])
    (3 : [2, 1])
    [3, 2, 1]
-}
-- The time complexity of this function is O(n). At each iteration of the
-- helper function iter, we are deconstructing the list passed in, this is a
-- constant time operation in haskell which is performed n times (once for each
-- element in the list. Thus, we can reach the conclusion that the asymptotic
-- time complexity of this function is O(n)

-- C.4
{-
    reverse [1, 2, 3]
    (reverse [2, 3]) ++ [1]
    ((reverse [3]) ++ [2]) ++ [1]
    (((reverse []) ++ 3) ++ [2]) ++ [1]
    (([] ++ [3]) ++ [2]) ++ [1]

    The mistake in Ben's argument is that he assumes that appending a singleton
    list to another list is an O(1) operation. However, in the case of reverse,
    each element of the input list is being appended to the end of the result
    list one at a time. This means that the length of the result list grows 
    linearly with the length of the input list, so the total time complexity is
    O(n^2)


    ([3] ++ [2]) ++ [1]
    (3 : ([] ++ [2])) ++ [1]
    (3 : [2]) ++ [1]
    [3, 2] ++ [1]
    3 : ([2] ++ [1])
    3 : (2 : ([] ++ [1]))
    3 : (2 : [1])
    3 : [2, 1]
    [3, 2, 1]
    
-}

-- C.5
{-
    head (isert [3, 1, 2, 5, 4])
    head (insert 3 (isort [1, 2, 5, 4]))
    head (insert 3 (insert 1 (isort [2, 5, 4])))
    head (insert 3 (insert 1 (insert 2 (isort [5, 4]))))
    head (insert 3 (insert 1 (insert 2 (insert 5 (isort [4])))))
    head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 (isort [])))))
    head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 [])))))
    head (insert 3 (insert 1 (insert 2 (insert 5 [4]))))
    head (insert 3 (insert 1 (insert 2 (4 : (insert 5 [])))))
    head (insert 3 (insert 1 (insert 2 : (4 : (insert 5 [])))))
    head (insert 3 (insert 1 (2 : (4 : (insert 5 [])))))
    head (insert 3 (1 : (2 : (4 : (insert 5 [])))))
    head (1 : (insert 3 : (2 : (4 : (insert 5 [])))))
    1
-}

-- C.6
{-
    foldr max 0 [1, 5, 3, -2, 4]
    max 1 (foldr max 0 [5, 3, -2, 4])
    max 1 (max 5 (foldr max 0 [3, -2, 4]))
    max 1 (max 5 (max 3 (foldr max 0 [-2, 4])))
    max 1 (max 5 (max 3 (max -2 (foldr max 0 [4]))))
    max 1 (max 5 (max 3 (max -2 (max 4 (foldr max 0 [])))))
    max 1 (max 5 (max 3 (max -2 (max 4 0))))
    max 1 (max 5 (max 3 (max -2 4)))
    max 1 (max 5 (max 3 4))
    max 1 (max 5 4)
    max 1 5
    5
-}

{-
    foldl max 0 [1, 5, 3, -2, 4]
    foldl max (max 0 1) [5, 3, -2, 4]
    foldl max (max (max 0 1) 5) [3, -2, 4]
    foldl max (max (max (max 0 1) 5) 3) [-2, 4]
    foldl max (max (max (max (max 0 1) 5) 3) -2) [4]
    foldl max (max (max (max (max (max 0 1) 5) 3) -2) 4) []
    max (max (max (max (max 0 1) 5) 3) -2) 4
    max (max (max 5 3) -2) 4
    max (max 5 -2) 4
    max 5 4
    5
-}

-- The space complexity of foldr is O(n) because we have to store all of the
-- elements of the list. And it follows the same logic for foldl. This is
-- possible because of lazy evaluation. We can see that these have the same
-- space complexity from our evaluations. 