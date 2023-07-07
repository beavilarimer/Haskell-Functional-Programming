module Lab5 where 

import Control.Monad 

-- A.1
-- hr_solutions :: [((Integer, Integer), (Integer, Integer), Integer)]
-- hr_solutions =
--   [((i, l), (j, k), i^3 + l^3) |
--    i <- [1..],
--    j <- [1..i-1],
--    k <- [1..j-1],
--    l <- [1..k-1],
--    i^3 + l^3 == j^3 + k^3]

hr_solutions :: [((Integer, Integer), (Integer, Integer), Integer)]
hr_solutions = do
  i <- [1..]
  j <- [1..i-1]
  k <- [1..j-1]
  l <- [1..k-1]
  let sum1 = i^3 + l^3
      sum2 = j^3 + k^3
  guard (sum1 == sum2)
  return ((i, l), (j, k), sum1)

-- A.2
-- Write an expression which computes the sum of the natural numbers below one
-- thousand which are multiples of 3 or 5.
blastFromPast1 :: Integer
blastFromPast1 = sum (do
    i <- [1..999]
    let threeOrFive = mod i 3 == 0 || mod i 5 == 0
    guard threeOrFive
    return i)

blastFromPast2 :: Integer
blastFromPast2 = sum (do
    i <- [1..999]
    if mod i 3 == 0 || mod i 5 == 0
      then return i
      else mzero)

-- A.3
-- A palindromic number reads the same both ways. The largest palindrome made
-- from the product of two 2-digit numbers is 9009 = 91 * 99. Find the largest
-- palindrome made from the product of two 3-digit numbers.
isPalindrome :: Integer -> Bool
isPalindrome i = show i == reverse (show i)

largestPalindrome :: Integer
largestPalindrome = maximum (do
    i <- [100..999]
    let product = i * (i+1)
    guard (isPalindrome product)
    return product)

-- 906609

-- A.4
type Expr = [Item]

data Item = N Int | O Op
  deriving Show

data Op = Add | Sub | Cat
  deriving Show

ops :: [Item]
ops = [O Add, O Sub, O Cat]

-- Define a value called exprs which consists of a list of all possible valid 
-- expressions from the puzzle description i.e. all possible combinations of 
-- the digits 1 to 9 (in order) with one of the operators from the Op datatype
-- between each digit.

-- A.4.a
exprs :: [Expr]
exprs = do
  var1 <- ops
  var2 <- ops
  var3 <- ops
  var4 <- ops
  var5 <- ops
  var6 <- ops
  var7 <- ops
  var8 <- ops
  return [N 1, var1, N 2, var2, N 3, var3, N 4, var4, N 5, var5, N 6, var6, N 7, var7, N 8, var8, N 9]

-- A.4.b
-- N i, Cat, N j --> N (ij)
normalize :: Expr -> Expr
normalize (N i : O Cat : N j : xs) = normalize (N (i * 10 + j) : xs)
normalize [N i] = [N i]
normalize _ = 
  error "normalize: Wrong input, we expect the format (N i : O Cat : N j)."

-- A.4.c
evaluate :: Expr -> Int
evaluate [N x] = x
evaluate (N i : O Add : N j : xs) = evaluate (N (i + j): xs)
evaluate (N i : O Sub : N j : xs) = evaluate (N (i - j): xs)
evaluate _ = error "evaluate: Expression contains cat operator"


-- Testing Functions
-- Pick out the expressions that evaluate to a particular number.
find :: Int -> [Expr] -> [Expr]
find n = filter (\e -> evaluate (normalize e) == n)

-- Pretty-print an expression.
pprint :: Expr -> String
pprint [N i] = show i
pprint (N i : O Add : es) = show i ++ " + " ++ pprint es
pprint (N i : O Sub : es) = show i ++ " - " ++ pprint es
pprint (N i : O Cat : es) = show i ++ pprint es
pprint _ = error "pprint: invalid argument"

-- Run the computation and print out the answers.
run :: IO ()
run = mapM_ putStrLn $ map pprint $ Lab5.find 100 exprs

-- B.1
-- do n1 <- [1..6]
--    n2 <- [1..6]
--    []
--    return (n1, n2)

-- Let us first replace the empty list with return and use the >>= notation
-- [1..6] >>= \n1 -> ([1..6] >>= \n2 -> ([] >> return (n1, n2)))
-- [1..6] >>= \n1 -> ([1..6] >>= \n2 -> ([] >>= (\_ -> return (n1, n2))))
-- [1..6] >>= \n1 -> ([1..6] >>= \n2 -> ([] >>= (\_ -> [(n1, n2)])))
-- [1..6] >>= \n1 -> ([1..6] >>= \n2 -> (concatMap (\_ -> [(n1, n2)]) []))
-- [1..6] >>= \n1 -> (concatMap ( \n2 -> (concatMap (\_ -> [(n1, n2)]) [])) [1..6])
-- concatMap (\n1 -> (concatMap ( \n2 -> concatMap (\_ -> [(n1, n2)]) []) [1..6])) [1..6]
-- concatMap (\n1 -> concatMap (\n2 -> []) [1..6]) [1..6]
-- concatMap (\n1 -> []) [1..6]
-- []

-- B.2
-- Reducing expression 1:
-- do n1 <- [1..6]
--    n2 <- [1..6]
--    return <anything>
--    return (n1, n2)

-- [1..6] >>= \n1 -> ([1..6] >>= \n2 -> (return <anything> -> return (n1, n2)))
-- [1..6] >>= \n1 -> ([1..6] >>= \n2 -> ([<anything>] -> [(n1, n2)]))
-- concatMap (\n1 -> ([1..6] >>= \n2 -> ([<anything>] -> [(n1, n2)]))) [1..6]
-- concatMap (\n1 -> concatMap (\n2 -> ([<anything>] -> [(n1, n2)])) [1..6]) [1..6]
-- concatMap (\n1 -> concatMap (\n2 -> ([<anything>] -> [(n1, n2)])) [1..6]) [1..6]
-- concatMap (\n1 -> concatMap (\n2 -> [(n1, n2)]) [1..6]) [1..6] 
-- concatMap (\n1 -> [(n1,1),...,(n1,6)]) [1..6] 
-- [(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),
-- (3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),
-- (5,1),(5,2),(5,3),(5,4),(5,5),(5,6),(6,1),(6,2),(6,3),(6,4),(6,5),(6,6)]

-- Reducing expression 2:
-- do n1 <- [1..6]
--    n2 <- [1..6]
--    return (n1, n2)

-- [1..6] >>= \n1 -> ([1..6] >>= \n2 -> return (n1, n2))
-- [1..6] >>= \n1 -> ([1..6] >>= \n2 -> [(n1, n2)])
-- [1..6] >>= \n1 -> concatMap (\n2 -> [(n1, n2)]) [1..6]
-- concatMap (\n1 -> concatMap (\n2 -> [(n1, n2)]) [1..6]) [1..6] 
-- concatMap (\n1 -> [(n1,1),...,(n1,6)]) [1..6] 
-- [(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),
-- (3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),
-- (5,1),(5,2),(5,3),(5,4),(5,5),(5,6),(6,1),(6,2),(6,3),(6,4),(6,5),(6,6)]

-- B.3
-- let s = ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] in
--   do ['a', 'a', c1, c2, 'b', 'b'] <- s
--      return [c1, c2]

-- s >>=
--  \y -> case y of
--    ['a', 'a', c1, c2, 'b', 'b'] -> return [c1, c2]
--    _ -> fail "Pattern match failure in do block"
-- (>>=) :: [a] -> (a -> [b]) -> [b]

-- concatMap
--  (\y -> case y of
--    ['a', 'a', c1, c2, 'b', 'b'] -> return [c1, c2]
--    _ -> fail "Pattern match failure in do block")

-- concat (map (\y -> case y of
--    ['a', 'a', c1, c2, 'b', 'b'] -> [[c1, c2]]
--    _ -> fail "Pattern match failure in do block") s)

-- concat (map (\y -> case y of
--    ['a', 'a', c1, c2, 'b', 'b'] -> [[c1, c2]]
--    _ -> fail "Pattern match failure in do block") 
--    ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"])

-- concat (['x', 'y'] ['z' 'w'] [] ['c', 'c'] [])
-- ["xy", "zw", "cc"]

-- If instead fail was
-- fail s = error s
-- once we reach a fail case the function would stop and we would not get the
-- desired output. For example, consider the given example list s, once we
-- reach "foobar", an error would arise and we wouldn't be able to return a
-- list of strings. Instead because the implementation is fail _ = [] we can
-- simply concatenate the empty list. 

-- B.4
-- Tips:
-- Show that given m = [x1, x2, ...] both expressions evaluate to the same
-- thing. Also show this for m = []. Expand (++) . k into an explicit
-- lambda expression.

-- Base cases:
-- foldr ((++) . k) [] m
-- foldr (\y -> output -> ((++) . k) y output)) [] []
-- foldr (\y -> output -> ((++) (k y)) output)) [] []
-- foldr (\y -> output -> (++) (k y) output) [] []
-- foldr (\y -> output -> (k y) ++ output) [] []
-- []

-- concat (map k m)
-- concat []
-- []


-- In the case that m = [x1, x2, ...]
-- foldr ((++) . k) [] m
-- foldr (\y -> output -> ((++) . k) y output)) [] [x1, x2, ...]
-- foldr (\y -> output -> ((++) (k y)) output)) [] [x1, x2, ...]
-- foldr (\y -> output -> (++) (k y) output) [] [x1, x2, ...]
-- foldr (\y -> output -> (k y) (++) output) [] [x1, x2, ...]
-- (k x1) ++ (k x2) ++ ... ++ (k xn) ++ []

-- Note that we are under the assumption that (k x1) returns a list
-- concat (map k m)
-- concat [k x1, k x2 ... k xn]
-- (k x1) ++ (k x2) ++ ... ++ (k xn)
