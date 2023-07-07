module Lab3ab where

-- A.1
data Nat1 = Zero1 | Succ1 Nat1

instance Eq Nat1 where
  (==) :: Nat1 -> Nat1 -> Bool
  Zero1 == Zero1 = True
  Succ1 m == Succ1 n = m == n
  _ == _ = False

instance Show Nat1 where
  show :: Nat1 -> String
  show Zero1 = "Zero1"
  show (Succ1 Zero1) = "Succ1 Zero1"
  show (Succ1 n) = "Succ1 (" ++ show n ++ ")"


-- A.2
data Nat2 = Zero2 | Succ2 Nat2 deriving (Eq, Show)

-- A.3
instance Ord Nat2 where
    (<=) :: Nat2 -> Nat2 -> Bool
    Zero2 <= _ = True
    (Succ2 _ ) <= Zero2 = False
    (Succ2 n) <= (Succ2 m)
        | n <= m = True
        | otherwise = False

{-
This will work because of the way that Nat2 is defined. Since it is a recursive
definition Ord will know that Zero2 is the smallest number and from that can 
automatically define and compare Zero2, Zero2 as EQ and Zero2 as LT Succ (Zero2)
from this the logic for other definitions follow.
-}

-- A.4
data Nat = Zero | Succ Nat
  deriving (Eq, Show, Ord)

data SignedNat =
  Neg Nat | Pos Nat
  deriving (Show)

instance Eq SignedNat where
  (==) :: SignedNat -> SignedNat -> Bool
  Pos n == Pos m = n == m
  Neg n == Neg m = n == m
  Neg Zero == Pos Zero = True
  Pos Zero == Neg Zero = True
  _ == _ = False
  
instance Ord SignedNat where
  (<=) :: SignedNat -> SignedNat -> Bool
  Neg n <= Neg m = m <= n
  Pos n <= Pos m = n <= m
  Pos n <= Neg m = n == m && n == Zero
  Neg _ <= Pos _ = True 

{- It would not work because the logic for the Negative intergers is flipped
and the automatically derived definitions for Eq and Ord would not make the
right sign corrections because there is no instructions (code) for the compiler
to make these corrections.
-}

-- A.5
addNat :: Nat -> Nat -> Nat
addNat Zero Zero = Zero
addNat Zero n = n
addNat n Zero = n
addNat (Succ n) m = Succ (addNat n m)

-- 2 = Succ Succ Zero
-- 1 = Succ Zero
-- addNat 2 1
-- addNat (Succ (Succ Zero)) (Succ Zero)

subNat :: Nat -> Nat -> Nat
subNat Zero Zero = Zero
subNat n Zero = n
subNat Zero _ = error "Not a valid operation for natural numbers"
subNat (Succ n) (Succ m) = subNat n m

mulNat :: Nat -> Nat -> Nat
mulNat _ Zero = Zero
mulNat Zero _ = Zero
mulNat n (Succ m) = addNat n (mulNat n m)

{-
2 * 3
2 + (2 * 2)
2 + 2 + (2 * 1)
2 + 2 + 2 + (2 * 0)
-}

addSignedNat :: SignedNat -> SignedNat -> SignedNat
addSignedNat (Pos n) (Pos m) = Pos (addNat n m)
addSignedNat (Neg n) (Neg m) = Neg (addNat n m)
addSignedNat (Neg n) (Pos m)
  | n >= m = Neg (subNat n m)
  | otherwise = Pos (subNat m n)
addSignedNat (Pos n) (Neg m)
  | n >= m = Pos (subNat n m)
  | otherwise = Neg (subNat m n)

subSignedNat :: SignedNat -> SignedNat -> SignedNat
subSignedNat (Pos n) (Pos m) = addSignedNat (Pos n) (Neg m)
subSignedNat (Neg n) (Neg m) = addSignedNat (Neg n) (Pos m)
subSignedNat (Neg n) (Pos m) = addSignedNat (Neg n) (Neg m)
subSignedNat (Pos n) (Neg m) = addSignedNat (Pos n) (Pos m)

mulSignedNat :: SignedNat -> SignedNat -> SignedNat
mulSignedNat (Pos n) (Neg m) = Neg (mulNat n m)
mulSignedNat (Neg n) (Pos m) = Neg (mulNat n m)
mulSignedNat (Pos n) (Pos m) = Pos (mulNat n m)
mulSignedNat (Neg n) (Neg m) = Pos (mulNat n m)

fromIntegerToNat :: Integer -> Nat
fromIntegerToNat 0 = Zero
fromIntegerToNat n = Succ (fromIntegerToNat (n - 1))

instance Num SignedNat where
    (+) :: SignedNat -> SignedNat -> SignedNat
    (+) = addSignedNat
    
    (-) :: SignedNat -> SignedNat -> SignedNat
    (-) = subSignedNat

    (*) :: SignedNat -> SignedNat -> SignedNat
    (*) = mulSignedNat

    abs :: SignedNat -> SignedNat
    abs (Pos x) = Pos x
    abs (Neg x) = Pos x

    signum :: SignedNat -> SignedNat
    signum (Pos Zero) = Pos Zero
    signum (Neg Zero) = Neg Zero
    signum (Neg _) = Neg (Succ Zero) 
    signum (Pos _) = Pos (Succ Zero)

    fromInteger :: Integer -> SignedNat
    fromInteger 0 = Pos Zero
    fromInteger n
      | n <= 0 = Neg (fromIntegerToNat (-n))
      | otherwise = Pos (fromIntegerToNat n)

-- A.6
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

signedNatToInteger :: SignedNat -> Integer
signedNatToInteger (Pos n) = natToInteger n
signedNatToInteger (Neg n) = -1 * natToInteger n

-- A.7
data UnaryInt = UZero | PSucc UnaryInt | NSucc UnaryInt

{-
  In this datatype, UZero represents 0, PSucc n represents the positive integer 
  n+1, and NSucc n represents the negative integer -(n+1).

  The advantage of this representation is that we don't have the redundancy of
  the SignedNat Zero, where we could have Pos Zero and Neg Zero. In this data
  type we have different constructors for positive negative which build off of
  zero and we have Zero which has no sign association.

  There are some obvious drawbacks in this implementation. For one, as before,
  it is space-inefficient, especially for large integers. But, most significantly
  if its methods are not implemented correctly we could have a number be composed
  of both negative and positive Succ's 
-}

-- A.8
factorial :: (Ord a, Num a) => a -> a
factorial n 
  | n < 0 = error "Factorial is undefined for negative numbers"
  | n == 0 = 1
  | otherwise = n * factorial (n - 1)

-- Pos (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))

-- B.1
{-
 >#< :: Integer -> Integer -> String.
 infix. The operator >#< cannot be chained together since it returns a string,
 and chaining multiple >#< operators would result in a type error.

 +| :: Integer -> Integer -> Integer.
 infixl but it could also be infixr because no matter which way it
 associates the result is the same and it does not lead to any type 
 errors if we were to chain the operators. Furthemore, modulo addition
 order given that all of the types are integers
 7 +| (6 +| 5) = 8
 (7 +| 6) +| 5 = 8

 &< :: [Integer] -> Integer -> [Integer],
 infixl. It cannot be declared as infixr because it takes a list of integers as
 the first argument and an integer as the second argument. If we were to chain 
 additional operators if this were infixr then we would reach a type error 
 because the second argument cannot be a list. However if this operator were to
 be infixl then it could be chained with additional &< operators without 
 resulting in a type error.
 ([1, 2] &< 3) &< 4 -> [1, 2, 3] &< 4 -> [1, 2, 3, 4]
 [1, 2] &< (3 &< 4) -> type error, we are trying to append to a list but there
                       is no available list

 >&& :: Integer -> [Integer] -> [Integer].
 infixr. It cannot be declared as infixl because it takes an integer as the 
 first argument and a list of integers as the second argument. Thus, chaining
 additional >&& operators is not possible because the first argument cannot be
 a list. However as a infixr it can be chained with additional >&& operators
 withiout resulting in a type error. 
 1 >&& (2 >&& [3, 4]) -> 1 >&& [2, 2, 3, 4] -> [1, 1, 2, 2, 3, 4]
 (1 >&& 2) >&& [3, 4] -> type error
-}

-- B.2
{-
 The operation could be either infixr or infixl because allow the possibility
 of chain operators to type check in both directions. Consider the following

  2 +# (13 +# 100) = 1
  (100 +# 2) +# 13 = 2

  However as seen by the examples above this operator should be infix because
  if doesn't really make sense to be able to chain an operator to count the
  number of ints of the result of a sum. 
-}
