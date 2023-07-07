module RedBlackTree where
import GHC.Cmm (ClosureTypeInfo(BlackHole))
import Data.Binary.Get (Decoder(Fail))

-- Color of the node
data Color = Red | Black
    deriving Show

-- A Red-black tree is either a leaf or a tree node with a color,
-- two branches, both of which are trees and a value of type a
data Tree a = Leaf | Node Color (Tree a) a (Tree a)
    deriving Show

member :: Ord a => a -> Tree a -> Bool
member _ Leaf = False
member x (Node left  y right)
  | x == y    = True
  | x < y     = member x left
  | otherwise = member x right

-- insert :: Ord a => a -> Tree a -> Tree a

-- toList :: Tree a -> [a]

-- fromList :: Ord a => [a] -> Tree a