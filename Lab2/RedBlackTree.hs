module RedBlackTree where

-- Color of the node
data Color = Red | Black
    deriving Show

-- A Red-black tree is either a leaf or a tree node with a color,
-- two branches, both of which are trees and a value of type a
data Tree a = Leaf | Node Color (Tree a) a (Tree a)
    deriving Show

-- A.1
member :: Ord a => a -> Tree a -> Bool
member _ Leaf = False
member x (Node _ left  y right)
  | x == y    = True
  | x < y     = member x left
  | otherwise = member x right


-- A.2
toList :: Tree a -> [a]
toList Leaf = []
toList (Node _ left y right) = toList left ++ [y] ++ toList right


-- A.3 
insert :: Ord a => a -> Tree a -> Tree a
insert elem t = makeBlack (ins elem t)
  where
    -- Insert an element into a tree
    ins :: Ord a => a -> Tree a -> Tree a
    ins elem Leaf = Node Red Leaf elem Leaf --new nodes are colored red
    ins elem t@(Node color left elem' right)
      | elem < elem' = balance color (ins elem left) elem' right
      | elem > elem' = balance color left elem' (ins elem right)
      | otherwise = t --in the case that the element is alredy in t

     -- Make the root of the tree black.
    makeBlack :: Tree a -> Tree a
    makeBlack Leaf = Leaf
    makeBlack (Node _ left elem right) = Node Black left elem right

    -- Balance a red-black tree under construction which may not satisfy
    -- the red and black invariants.balance :: Ord a => Color -> Tree a -> a -> Tree a -> Tree a
    balance Black (Node Red (Node Red a x b) y c) z d =
      Node Red (Node Black a x b) y (Node Black c z d)
    balance Black (Node Red a x (Node Red b y c)) z d =
      Node Red (Node Black a x b) y (Node Black c z d)
    balance Black a x (Node Red (Node Red b y c) z d) =
      Node Red (Node Black a x b) y (Node Black c z d)
    balance Black a x (Node Red b y (Node Red c z d)) =
      Node Red (Node Black a x b) y (Node Black c z d)
    balance color left elem right = Node color left elem right

-- A.5
fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Leaf

minDepth :: Tree a -> Int
minDepth Leaf = 0
minDepth (Node _ left _ right) = 1 + min (minDepth left) (minDepth right)

maxDepth :: Tree a -> Int
maxDepth Leaf = 0
maxDepth (Node _ left _ right) = 1 + max (maxDepth left) (maxDepth right)

-- A.6    
testInvariant1 :: Ord a => Tree a -> Bool
testInvariant1 Leaf = True
testInvariant1 (Node _ l x r) = 
  helper l False True x x && helper r True False x x
  where
  helper :: Ord a => Tree a -> Bool -> Bool -> a -> a -> Bool
  helper Leaf _ _ _ _ = True
  helper (Node _ l' y r') seenMin seenMax minVal maxVal
    | (y >= maxVal  && seenMax) ||  (y <= minVal  && seenMin) = False
    | otherwise = helper l' seenMin True minVal y && helper r' True seenMax y maxVal
{-
  [A]
  / \
[B] [C]       min:A max: Notseen A
    /  \                 
  [F]  [D]

-}    


-- A.7
testInvariant2 :: Tree a -> Bool
testInvariant2 Leaf = True
testInvariant2 (Node c left _ right) = helper c left && helper c right
  where 
    helper :: Color -> Tree a -> Bool
    helper _ Leaf = True
    helper _ (Node Black l _ r) = helper Black l && helper Black r
    helper Black (Node Red l _ r) = helper Red l && helper Red r
    helper Red (Node Red _ _ _ ) = False


-- A.8
testInvariant3 :: Tree a -> Bool
testInvariant3 t = allEqual (leafCounts t 0)
  where
    -- Given a tree, return a list of the count of black nodes on every path
    -- from the root of the tree to a leaf.
    leafCounts :: Tree a -> Int -> [Int]
    leafCounts Leaf n = [n]
    leafCounts (Node Black left _ right) n = leafCounts left (n + 1) ++ leafCounts right (n + 1)
    leafCounts (Node Red left _ right) n = leafCounts left n ++ leafCounts right n

    -- Return True if all the elements of a list are equal.
    allEqual :: Ord a => [a] -> Bool
    allEqual [] = True
    allEqual [_] = True
    allEqual (x:r@(y:_)) | x == y = allEqual r
                         | otherwise = False

-- B.1
-- We define Set as a type synonym for Tree.
type Set a = Tree a

-- Empty set.
empty :: Set a
empty = Leaf

-- Convert a list to a set.
toSet :: Ord a => [a] -> Set a
toSet = fromList

isSubset :: Ord a => Set a -> Set a -> Bool
isSubset a b = all (`member` b) (toList a)

-- B.2
eqSet :: Ord a => Set a -> Set a -> Bool
eqSet a b = isSubset a b && isSubset b a

-- B.3
union :: Ord a => Set a -> Set a -> Set a
union a b = foldr insert b (toList a)

-- B.4
intersection :: Ord a => Set a -> Set a -> Set a
intersection s1 s2 = foldr (\x r -> if member x s1 then insert x r else r) empty (toList s2)

-- B.5
difference :: Ord a => Set a -> Set a -> Set a
difference s1 s2 = foldr (\x r -> if member x s2 then r else insert x r) empty (toList s1)
