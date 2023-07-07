module SparseMatrix where

import qualified Data.Map as M
import qualified Data.Set as S


data SparseMatrix a =
  SM { bounds     :: (Integer, Integer),  -- number of rows, columns
       rowIndices :: S.Set Integer,       -- row indices with nonzeros
       colIndices :: S.Set Integer,       -- column indices with nonzeros
       vals       :: M.Map (Integer, Integer) a }  -- values
  deriving (Eq, Show)

-- C.1
sparseMatrix :: (Eq a, Num a) =>
  [((Integer, Integer), a)] -> (Integer, Integer) -> SparseMatrix a
  -- sparseMatrix <list of index/element pairs> <bounds> -> sparse matrix
sparseMatrix entries (rows, cols)
    | rows < 1 || cols < 1 = error "Invalid Bounds"
    | any (\((i, j), _) -> i > rows || i < 1 || j > cols || j < 1) entries = error "Index out of bounds"
    | otherwise = SM { bounds, rowIndices, colIndices, vals }
        where
            nonZeroPairs = filter (\((_, _), v) -> v /= 0) entries
            rowIndices = S.fromList (map (\((i, _), _) -> i) nonZeroPairs)
            colIndices = S.fromList (map (\((_, j), _) -> j) nonZeroPairs)
            bounds = (rows, cols)
            vals = M.fromList nonZeroPairs

-- C.2
addSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
addSM (SM (rowsA, colsA) _ _ valsA) 
  (SM (rowsB, colsB) _ _ valsB)
  | (rowsA, colsA) /= (rowsB, colsB) = error "Matrices don't have the same bounds"
  | otherwise = SM { bounds, rowIndices, colIndices, vals }
    where
      bounds = (rowsB, colsB)
      vals_with_zeros = M.unionWith (+) valsA valsB
      vals = M.fromList (filter (\((_, _), v) -> v /= 0) (M.toList vals_with_zeros))
      colIndices = S.fromList $ map snd (M.keys vals)
      rowIndices = S.fromList $ map fst (M.keys vals)


-- C.3
negateSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a
negateSM (SM bounds rowIndices colIndices vals) =
  SM bounds rowIndices colIndices (M.map negate vals)

-- C.4
subSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
subSM a b = addSM a (negateSM b)

-- C.5
-- Defing a helper functions to multiply the column and row vectors
mulRowCol :: forall a . (Eq a, Num a) => 
  Integer -> Integer -> M.Map (Integer, Integer) a ->
  M.Map (Integer, Integer) a -> ((Integer, Integer), a)
mulRowCol i j valsA valsB = ((p_row, p_col), value)
  where
    p_row = i
    p_col = j

    f :: (Eq a, Num a) => a -> (Integer, Integer) -> a -> a
    f acc (_, curr_col) v = acc + (v * M.findWithDefault 0 (curr_col, j) valsB)
    value = M.foldlWithKey f 0 (M.filterWithKey (\(i', _) _ -> i == i') valsA)

mulSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
mulSM (SM (rowsA, colsA) rowIndicesA _ valsA)
      (SM (rowsB, colsB) _ colIndicesB valsB)
  | colsA /= rowsB = error "Matrices are not compatible for multiplication"
  | otherwise = SM { bounds, rowIndices, colIndices, vals }
    where
      bounds = (rowsA, colsB)
      indices = [(i, j) | i <- S.toList rowIndicesA, j <- S.toList colIndicesB]
      vals = M.fromList $ filter (\(_, v) -> v /= 0) $ map (\(i, j) -> mulRowCol i j valsA valsB) indices
      rowIndices = S.fromList $ map fst (M.keys vals)
      colIndices = S.fromList $ map snd (M.keys vals)

-- C.6
getSM :: Num a => SparseMatrix a -> (Integer, Integer) -> a
getSM (SM (rowsA, colsA) rowIndicesA colIndicesA valsA) (row, col)
  | row > rowsA || row < 1 || col > colsA || col < 1 = 
    error "The row or column location is out of bounds."
  | S.member row rowIndicesA && S.member col colIndicesA = 
    M.findWithDefault 0 (row, col) valsA
  | otherwise = 0

rowsSM :: Num a => SparseMatrix a -> Integer
rowsSM (SM (rowsA, _) _ _ _) = rowsA

colsSM :: Num a => SparseMatrix a -> Integer
colsSM (SM (_, colsA) _ _ _) = colsA

-- C.7
(<|+|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|+|>) = addSM

(<|-|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|-|>) = subSM

(<|*|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|*|>) = mulSM

(<!>) :: Num a => SparseMatrix a -> (Integer, Integer) -> a
(<!>) = getSM

-- C.8
{-
Sparse matrix is a much more complex structure that has behaviors outside the
scope of Num. For example consider all of the properties needed for a matrix,
such as bounds, while this is not necessary for a Num instance.
-}
