{-# LANGUAGE TupleSections #-}

module MyLib (optimalSetOfLength, isSpecialSumSet, sumString) where

import Data.Function (on)
import Data.List
  ( elemIndex,
    find,
    minimumBy,
    sort,
    subsequences,
  )
import Data.List.Ordered (isSortedBy)
import Data.Maybe (catMaybes)
import Data.Tree (flatten, unfoldTree)
import GHC.Float (double2Int)
import Numeric.LinearProgramming
  ( Bound ((:>=:)),
    Constraints (General),
    Optimization (Minimize),
    Solution (Feasible, Optimal),
    exact,
  )

-- | Build string encoding of sum string.
--
-- >>> sumString [1,2,3]
-- "123"
--
sumString ::
  -- | sum string to encode.
  [Int] ->
  String
sumString = foldr (\int str -> show int <> str) "" 

-- | Compute the integer set which minimizes the set sum,
-- subject to the constraints on a special subset set.
optimalSetOfLength ::
  -- | The cardinality of the desired set.
  Int ->
  Maybe [Int]
optimalSetOfLength cardinality
  | cardinality < 0 = Nothing
  | cardinality == 1 = Just [1]
  | cardinality == 2 = Just [1, 2]
  | otherwise =
    ( solutionToSet
        . minimumBy solutionCompare
        . catMaybes
        . flatten
        . unfoldTree (simplexSolutionUnfolder cardinality)
    )
      (orderBounds cardinality <> borderBounds cardinality)

-- | Bounds to ensure that n1 < n2 < ... < nn given cardinality
orderBounds ::
  -- | Cardinality
  Int ->
  [[(Double, Int)]]
orderBounds cardinality
  | cardinality < 1 = error "This shouldn't have happened"
  | cardinality == 1 = []
  | otherwise =
    (\c -> [(1, c), (negate 1, pred c)]) <$> [2 .. cardinality]

-- | Comparison of solutions.
solutionCompare ::
  -- | x
  Solution ->
  -- | y
  Solution ->
  Ordering
solutionCompare (Optimal o) (Optimal o') = compare o o'
solutionCompare (Optimal o) (Feasible f) = compare o f
solutionCompare (Feasible f) (Optimal o) = compare f o
solutionCompare (Feasible f) (Feasible f') = compare f f'
solutionCompare (Optimal _) _ = LT
solutionCompare _ (Optimal _) = GT
solutionCompare (Feasible _) _ = LT
solutionCompare _ (Feasible _) = GT
solutionCompare _ _ = EQ

-- | Build the tree of simplex solutions.
simplexSolutionUnfolder ::
  -- | Cardinality
  Int ->
  -- | Sparse matrix encoding the constraints for the simplex
  -- solver.
  [[(Double, Int)]] ->
  (Maybe Solution, [[[(Double, Int)]]])
simplexSolutionUnfolder cardinality constraints =
  case nextConstraintPair sol of
    [[]] ->
      ( case sol of
          Feasible _ -> Just sol
          Optimal _ -> Just sol
          _ -> Nothing,
        []
      )
    cons -> (Nothing, (: constraints) <$> cons)
  where
    sol :: Solution
    sol =
      exact
        (Minimize $ replicate cardinality 1)
        (General ((:>=: 1) <$> constraints))
        [1 :>=: 1]

-- | Is xs a special sum set?
isSpecialSumSet ::
  -- | xs:  Set to test
  [Int] ->
  Bool
isSpecialSumSet =
  isSortedBy ((<) `on` snd)
    . sort
    . ((\ys -> (length ys, sum ys)) <$>)
    . subsequences

-- | Next pair of constraints to force special set conditions.
nextConstraintPair ::
  -- | Solution for current set of constraints.
  Solution ->
  [[(Double, Int)]]
nextConstraintPair (Optimal (_, set)) =
  nextConstraintPairForSet set
nextConstraintPair (Feasible (_, set)) =
  nextConstraintPairForSet set
nextConstraintPair _ = []

-- | Compute next pair of constraints to drive solution to
-- special set.
nextConstraintPairForSet ::
  -- | Current solution set members
  [Double] ->
  [[(Double, Int)]]
nextConstraintPairForSet solution =
  case pairPairs of
    Nothing -> [[]]
    Just ((_, xs), (_, ys)) ->
      [ ((1,) <$> indices xs) <> ((negate 1,) <$> indices ys),
        ((negate 1,) <$> indices xs) <> ((1,) <$> indices ys)
      ]
  where
    pairs :: [(Double, [Double])]
    pairs =
      sort $ (\xs -> (sum xs, xs)) <$> subsequences solution
    pairPairs :: Maybe ((Double, [Double]), (Double, [Double]))
    pairPairs =
      find
        (\((s1, _), (s2, _)) -> s1 == s2)
        (zip pairs (tail pairs))
    indices :: [Double] -> [Int]
    indices xs =
      (catMaybes . (((1 +) <$>) . (`elemIndex` solution) <$>)) xs

-- | Unpack solution if it is optimal or feasible.
solutionToSet ::
  -- | Solution to unpack.
  Solution ->
  Maybe [Int]
solutionToSet (Optimal (_, xs)) = Just $ double2Int <$> xs
solutionToSet (Feasible (_, xs)) = Just $ double2Int <$> xs
solutionToSet _ = Nothing

-- | Build the constraints requiring subsets of greater
-- cardinality to have greater sums than those of lesser
-- cardinality.
borderBounds ::
  -- | Cardinality of total set
  Int ->
  [[(Double, Int)]]
borderBounds cardinality =
  borderBoundForLesserLength indices <$> [1 .. pred cardinality]
  where
    --  | List of (length, subset index) pairs
    indices :: [(Int, [Int])]
    indices =
      ( ((\xs -> (length xs, xs)) <$>)
          . subsequences
      )
        [1 .. cardinality]

-- | Return a constraint to ensure that the maximum sum among
-- sets of a given cardinality is less than the minimum sum of
-- the next higher cardinality.
borderBoundForLesserLength ::
  --  | List of (length, subset index) pairs
  [(Int, [Int])] ->
  -- | Cardinality for the lesser subset.
  Int ->
  [(Double, Int)]
borderBoundForLesserLength indices lesserCardinality =
  constraint
    ( minimum $
        filter
          (\(len, _) -> len == succ lesserCardinality)
          indices
    )
    ( maximum $
        filter
          (\(len, _) -> len == lesserCardinality)
          indices
    )

-- | form constraint coefficient set for greater and
-- lesser (length, index subset)
constraint ::
  -- | Greater subset characterization.
  (Int, [Int]) ->
  -- | Lesser subset characterization.
  (Int, [Int]) ->
  [(Double, Int)]
constraint (_, xs) (_, ys) =
  ((1,) <$> xs) <> ((negate 1,) <$> ys)
