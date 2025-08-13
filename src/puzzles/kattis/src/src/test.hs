-- TODO Refactor propUnion test, you probably need to break it down to smaller
-- tests, maybe keeping similar setup

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Perlude

import           Control.Monad.ST   (ST, runST)
import           Data.Array.Base    (UArray)
import           Data.Array.ST      (STUArray, freeze, runSTUArray)
import           Data.Foldable      (traverse_)
import           Data.Functor       (void)
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.Tree          (Forest, Tree)
import           Hedgehog
import qualified Hedgehog.Gen       as Gen
import qualified Hedgehog.Range     as Range
import           Internal.UnionFind

main :: IO ()
main =
  void tests

propUnion :: Property
propUnion =
  property
  $ do
  subsets <- forAll genSubsets
  unions <- forAll (traverse genUnions subsets)
  let size = sum $ Set.size <$> subsets
      (reps, array) = runST
        $ do
        uf <- mkUnionFind size (concat unions)
        reps' <- toReps uf subsets
        fz  <- toArray uf
        return (reps', fz)
      classSizes = Set.size <$> reps
  annotateShow array
  annotateShow reps
  annotate "Each subset must have the a single representation"
  classSizes === replicate (length subsets) 1
  annotate "Each subset must have a different representation"
  length subsets === Set.size (Set.unions reps)

-- mkUnionFind :: Int -> [(Int, Int)] -> ST s (STUArray s Int Int)
mkUnionFind :: Int -> [(Int, Int)] -> ST s (MutableUnionFind s)
mkUnionFind size unions =
  do
    uf <- new size
    traverse_ (uncurry $ union uf) unions
    return uf

-- Convert all elements of the sets into their rep. The expectation is that each
-- sets converts to a single rep and the rep is different for each set
toReps :: MutableUnionFind s -> [Set Int] -> ST s [Set Int]
toReps uf subsets =
  let
    toRep s = Set.fromList <$> traverse (find uf) (Set.toList s)
  in
    traverse toRep subsets


validateSameRep :: MutableUnionFind s -> Set Int -> ST s Bool
validateSameRep uf elements =
  do
    reps <- traverse (find uf) $ Set.toList elements
    return $ allEqual reps

allEqual :: Eq a => [a] -> Bool
allEqual []    = True
allEqual (h:t) = all (h ==) t

tests :: IO Bool
tests =
  checkParallel $ Group "Test.UnionFind" [
      ("propUnion", propUnion)
    ]

-- Generate a list of shuffled 0 to N numbers
genElements :: Gen [Int]
genElements =
  do
    len <- Gen.int $ Range.linear 1 1000
    Gen.shuffle $ take len [0..]

genPartitions :: [Int] -> Gen [[Int]]
genPartitions [] = pure []
genPartitions elements =
  do
    n <- Gen.int $ Range.constant 1 (length elements)
    let (subset, remainder) = splitAt n elements
    (subset :) <$> genPartitions remainder


-- Given a set of free nodes, generates a list of pairs that constructs an
-- arbitrary tree that links all the elements of the set
genUnions :: Set Int -> Gen [(Int, Int)]
genUnions set =
  if Set.null set
  then return []
  else do
    x <- Gen.element set
    genUnions' (Set.delete x set) (Set.singleton x)

genUnions' :: Set Int -> Set Int -> Gen [(Int, Int)]
genUnions' free bound =
  if null free
  then return []
  else
    do
      x <- Gen.element free
      y <- Gen.element bound
      ((x, y):) <$> genUnions' (Set.delete x free) (Set.insert x bound)

genSubsets :: Gen [Set Int]
genSubsets = fmap Set.fromList <$> (genElements >>= genPartitions)
