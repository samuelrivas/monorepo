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
        reps <- toReps uf subsets
        fz :: UArray Int Int  <- freeze uf
        return (reps, fz)
      classSizes = Set.size <$> reps
  annotateShow array
  annotateShow reps
  classSizes === replicate (length subsets) 1

mkUnionFind :: Int -> [(Int, Int)] -> ST s (STUArray s Int Int)
mkUnionFind size unions =
  do
    array <- new size
    traverse_ (uncurry $ union array) unions
    return array

-- Convert all elements of the sets into their rep. The expectation is that each
-- sets converts to a single rep and the rep is different for each set
toReps :: STUArray s Int Int -> [Set Int] -> ST s [Set Int]
toReps array subsets =
  let
    toRep s = Set.fromList <$> traverse (find array) (Set.toList s)
  in
    traverse toRep subsets


validateSameRep :: STUArray s Int Int -> Set Int -> ST s Bool
validateSameRep array elements =
  do
    reps <- traverse (find array) $ Set.toList elements
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
