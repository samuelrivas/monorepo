{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

module Advent.Day18 where

import           Perlude

import           Advent.Templib                  (linesOf)

import           Control.Applicative             ((<|>))
import           Control.Lens                    (Field1, Field2, Getter, Lens',
                                                  Prism', _1, _2, _Just, at,
                                                  both, non, over, preview,
                                                  prism, set, singular, sumOf,
                                                  to, view, views)
import           Control.Monad                   (MonadPlus, replicateM,
                                                  replicateM_, (>=>))
import           Control.Monad.MonadSearch.Astar (AstarConfig, mkConfig,
                                                  searchAstarT)
import           Control.Monad.Reader            (MonadReader, ReaderT, asks,
                                                  runReaderT)
import           Control.Monad.State             (MonadState, State, gets,
                                                  modify, runState)
import           Data.Advent                     (Day (..))
import           Data.Bidim                      (Bidim, Coord, boundaries,
                                                  cross)
import           Data.Char                       (digitToInt)
import           Data.Functor.Identity           (Identity, runIdentity)
import           Data.Generics.Labels            ()
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as HashMap
import           Data.HashSet                    (HashSet)
import qualified Data.HashSet                    as HashSet
import           Data.Hashable                   (hash)
import           Data.List                       (find, foldl1', sortOn)
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NonEmpty
import           Data.Maybe                      (catMaybes, fromJust, isJust,
                                                  listToMaybe, mapMaybe)
import           Data.MultiSet                   (MultiSet)
import qualified Data.MultiSet                   as MultiSet
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Data.Text                       (intercalate)
import qualified Data.Text                       as Text
import qualified Prelude                         as Prelude
import           System.IO.Advent                (getInput, solve)
import           Text.Parsec                     (anyChar, char, noneOf)
import           Text.Parsec.Bidim               (bidim)
import           Text.Parsec.Parselib            (Parser, digitAsNum, literal,
                                                  text1, unsafeParseAll)

type Parsed = [Tree Int]

day :: Day
day = D18

rawInput :: IO Text
rawInput = getInput day

example :: Text
example =
  intercalate
  "\n" [
  "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
  "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]",
  "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]",
  "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]",
  "[7,[5,[[3,8],[1,4]]]]",
  "[[2,[2,2]],[8,[8,1]]]",
  "[2,9]",
  "[1,[[[9,3],9],[[9,0],[0,7]]]]",
  "[[[5,[7,4]],7],1]",
  "[[[[4,2],2],6],[8,7]]"
  ]

-- TODO This tree may be good for a library. We are not using the optics here,
-- but since I wrote them when trying an alternative apprach I'll let them stay
-- in case I end up collecting all this to a library
data Tree a = Node (Tree a) (Tree a) | Leaf a
  deriving Eq

-- TODO I am not sure if this is a good idea. It is not dual to treeP, but on
-- the other hand is would be confusing otherwise as it would render the same as
-- lists
instance Show a => Show (Tree a) where
  show = unpack . ("Tree " <>) . showTree

instance Functor Tree where
  fmap f (Leaf a)   = Leaf (f a)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

instance Foldable Tree where
  foldMap f (Leaf a)   = f a
  foldMap f (Node l r) = foldMap f l <> foldMap f r

instance Traversable Tree where
  traverse f (Leaf a)   = Leaf <$> f a
  traverse f (Node l r) = Node <$> traverse f l <*> traverse f r

node :: Prism' (Tree a) (Tree a, Tree a)
node =
  let
    mkNode (l, r) = Node l r
  in
    prism mkNode $ \case
    (Node l r) -> Right (l, r)
    t          -> Left t

leaf :: Prism' (Tree a) a
leaf =
    prism Leaf $ \case
    (Leaf a) -> Right a
    t        -> Left t

parsedExample :: Parsed
parsedExample = fromJust $ unsafeParseAll parser example

exampleTree :: Tree Int
exampleTree =
  fromJust . unsafeParseAll treeP $ "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"

parser :: Parser Parsed
parser = linesOf treeP

treeP :: Parser (Tree Int)
treeP = (Leaf <$> digitAsNum) <|> nodeP

nodeP :: Parser (Tree Int)
nodeP =
  Node
  <$> (char '[' *> treeP <* char ',')
  <*> treeP <* char ']'

showTree :: Show a => Tree a -> Text
showTree (Leaf a)   = show a
showTree (Node a b) = "[" <> showTree a <> "," <> showTree b <> "]"

tryExplode :: Tree Int -> Maybe (Tree Int)
tryExplode = preview (_Just . _2) .  tryExplode' 4

-- TODO Use alternative to make this clearer Returns Nothing for subtrees that
-- don't explode, otherwise returns the exploded subtree and the carry to sum to
-- the left and right numbers
tryExplode' :: Int -> Tree Int -> Maybe (Int, Tree Int, Int)
tryExplode' 0 (Node (Leaf a) (Leaf b)) = Just (a, Leaf 0, b)
tryExplode' n (Node l r) =
  case tryExplode' (n - 1) l of
    Just (carryLeft, t, carryRight) ->
      Just (carryLeft, Node t (sumLeftmost carryRight r), 0)
    Nothing ->
      case tryExplode' (n - 1) r of
        Just (carryLeft, t, carryRight) ->
          Just (0, Node (sumRightmost carryLeft l) t, carryRight)
        Nothing -> Nothing
tryExplode' _ _ = Nothing

trySplit :: Tree Int -> Maybe (Tree Int)
trySplit (Leaf n)
  | n >= 10 = let d = n `div` 2 in Just $ Node (Leaf d) (Leaf (n - d))
  | otherwise = Nothing
trySplit (Node l r) =
  ((`Node` r) <$> trySplit l) <|> ((l `Node`) <$> trySplit r)

sumLeftmost :: Int -> Tree Int -> Tree Int
sumLeftmost n (Leaf x)   = Leaf (x + n)
sumLeftmost n (Node x y) = Node (sumLeftmost n x) y

sumRightmost :: Int -> Tree Int -> Tree Int
sumRightmost n (Leaf x)   = Leaf (x + n)
sumRightmost n (Node x y) = Node x (sumRightmost n y)

reduceStep :: Tree Int -> Maybe (Tree Int)
reduceStep t = tryExplode t <|> trySplit t

reduce :: Tree Int -> Tree Int
reduce t = maybe t reduce (reduceStep t)

sumTree :: Tree Int -> Tree Int -> Tree Int
sumTree a b = reduce $ Node a b

magnitude :: Tree Int -> Int
magnitude (Leaf n)   = n
magnitude (Node l r) = 3 * magnitude l + 2 * magnitude r

leftmost :: Tree a -> a
leftmost (Leaf a)   = a
leftmost (Node l _) = leftmost l

rightmost :: Tree a -> a
rightmost (Leaf a)   = a
rightmost (Node _ r) = rightmost r

solver1 :: Parsed -> Int
solver1 = magnitude . foldl1' sumTree

solver2 :: Parsed -> Int
solver2 input =
  maximum [magnitude . sumTree a $ b | a <- input, b <- input, not (a == b)]

main :: IO ()
main = solve day parser solver1 solver2
