{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Advent.Day18 where

import           Perlude

import           Advent.Templib                  (linesOf)

import           Control.Lens                    (Getter, Prism', _1, _2, _Just,
                                                  at, both, non, over, preview,
                                                  prism, singular, sumOf, to,
                                                  view, views)
import           Control.Monad                   (replicateM_)
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
import           Data.List                       (sortOn)
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NonEmpty
import           Data.Maybe                      (catMaybes, fromJust)
import           Data.MultiSet                   (MultiSet)
import qualified Data.MultiSet                   as MultiSet
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Data.Text                       (intercalate)
import qualified Data.Text                       as Text
import           System.IO.Advent                (getInput, solve)
import           Text.Parsec                     (anyChar, char, noneOf, (<|>))
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

-- TODO This tree may be good for a library
data Tree a = Node (Tree a) (Tree a) | Leaf a
  deriving (Show, Eq)

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

parser :: Parser Parsed
parser = linesOf treeP

treeP :: Parser (Tree Int)
treeP = (Leaf <$> digitAsNum) <|> nodeP

nodeP :: Parser (Tree Int)
nodeP =
  Node
  <$> (char '[' *> treeP <* char ',')
  <*> treeP <* char ']'

-- TODO Add Astar and runAstar to Astar
solver1 :: Parsed -> Int
solver1 input = undefined

solver2 :: Parsed -> Int
solver2 = undefined

main :: IO ()
main = solve day parser solver1 solver2
