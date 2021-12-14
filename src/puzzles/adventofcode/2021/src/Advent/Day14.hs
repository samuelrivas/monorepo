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
module Advent.Day14 where

import           Perlude

import           Advent.Templib       (linesOf)

import           Control.Lens         (_1, _2, over, view)
import           Control.Monad        (replicateM_)
import           Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import           Control.Monad.State  (MonadState, State, gets, modify,
                                       runState)
import           Data.Advent          (Day (..))
import           Data.Generics.Labels ()
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HashMap
import           Data.List            (sortOn)
import           Data.Maybe           (catMaybes, fromJust)
import           Data.MultiSet        (MultiSet)
import qualified Data.MultiSet        as MultiSet
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (intercalate)
import qualified Data.Text            as Text
import           System.IO.Advent     (getInput, solve)
import           Text.Parsec          (anyChar, noneOf)
import           Text.Parsec.Parselib (Parser, literal, text1, unsafeParseAll)

data Instruction = FoldX Int | FoldY Int deriving (Eq, Show)

type Parsed =  (Text, [(Text, Char)])

day :: Day
day = D14

rawInput :: IO Text
rawInput = getInput day

example :: Text
example =
  intercalate
  "\n" [
  "NNCB",
  "",
  "CH -> B",
  "HH -> N",
  "CB -> H",
  "NH -> C",
  "HB -> C",
  "HC -> B",
  "HN -> C",
  "NN -> C",
  "BH -> H",
  "NC -> B",
  "NB -> B",
  "BN -> B",
  "BB -> N",
  "BC -> B",
  "CC -> N",
  "CN -> C"
  ]

parsedExample :: Parsed
parsedExample = fromJust $ unsafeParseAll parser example

parser :: Parser Parsed
parser =
  (,)
  <$> text1 (noneOf "\n") <* literal "\n\n"
  <*> linesOf ruleP

ruleP :: Parser (Text, Char)
ruleP =
  (,)
  <$> text1 (noneOf " ") <* literal " -> "
  <*> anyChar

type Rules = HashMap (Char, Char) Char
type Polymer = MultiSet (Char, Char)

mkRules :: [(Text, Char)] -> Rules
mkRules = HashMap.fromList . over (traverse . _1) toPair

toPair :: Text -> (Char, Char)
toPair t =
  case unpack t of
    [a, b] -> (a, b)
    _      -> error "something i wrong"

guard :: Char
guard = '$'

-- We append the guards to avoid having to handle the special case of the first
-- and last pair not duplicating one of the ends
mkPolymer :: Text -> Polymer
mkPolymer t =
  let
    chars = unpack (flip Text.snoc guard . Text.cons guard $ t)
  in
    MultiSet.fromList $ zip chars (tail chars)

step :: MonadReader Rules m
  => MonadState Polymer m
  => m ()
step = do
  newPolymer <- newPairs
  removeBrokenPairs
  modify (MultiSet.union newPolymer)

newPairs :: MonadReader Rules m => MonadState Polymer m => m Polymer
newPairs = do
  rules <- asks HashMap.toList
  MultiSet.unions <$> (fmap catMaybes . traverse polymerise $ rules)

removeBrokenPairs :: MonadReader Rules m => MonadState Polymer m => m ()
-- removeBrokenPairs = pairsToBreak >>= modify . flip MultiSet.difference
removeBrokenPairs = do
  toBreak <- pairsToBreak
  modify $ MultiSet.filter (not . (`Set.member` toBreak))

-- TODO prettify this when time allows :)
-- TODO maybe get a lens for MultiSet
polymerise :: MonadReader Rules m
  => MonadState Polymer m
  => ((Char, Char), Char)
  -> m (Maybe (MultiSet (Char, Char)))
polymerise (pair@(a, b), new) = do
  gets (MultiSet.occur pair) >>= \case
    0 -> pure Nothing
    n -> pure . Just . MultiSet.fromOccurList $
         [((a, new), n), ((new, b), n)]

-- TODO  I am pretty sure there is a better way of doing this, try to find it
countElements :: Polymer -> MultiSet Char
countElements =
  let
    toOccurs ((a, b), n) = [(a, n), (b, n)]
  in
    MultiSet.fromOccurList
    . fmap (over _2 (`div` 2))
    . MultiSet.toOccurList . MultiSet.fromOccurList
    . filter ((/= guard) . view _1)
    . concatMap toOccurs
    . MultiSet.toOccurList

pairsToBreak :: MonadReader Rules m => m (Set (Char, Char))
pairsToBreak = asks $ Set.fromList . HashMap.keys

runPolymer :: Parsed -> ReaderT Rules (State Polymer) a -> (a, Polymer)
runPolymer (polymer, rules) x =
  runState (runReaderT x (mkRules rules)) (mkPolymer polymer)

solver :: Int -> Parsed -> Int
solver n input =
  let
    elements =
      sortOn (view _2)
      . MultiSet.toOccurList
      . countElements
      . view _2
      $ runPolymer input (replicateM_ n step)
    least = view _2 . head $ elements
    most = view _2 . last $ elements
  in
    most - least

solver1 :: Parsed -> Int
solver1 = solver 10

solver2 :: Parsed -> Int
solver2 = solver 40

main :: IO ()
main = solve day parser solver1 solver2
