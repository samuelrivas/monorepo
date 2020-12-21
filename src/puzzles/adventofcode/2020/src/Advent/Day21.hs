{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Advent.Day21 where

import           Advent.Perlude

import           Control.Lens        (at, both, each, foldlOf, non, over,
                                      preview, view, _1, _2, _head)
import           Control.Monad       (guard)
import           Control.Monad.State (MonadState)
import           Data.Hashable       (Hashable)
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HashSet
import           Data.List           (find, foldl', sort, unfoldr)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Maybe          (catMaybes)
import           Data.Maybe          (fromJust, isJust)
import qualified Data.Text           as Text
import qualified System.IO.Advent    as IOAdvent

-- example = [
--   (["mxmxvkd", "kfcds", "sqjhc", "nhms"], ["dairy", "fish"]),
--   (["trh", "fvjkl", "sbzzf", "mxmxvkd"], ["contains", "dairy"]),
--   (["sqjhc", "fvjkl"], ["soy"]),
--   (["sqjhc", "mxmxvkd", "sbzzf"], ["fish"])]

example :: Text
example = "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\n\
          \trh fvjkl sbzzf mxmxvkd (contains dairy)\n\
          \sqjhc fvjkl (contains soy)\n\
          \sqjhc mxmxvkd sbzzf (contains fish)\n"

parseFood :: Text -> [(Text, [Text])]
parseFood text =
  let
    (ingredientsText, allergensText) =
      over both Text.strip $ Text.breakOn "(" text

    ingredients = Text.words ingredientsText

    allergens =
        Text.splitOn ", "
      . fromJust . Text.stripPrefix "contains "
      . Text.dropAround (`elem`  ['(', ')'])
      $ allergensText
  in do
    allergen <- allergens
    pure (allergen, ingredients)

-- (allergen -> [possible ingredient]
parse :: Text -> [(Text, [Text])]
parse = concatMap parseFood . Text.lines

-- TODO: move to utils
intersections :: Eq a => Hashable a => [HashSet a] -> HashSet a
intersections []    = HashSet.empty
intersections (h:t) = foldl' HashSet.intersection h t

-- Given a set of ingredients that have known allergens, and an set of
-- ingredients that possibly have a concrete alergen, figure out if we can
-- deduct one ingredient containing that alergen. Note that if the ingredient is
-- already known to have that alergen we won't deduct it
findIngredient :: HashSet Text -> [HashSet Text] -> Maybe Text
findIngredient knownIngredients ingredients =
  let
    possible =
      HashSet.difference (intersections ingredients) knownIngredients

  in if HashSet.size possible == 1 then preview _head .  HashSet.toList $ possible
     else Nothing

-- Find another mapping (allergen -> ingredient)
deductNext :: HashSet Text -> [(Text, [HashSet Text])] -> Maybe (Text, Text)
deductNext knownIngredients l =
  let
    deduction (allergen, ingredients) =
      (allergen, ) <$> findIngredient knownIngredients ingredients
  in
    preview _head $ catMaybes (deduction <$> l)

getInput :: IO Text
getInput = IOAdvent.getInput "21"

main :: IO ()
main = do
  input <- getInput

  putStr "Solution 1: "
  print $ "NA"

  putStr "Solution 2: "
  print $ "NA"
