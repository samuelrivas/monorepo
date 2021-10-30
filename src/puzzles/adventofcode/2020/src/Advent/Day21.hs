{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Advent.Day21 where

import           Perlude

import           Control.Lens         (_1, _2, _head, at, both, each, foldlOf,
                                       non, over, preview, view)
import           Control.Monad.Loops  (whileJust)
import           Control.Monad.Reader (MonadReader, Reader, ask, runReader)
import           Control.Monad.State  (MonadState, StateT, evalStateT, get,
                                       modify)
import           Data.Advent          (Day (..))
import           Data.HashSet         (HashSet)
import qualified Data.HashSet         as HashSet
import           Data.Hashable        (Hashable)
import           Data.List            (foldl', sort)
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (catMaybes, fromJust)
import qualified Data.Text            as Text
import qualified System.IO.Advent     as IOAdvent

-- TODO: we are using two parsers here, which is messy and confusing. Fix that
-- TODO: generalise this. Many problems are similar to this, see day 16 for
-- exampe
getInput :: IO Text
getInput = IOAdvent.getInput D21

example :: Text
example = "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\n\
          \trh fvjkl sbzzf mxmxvkd (contains dairy)\n\
          \sqjhc fvjkl (contains soy)\n\
          \sqjhc mxmxvkd sbzzf (contains fish)\n"

parseIngredients :: Text -> [Text]
parseIngredients text =
  let
    (ingredientsText, _allergensText) =
      over both Text.strip $ Text.breakOn "(" text

  in Text.words ingredientsText

-- Produces a map allegen -> [Ingredients]
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

toSets ::  [(Text, [Text])] -> [(Text, [HashSet Text])]
toSets =   Map.toList
         . Map.fromListWith (++)
         . fmap (over _2 ((:[]) . HashSet.fromList))

-- Get all the ingredients concatenated (to count for part 1)
allIngredientList :: Text -> [Text]
allIngredientList = concatMap parseIngredients . Text.lines

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

allergenSet :: [(Text, [HashSet Text])] -> HashSet Text
allergenSet = HashSet.fromList . fmap (view _1)

findNext ::
  MonadState (HashSet Text) m => MonadReader [(Text, [HashSet Text])] m =>
  m (Maybe (Text, Text))
findNext = do
  known <- get
  menus <- ask
  case deductNext known menus of
    Nothing -> pure Nothing
    Just (allergen, ingredient) -> do
      modify $ HashSet.insert ingredient
      pure $ Just (allergen, ingredient)

deductAll :: MonadState (HashSet Text) m => MonadReader [(Text, [HashSet Text])] m =>
  m [(Text, Text)]
deductAll = whileJust findNext pure

run ::
     [(Text, [HashSet Text])]
  -> StateT (HashSet Text) (Reader [(Text, [HashSet Text])]) a
  -> a
run menu x = runReader (evalStateT x HashSet.empty) menu

solution1 :: Text -> Int
solution1 text =
  let
    menu = toSets . parse $ text
    deduced = run menu deductAll
    allIngredients = allIngredientList text
  in
    length $ filter (not . (`elem` (view _2 <$> deduced))) allIngredients

solution2 :: Text -> Text
solution2 text =
  let
    menu = toSets . parse $ text
    deduced = run menu deductAll
  in
    Text.intercalate "," . fmap (view _2) . sort $ deduced

main :: IO ()
main = do
  input <- getInput

  putStr "Solution 1: "
  print . solution1 $ input

  putStr "Solution 2: "
  putStrLn . solution2 $ input
