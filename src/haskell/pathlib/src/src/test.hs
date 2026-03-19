{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Perlude

import           Data.Text      (intercalate)
import           Hedgehog       (MonadGen, Property, check, forAll, property,
                                 (===))
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

prop_reverse :: Property
prop_reverse = property $ do
  xs <- forAll $
    Gen.list (Range.linear 0 100) (Gen.int (Range.linear minBound maxBound))
  reverse (reverse xs) === xs

propPath :: Property
propPath =
  property $ do
  ps  <- forAll $
    pathTextGen
  ps === ""

anyCharGen :: MonadGen m => m Char
anyCharGen = Gen.frequency [(5, Gen.alphaNum), (2, Gen.latin1), (1, Gen.unicode)]

componentGen :: MonadGen m => m Text
componentGen = Gen.text (Range.linear 1 100) anyCharGen

pathTextGen :: MonadGen m => m Text
pathTextGen =
  do
    components <- Gen.list (Range.linear 0 100) componentGen
    return $ intercalate "/" components

main :: IO ()
main = do
  _ <- check prop_reverse
  _ <- check propPath
  pure ()
