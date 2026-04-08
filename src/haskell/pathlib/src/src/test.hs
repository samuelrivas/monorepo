{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Perlude

import           Data.List      (intersperse, singleton)
import           Data.Text      (intercalate)
import           Hedgehog       (MonadGen, Property, check, forAll, property,
                                 (===))
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

data ProtoPath = ProtoPath {
  components :: [Text],
  leading    :: Bool,
  trailing   :: Bool
  }
  deriving stock Show

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

protoPathGen :: MonadGen m => m ProtoPath
protoPathGen =
  ProtoPath
  <$> Gen.list (Range.linear 0 100) componentGen
  <*> Gen.bool
  <*> Gen.bool

-- TODO: remove this one, we won't need it
pathTextGen :: MonadGen m => m Text
pathTextGen =
  do
    components <- Gen.list (Range.linear 0 100) componentGen
    return $ intercalate "/" components

slashGen :: MonadGen m => m Text
slashGen = Gen.text (Range.linear 1 100) (Gen.constant '/')

textPathGen :: MonadGen m => ProtoPath -> m [Text]
textPathGen proto =
  let
    tGens = intersperse slashGen (pure <$> components proto)
  in
    sequence tGens

testCaseGen :: MonadGen m => m (ProtoPath, Text)
testCaseGen =
  -- do
  --   proto <- protoPathGen
  --   text <- textPathGen proto
  --   pure (proto, text)
  undefined

-- TODO: rewrite with bool
genIf :: MonadGen m => Bool -> m Text -> m [Text]
genIf False _ = pure []
genIf True g  = singleton <$> g

main :: IO ()
main = do
  _ <- check prop_reverse
  _ <- check propPath
  pure ()
