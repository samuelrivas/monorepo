{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Templib (
  conv
  ) where

import           Control.Applicative ((<$>))
import           Data.List           (tails)

conv :: ([a] -> b) -> [a] -> [b]
conv f l = f <$> tails l

conv' f l = 
