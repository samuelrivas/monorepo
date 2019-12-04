-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

factor :: Int -> [Int]
factor 0 = []
factor x = factor (x `div` 10) ++ [x `mod` 10]

has_double :: Eq a => [a] -> Bool
has_double l = any (uncurry  (==)) $ zip l (tail l)

is_ascending :: Ord a => [a] -> Bool
is_ascending l = all (uncurry  (<=)) $ zip l (tail l)

valid :: Int -> Bool
valid x =
  let
    digits = factor x
  in
    has_double digits && is_ascending digits

main :: IO ()
main =
  putStrLn $ "Solution 1: " <> show (length $ filter valid [256310..732736])
