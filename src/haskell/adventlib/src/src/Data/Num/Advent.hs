-- | Utilities to operate with numbers in various representations

module Data.Num.Advent (
  numListToNum,
  bitListToDec,
  numListToDec
  ) where

-- | Convert a list of digits into a number using the given base.
--
-- For example:
--
-- >>> numListToNum 10 [1, 2, 3]
-- 123
-- >>> numListToNum 2 [1, 0, 1]
-- 5
numListToNum :: Num x => x -> [x] -> x
numListToNum base = foldl' (\acc x -> acc * base + x) 0

-- | Convert a list of bits into a decimal number.
bitListToDec :: Num x => [Bool] -> x
bitListToDec = fromIntegral . numListToNum 2 . fmap fromEnum

-- | Convert a list of digits into a decimal number.
numListToDec :: Num x => [x] -> x
numListToDec = numListToNum 10
