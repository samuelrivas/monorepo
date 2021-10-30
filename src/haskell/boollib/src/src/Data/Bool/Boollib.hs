-- | Functions to operate with 'Bool'

module Data.Bool.Boollib (
  xor
  ) where

-- | Logic exclusive or
xor :: Bool -> Bool -> Bool
xor True True   = False
xor False False = False
xor _ _         = True

