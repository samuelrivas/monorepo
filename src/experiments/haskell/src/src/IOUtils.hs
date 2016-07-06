module IOUtils (
  repeat_until_just
  ) where

{-# ANN module "HLint: ignore Use camelCase" #-}

repeat_until_just :: IO (Maybe a) -> IO a
repeat_until_just action =
  action >>= maybe (repeat_until_just action) return
