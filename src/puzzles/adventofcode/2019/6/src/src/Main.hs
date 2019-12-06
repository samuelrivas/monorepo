{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

import           Prelude               hiding (getLine, lines, putStrLn,
                                        unlines)

import           Control.Lens          (assign, ix, modifying, preview, use,
                                        uses)
import           Control.Monad         (when)
import           Control.Monad.Loops   (whileM_)
import           Control.Monad.Reader  (MonadReader)
import           Control.Monad.RWS.CPS (RWST, evalRWST, execRWST, get, put,
                                        runRWST, tell)
import           Control.Monad.State   (MonadState)
import           Control.Monad.Writer  (MonadWriter)
import           Data.Array            (elems, listArray, (!), (//))
import           Data.Generics.Labels  ()
import           Data.List             (uncons)
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text, lines, pack, splitOn, unlines,
                                        unpack)
import           Data.Text.IO          (getLine, putStrLn)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

test_input :: Text
test_input =
  unlines
  ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L"]

parse_orbit :: Text -> Maybe (Text, Text)
parse_orbit input = do
  [x, y] <- Just . splitOn ")" $ input
  pure (x, y)

parse_input :: Text -> Maybe [(Text, Text)]
parse_input input = sequence (parse_orbit <$> lines input)

main :: IO ()
main = undefined
