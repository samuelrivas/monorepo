module Perlude (
  show,
  module X,
  ) where

import           Data.Text    as X (Text, pack)
import           Data.Text.IO as X (putStrLn)
import           Prelude      as X hiding (putStrLn, show)
import qualified Prelude

show :: Show a => a -> Text
show = pack . Prelude.show

