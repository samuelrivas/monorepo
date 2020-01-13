module Perlude (
  show,
  showStr,
  module X,
  ) where

import           Data.Generics.Labels as X ()
import           Data.Text            as X (Text, pack)
import           Data.Text.IO         as X (putStrLn)
import           GHC.Generics         as X (Generic)
import           Prelude              as X hiding (putStrLn, show)
import qualified Prelude

show :: Show a => a -> Text
show = pack . Prelude.show

showStr :: Show a => a -> String
showStr = Prelude.show
