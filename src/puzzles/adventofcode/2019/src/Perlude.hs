-- A slightly adapted Prelude that you can import instead
--
-- The main purpose is to change common function signatures from 'String' to
-- 'Text', but there are a few more modifictions:
--
-- Export 'Generic' and the instances for 'IsLabel' for all instances of Generic
-- so that we can use lenses with @OverloadedLabels@ without adding noise to the
-- import list
--
-- Export 'pack' and 'unpack' from Text to relieve a bit the pain of dealing
-- with interfaces using Strings
module Perlude (
  show,
  showStr,
  error,
  module X,
  ) where

import           Data.Generics.Labels as X ()
import           Data.Text            as X (Text, pack, unpack)
import           Data.Text.IO         as X (putStrLn)
import           GHC.Generics         as X (Generic)
import           Prelude              as X hiding (error, putStrLn, show)
import qualified Prelude

show :: Show a => a -> Text
show = pack . Prelude.show

showStr :: Show a => a -> String
showStr = Prelude.show

error :: Text -> a
error = Prelude.error . unpack
