{-# LANGUAGE NoImplicitPrelude #-}

{-|
The intention of this alternative -Prelude- is to remove any references to
'String' or 'IO', substituting them with 'Text' and 'MonadIO'.
-}

-- TODO: This is not comprehensive yet, I have only added the functions that I
-- normally use.
module Perlude (
  read,
  show,
  putStr,
  putStrLn,
  appendFile,
  getContents,
  getLine,
  writeFile,
  interact,
  readFile,
  print,
  module Data.Text,
  module Prelude,
  module Control.Monad.IO.Class,
  ) where

import           Control.Monad.IO.Class
import           Data.Text              (Text, lines, pack, unlines, unpack)
import qualified Data.Text.IO           as TextIO
import           Prelude                hiding (appendFile, getContents,
                                         getLine, interact, lines, print,
                                         putStr, putStrLn, read, readFile, show,
                                         unlines, writeFile)
import qualified Prelude

read :: Read a => Text -> a
read = Prelude.read . unpack

show :: Show a => a -> Text
show = pack . Prelude.show

putStr :: MonadIO m => Text -> m ()
putStr = liftIO . TextIO.putStr

putStrLn :: MonadIO m => Text -> m ()
putStrLn = liftIO . TextIO.putStr

appendFile :: MonadIO m => FilePath -> Text -> m ()
appendFile path  = liftIO . TextIO.appendFile path

getContents :: MonadIO m => m Text
getContents = liftIO TextIO.getContents

getLine :: MonadIO m => m Text
getLine = liftIO TextIO.getLine

interact :: MonadIO m => (Text -> Text) -> m ()
interact = liftIO . TextIO.interact

readFile :: MonadIO m => FilePath -> m Text
readFile = liftIO . TextIO.readFile

writeFile :: MonadIO m => FilePath -> Text -> m ()
writeFile path = liftIO . TextIO.writeFile path

print :: MonadIO m => Show a => a -> m ()
print = liftIO . Prelude.print
