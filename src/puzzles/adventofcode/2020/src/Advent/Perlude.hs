{-# LANGUAGE NoImplicitPrelude #-}

-- TODO: Move this to Adventlib

-- TODO: For some reason, if I name this module Advent.Prelude, emacs barfs that
-- the module name doesn't match with the file name and doesn't compile
-- it. Since that is very annoying I decided to name it not-prelude. However,
-- ghci and ghc do compile with `Advent.Prelude` so I have no idea what is going
-- on. Figure it out!

-- To document: This removes references to String an IO from the standard
-- Prelude
module Advent.Perlude (
  read,
  show,
  putStr,
  putStrLn,
  appendFile,
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
import           Data.Text              (Text, pack, unpack, lines)
import qualified Data.Text.IO           as TextIO
import           Prelude                hiding (appendFile, getContents,
                                         getLine, interact, print, putStr,
                                         putStrLn, read, readFile, show, lines,
                                         writeFile)
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
