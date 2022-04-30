{-# LANGUAGE NoImplicitPrelude #-}

{-|
The intention of this alternative -Prelude- is to remove any references to
'String' or 'IO', substituting them with 'Text' and 'MonadIO'.
-}

-- TODO: This is not comprehensive yet, I have only added the functions that I
-- normally use.
module Perlude (
  appendFile,
  fail,
  getArgs,
  getContents,
  getLine,
  interact,
  print,
  putStr,
  putStrLn,
  hPrint,
  hPutStr,
  hPutStrLn,
  read,
  readFile,
  show,
  writeFile,
  module Data.Text,
  module Prelude,
  module Control.Monad.IO.Class,
  ) where

import           Control.Monad.IO.Class
import           Data.Text              (Text, lines, pack, unlines, unpack)
import qualified Data.Text.IO           as TextIO
import           Prelude                hiding (appendFile, fail, getContents,
                                         getLine, interact, lines, print,
                                         putStr, putStrLn, read, readFile, show,
                                         unlines, writeFile)
import qualified Prelude
import qualified System.Environment
import qualified System.IO

read :: Read a => Text -> a
read = Prelude.read . unpack

show :: Show a => a -> Text
show = pack . Prelude.show

putStr :: MonadIO m => Text -> m ()
putStr = liftIO . TextIO.putStr

putStrLn :: MonadIO m => Text -> m ()
putStrLn = liftIO . TextIO.putStrLn

hPrint :: MonadIO m => Show a => System.IO.Handle -> a -> m ()
hPrint h = liftIO . System.IO.hPrint h

hPutStrLn :: MonadIO m => System.IO.Handle -> Text -> m ()
hPutStrLn h = liftIO . System.IO.hPutStrLn h . unpack

hPutStr :: MonadIO m => System.IO.Handle -> Text -> m ()
hPutStr h = liftIO . System.IO.hPutStr h . unpack

appendFile :: MonadIO m => FilePath -> Text -> m ()
appendFile path  = liftIO . TextIO.appendFile path

getArgs :: MonadIO m => m [Text]
getArgs = liftIO $ fmap pack <$> System.Environment.getArgs

getContents :: MonadIO m => m Text
getContents = liftIO TextIO.getContents

getLine :: MonadIO m => m Text
getLine = liftIO TextIO.getLine

interact :: MonadIO m => (Text -> Text) -> m ()
interact = liftIO . TextIO.interact

fail :: MonadFail m => Text -> m a
fail = Prelude.fail . unpack

readFile :: MonadIO m => FilePath -> m Text
readFile = liftIO . TextIO.readFile

writeFile :: MonadIO m => FilePath -> Text -> m ()
writeFile path = liftIO . TextIO.writeFile path

print :: MonadIO m => Show a => a -> m ()
print = liftIO . Prelude.print
