{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- Prepares a file for submission
--
-- Eventually, it will add all depdenencies, for now it just renames the main
-- module as Main and writes the resulting code to a temp directory

module Submit where

import           Perlude

import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT,
                                             withExceptT)
import           Data.Text                  (replace)
import           System.Exit                (exitFailure)
import           System.FilePath            (takeBaseName, (</>))
import           System.Posix               (mkdtemp)
import           System.Posix.Env           (getEnvDefault)

parseArgs :: MonadError Text m => [Text] -> m Text
parseArgs [x] = return x
parseArgs _   = throwError "Invalid amount of arguments"

usage :: Text -> Text
usage progName = "Usage: " <> progName <> " <file>"

runApp :: MonadIO m => ExceptT Text m a -> m a
runApp x = runExceptT x >>= \case
  Right a -> return a
  Left error -> do
    putStrLn error
    liftIO exitFailure

addUsage :: Text -> Text -> Text
addUsage progName = (<> "\n" <> usage progName)

baseName :: Text -> Text
baseName fileName = pack $ takeBaseName (unpack fileName)

mainify :: Text -> Text -> Text
mainify fileName = replace (baseName fileName) "Main"

getTmp :: MonadIO m => m FilePath
getTmp = liftIO $ getEnvDefault "TMPDIR" "/tmp"

main :: IO ()
main = do
  runApp $ do
    prog <- getProgramName
    file <- withExceptT (addUsage prog) (getArgs >>= parseArgs)
    code <- readFile $ unpack file
    tmpDir <- getTmp
    outputDir <- lift (mkdtemp $ tmpDir <> "/kattis-submit-")
    writeFile (outputDir </> unpack file) $ mainify file code
    putStrLn $ pack outputDir
