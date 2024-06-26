{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Prelude             hiding (unlines)

import qualified Data.ByteString     as BS
import           Data.Maybe          (catMaybes, fromMaybe)
import           Data.Text           (Text, pack, unpack)
import qualified Data.Text           as Text
import           Data.Text.Encoding  (encodeUtf8)
import           Options.Applicative (Parser, ReadM, execParser, fullDesc, help,
                                      helper, info, long, maybeReader, metavar,
                                      option, optional, progDesc, strArgument,
                                      strOption, value)
import           System.Process      (readProcess)

data LibInfo = LibInfo
  { name              :: Text
  , version           :: Text
  , exposedModules    :: [Text]
  , importDir         :: Text
  , staticLibDir      :: Text
  , dynamicLibDir     :: Text
  , dependencies      :: [Text]
  , haddockInterfaces :: Maybe [Text]
  , haddockHtml       :: Maybe Text
  } deriving Show

toIds :: [Text] -> IO [Text]
toIds = traverse pkgId

addIds :: LibInfo -> IO LibInfo
addIds libInfo = do
  ids <- toIds . dependencies $ libInfo
  pure $ libInfo { dependencies = ids }

optionalField :: Text -> Maybe Text -> Maybe Text
optionalField name = fmap (name <>)

render :: LibInfo -> Text
render LibInfo {..} =
  let
    multivalue = Text.intercalate "\n" . fmap ("    " <>)
  in
    Text.intercalate "\n" $ [
    "name: " <> name,
    "version: " <> version,
    "id: " <> name,
    "key: " <> name,
    "exposed: True",
    "exposed-modules:",
    multivalue exposedModules,
    "import-dirs: " <> importDir,
    "library-dirs: " <> staticLibDir,
    "dynamic-library-dirs: " <> dynamicLibDir,
    "hs-libraries: HS" <> name,
    "depends:",
    multivalue dependencies
  ] <>
  catMaybes [
    optionalField "haddock-interfaces: \n" (multivalue <$> haddockInterfaces),
    optionalField "haddock-html: " haddockHtml
    ]


multiReader :: ReadM [Text]
multiReader = fmap pack <$> maybeReader (Just . words)

pkgId :: Text -> IO Text
pkgId pkg =
  Text.strip . pack
  <$> readProcess "ghc-pkg" ["field", unpack pkg, "id", "--simple-output"] ""

cmdParser :: Parser LibInfo
cmdParser =
  LibInfo
  <$> strArgument (metavar "LIBRARY")
  <*> strOption (long "version" <> help "Package version")
  <*> option multiReader
       (long "exposed"
        <> help "Exposed modules. They must be quoted, and separated by spaces"
        <> metavar "\"Mod1 Mod2 ...\"")
  <*> strOption (long "import-path" <> help "The import path")
  <*> strOption (long "static-lib-path" <> help "the static lib path")
  <*> strOption (long "dynamic-lib-path" <> help "the dynamic lib path")
  <*> option multiReader
       (long "dependencies"
         <> help "Package dependencies. They must be quoted, and separated by spaces"
         <> metavar "\"package1 package1 ...\"")
  <*> optional (option multiReader
       (long "haddock-interfaces"
        <> help ".haddock files exposed by this package"
        <> metavar "\"/path/to/foo.haddock /path/to/bar.haddock ...\""))
  <*> optional
       (strOption
        (long "haddock-html" <> help "Haddock html directory"))

main :: IO ()
main =
  let
    opts = info (helper <*> cmdParser)
      (fullDesc <> progDesc "Print a config file for LIBRARY")
  in
    execParser opts >>= addIds >>= BS.putStr . encodeUtf8 . render
