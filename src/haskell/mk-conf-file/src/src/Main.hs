{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Prelude             hiding (unlines)

import qualified Data.ByteString     as BS
import           Data.Text           (Text, intercalate, pack)
import           Data.Text.Encoding  (encodeUtf8)
import           Options.Applicative (Parser, ReadM, execParser, fullDesc, help,
                                      helper, info, long, maybeReader, metavar,
                                      option, progDesc, strArgument, strOption)

data LibInfo = LibInfo
  { name           :: Text
  , version        :: Text
  , exposedModules :: [Text]
  , importDir      :: Text
  , staticLibDir   :: Text
  , dynamicLibDir  :: Text
  , dependencies   :: [Text]
  } deriving Show

render :: LibInfo -> Text
render LibInfo {..} =
  let
    multivalue = intercalate "\n" . fmap ("    " <>)
  in
    intercalate "\n" [
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
  ]

multiReader :: ReadM [Text]
multiReader = fmap pack <$> maybeReader (Just . words)

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

main :: IO ()
main =
  let
    opts = info (helper <*> cmdParser)
      (fullDesc <> progDesc "Print a config file for LIBRARY")
  in
    execParser opts >>=
    BS.putStr
    . encodeUtf8
    . render

