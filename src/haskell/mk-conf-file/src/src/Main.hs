{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Prelude                 hiding (unlines)

import qualified Data.ByteString.Lazy    as BS
import           Data.Text               (Text, pack, unlines, unpack)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Text.Template      (Context, Template, render, template)
import           Options.Applicative     (Parser, ReadM, auto, execParser,
                                          fullDesc, header, help, helper, info,
                                          long, maybeReader, metavar, option,
                                          progDesc, short, showDefault,
                                          strArgument, strOption, switch, value)

-- TODO Depending on 'template' just to do this is quite superficial, fix this
-- so that we just render what we want based on a 'LibInfo' file
fileTemplate :: Template
fileTemplate = template
  "name: $name\n\
  \version: 1.0\n\
  \id: $name\n\
  \key: $name\n\
  \exposed: True\n\
  \exposed-modules:\n\
  \$exposedModules\n\
  \import-dirs: $importDir\n\
  \library-dirs: $staticLibDir\n\
  \dynamic-library-dirs: $dynamicLibDir\n\
  \hs-libraries: HS$name\n\
  \depends:\n\
  \$dependencies"

mkContext :: LibInfo -> Context
mkContext LibInfo {..} =
  \case
    "name"           -> name
    "exposedModules" -> unlines exposedModules
    "importDir"      -> importDir
    "staticLibDir"   -> staticLibDir
    "dynamicLibDir"  -> dynamicLibDir
    "dependencies"   -> unlines dependencies
    other            -> error . unpack $ "wrong key " <> other

data LibInfo = LibInfo
  { name           :: Text
  , exposedModules :: [Text]
  , importDir      :: Text
  , staticLibDir   :: Text
  , dynamicLibDir  :: Text
  , dependencies   :: [Text]
  } deriving Show

multiReader :: ReadM [Text]
multiReader = fmap pack <$> maybeReader (Just . words)

cmdParser :: Parser LibInfo
cmdParser =
  LibInfo
  <$> strArgument (metavar "LIBRARY")
  <*> option multiReader
       (long "exposed"
        <> help "Exposed modules. They must be quoted, and separated by spaces"
        <> metavar "\"Mod1 Mod2 ...\"")
  <*> strOption (long "import-path")
  <*> strOption (long "static-lib-path")
  <*> strOption (long "dynamic-lib-path")
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
    . render fileTemplate
    . mkContext

