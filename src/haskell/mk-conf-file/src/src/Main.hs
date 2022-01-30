{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude                 hiding (unlines)

import qualified Data.ByteString.Lazy    as BS
import           Data.Semigroup          ((<>))
import           Data.Text               (Text, unlines, unpack)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Text.Template      (Context, Template, render, template)

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

mkContext :: Text -> [Text] -> Text -> Text ->  Text -> [Text] -> Context
mkContext name exposedModules importDir staticLibDir dynamicLibDir dependencies =
  \case
    "name"           -> name
    "exposedModules" -> unlines exposedModules
    "importDir"      -> importDir
    "staticLibDir"   -> staticLibDir
    "dynamicLibDir"  -> dynamicLibDir
    "dependencies"   -> unlines dependencies
    other            -> error . unpack $ "wrong key " <> other

main :: IO ()
main =
  BS.putStr
  . encodeUtf8
  . render fileTemplate
  $ mkContext "my-lib" ["Data.Mylib", "System.Mylib"] "import" "static" "dynamic" ["liba", "libb"]
