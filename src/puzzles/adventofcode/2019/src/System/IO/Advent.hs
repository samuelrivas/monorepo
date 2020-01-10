module System.IO.Advent where

import           Prelude                hiding (readFile)

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text              (Text)
import           Data.Text.IO           (readFile)
import           System.FilePath        ((</>))
import           UnliftIO.Environment   (getEnv)

adventInputDir :: MonadIO m => m FilePath
adventInputDir = getEnv "ADVENT_INPUT_DIR"

getInput :: MonadIO m => String -> m Text
getInput day =
  let file = day <> ".txt"
  in (</> file) <$> adventInputDir >>= liftIO . readFile
