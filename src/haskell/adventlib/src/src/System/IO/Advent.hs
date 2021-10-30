module System.IO.Advent where

import           Prelude                hiding (readFile)

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Advent            (Day (..))
import           Data.Text              (Text)
import           Data.Text.IO           (readFile)
import           System.FilePath        ((</>))
import           UnliftIO.Environment   (getEnv)

adventInputDir :: MonadIO m => m FilePath
adventInputDir = getEnv "ADVENT_INPUT_DIR"

getInput :: MonadIO m => Day -> m Text
getInput day =
  let file = dayToString day <> ".txt"
  in (</> file) <$> adventInputDir >>= liftIO . readFile

adventPath :: String -> FilePath -> IO FilePath
adventPath day path =  (</> day </> path) <$> adventInputDir

dayToString :: Day -> String
dayToString = show . (+ 1) . fromEnum
