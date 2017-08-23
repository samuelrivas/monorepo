import           Control.Monad.Except
import qualified Data.ByteString.Lazy    as B
import qualified Data.Char               as Char
import qualified Data.Text.Lazy          as T
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import           Data.Text.Lazy.Read     (decimal)
import           System.Environment      (getArgs)
import           System.Exit             (ExitCode (..), exitSuccess, exitWith)
import           System.IO               (stderr, stdout)

{-# ANN module "HLint: ignore Use camelCase" #-}

-- TODO Refactor to lib

type Result a = Either T.Text a

-- (>>|) :: Functor f => f a -> (a -> b) -> f b
-- (>>|) = flip fmap

map_left :: (a -> c) -> Either a b -> Either c b
map_left f (Left x)  = Left $ f x
map_left _ (Right x) = Right x

read_int :: T.Text -> Result Int
read_int text =
  let to_result = map_left T.pack
  in do
    (int, remaining) <- to_result $ decimal text
    if T.null remaining
      then
      return int
      else
      throwError (T.append text (T.pack " cannot be parsed into a number"))

print_error :: T.Text -> IO ()
print_error = B.hPut stderr . encodeUtf8

exit :: Result a -> IO ()
exit (Right _) = exitSuccess
exit (Left e) = do
  print_error e
  exitWith (ExitFailure 1)

-- Command line handling

parse_args :: [T.Text] -> Result Int
parse_args [x] = read_int x
parse_args _   = throwError $ T.pack "You must provide exactly one argument"

get_context :: ExceptT T.Text IO Int
get_context = do
  args <- lift $ fmap T.pack <$> getArgs
  ExceptT $ return $ parse_args args

-- Meat of the program

padding :: Integral t => t -> T.Text
padding context = let pad_char = Char.chr 0
  in T.replicate (fromIntegral context) $ T.singleton pad_char

pad :: Integral t => t -> T.Text -> T.Text
pad context =
  let padding' = padding context
  in T.append padding' . flip T.append padding'

chunk :: Integral t => t -> T.Text -> [T.Text]
chunk context text =
  let to_take = fromIntegral $ context * 2
  in
  case T.uncons text of
    Just (h, t) | T.length t >= to_take ->
                  T.cons h (T.take to_take t) : chunk context t

    _         -> []

chunks_to_bytes :: [T.Text] -> B.ByteString
chunks_to_bytes = encodeUtf8 . T.unlines

-- Impure world

output_chunked :: Integral t => t -> T.Text -> IO ()
output_chunked context text =
  let chunks = chunk context $ pad context text
      bytes = chunks_to_bytes chunks
  in B.hPut stdout bytes

get_input :: IO T.Text
get_input = decodeUtf8 <$> B.getContents

run_with_context :: Integral t => t -> IO ()
run_with_context = (get_input >>=) . output_chunked

main_t :: ExceptT T.Text IO ()
main_t = get_context >>= lift . run_with_context

main :: IO ()
main = runExceptT main_t >>= exit
