{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module TenKindsOfPeople where

import           Perlude

import           Control.Monad        (when)
import           Control.Monad.ST     (ST, runST)
import           Data.Array.Base      (IArray, UArray, array, bounds, indices,
                                       unsafeAt, (!))
import           Data.Char            (ord)
import           Data.Coerce
import           Data.Foldable        (traverse_)
import           Data.Ix              (Ix (..))
import           Data.List.NonEmpty   (NonEmpty (..), nonEmpty)
import           Data.Map.Strict      (insert, member, size)
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromJust, fromMaybe)
import           Data.Text            (chunksOf, intercalate)
import           GHC.Char             (chr)
import           GHC.Ix               (Ix (unsafeIndex))
import           Internal.UnionFind   (MutableUnionFind, UnionFind, find', new,
                                       toUnionFind, union)
import qualified Prelude
import           Text.Parsec          (count, newline, sepEndBy, space)
import           Text.Parsec.Parselib (Parser, bit, digitsAsNum, unsafeParse)

-- Input to the problem
data Input = Input {
  maxCoord :: Coord,
  bitmap   :: UArray Coord Bool,
  queries  :: [(Coord, Coord)]
  } deriving stock Show

-- Types to make sure that we don't mix Rows with Columns. Everything is
-- 1-indexed, so we don't create types to differentiate 1 from 0 indexes

newtype Row = Row { unRow :: Int }
  deriving newtype (Eq, Ord, Ix, Num, Enum)

instance Show Row where
  show = ("Row:" ++) .  Prelude.show . unRow

newtype Col = Col { unCol :: Int }
  deriving newtype (Eq, Ord, Ix, Num, Enum)

instance Show Col where
  show = ("Col:" ++) .  Prelude.show . unCol

-- In this problem coordinates are row, col, which is the reverse of how
-- Cartesian coordinates are usually represented
newtype Coord = Coord { unCoord :: (Row, Col) }
  deriving newtype (Eq, Ord, Ix)

instance Show Coord where
  show (Coord (Row r, Col c)) = "Coord" ++ Prelude.show (r, c)

coord :: Row -> Col -> Coord
coord = curry Coord

toRow :: Coord -> Row
toRow = fst . unCoord

toCol :: Coord -> Col
toCol = snd . unCoord

sumCoord :: Coord -> Coord -> Coord
sumCoord (Coord (r1, c1)) (Coord (r2, c2)) = coord (r1 + r2) (c1 + c2)

coordUp :: Coord -> Coord
coordUp = sumCoord $ coord 1 0

coordRight :: Coord -> Coord
coordRight = sumCoord $ coord 0 1

example1 :: Text
example1 =
  intercalate "\n"
  [
    "1 4",
    "1100",
    "2",
    "1 1 1 4",
    "1 1 1 1"
  ]

exampleFoo :: Text
exampleFoo =
  intercalate "\n"
  [
    "2 4",
    "1100",
    "0110",
    "2",
    "1 1 1 4",
    "1 1 1 1",
    ""
  ]

example2 :: Text
example2 =
  intercalate "\n"
  [
    "10 20",
    "11111111111111111111",
    "11000000000000000101",
    "11111111111111110000",
    "11111111111111110000",
    "11000000000000000111",
    "00011111111111111111",
    "00111111111111111111",
    "10000000000000001111",
    "11111111111111111111",
    "11111111111111111111",
    "3",
    "2 3 8 16",
    "8 1 7 3",
    "1 1 10 20",
    ""
  ]

parser :: Parser Input
parser =
  do
    maxCoord <- parseCoord <* newline
    bitmap <- parseBitmap maxCoord
    _ :: Int <- digitsAsNum <* newline
    queries <- sepEndBy parseQuery newline
    return $ Input maxCoord  bitmap queries

parseCoord :: Parser Coord
parseCoord =
  (coord . Row <$> digitsAsNum) <* space
  <*> (Col <$> digitsAsNum)

parseBitmapRow :: Col -> Parser [Bool]
parseBitmapRow cols = count (coerce cols) bit <* newline

parseBitmap :: Coord -> Parser (UArray Coord Bool)
parseBitmap maxCoord@(Coord (rows, cols)) =
  mkBitmapArray maxCoord <$> count (coerce rows) (parseBitmapRow cols)

mkBitmapArray :: Coord -> [[Bool]] -> UArray Coord Bool
mkBitmapArray maxCoord bits =
  let
    is = coord <$> [1..toRow maxCoord] <*> [1..toCol maxCoord]
  in
    array (origin, maxCoord) $ zip is (concat bits)

origin :: Coord
origin = coord 1 1

forceNonEmpty :: [a] -> NonEmpty a
forceNonEmpty = fromJust . nonEmpty

parseQuery :: Parser (Coord, Coord)
parseQuery =
  (,)
  <$> parseCoord <* space
  <*> parseCoord

makeMutableUF :: UArray Coord Bool -> ST s (MutableUnionFind s Coord)
makeMutableUF bitmap =
  do
    uf <- uncurry new $ bounds bitmap
    traverse_ (connectNeighbours uf bitmap) (indices bitmap)
    return uf

connectNeighbours ::
  MutableUnionFind s Coord
  -> UArray Coord Bool
  -> Coord
  -> ST s ()
connectNeighbours uf bitmap x =
  do
    connectIfSame uf bitmap x (coordUp x)
    connectIfSame uf bitmap x (coordRight x)

connectIfSame ::
  MutableUnionFind s Coord
  -> UArray Coord Bool
  -> Coord
  -> Coord
  -> ST s ()
connectIfSame uf bitmap x y = when (sameBit bitmap x y) $ union uf x y

-- Works for any coords, returns false if any of the coords is out of bounds
sameBit :: UArray Coord Bool -> Coord -> Coord -> Bool
sameBit bitmap x y = fromMaybe False $ (==) <$> bitmap !? x <*> bitmap !? y

makeUF :: UArray Coord Bool -> UnionFind Coord
makeUF bitmap =
  runST $ makeMutableUF bitmap >>= toUnionFind

-- Useful for debugging. Prints a map where each root is printed for each
-- location in the original bitmap
--
-- TODO: probably should move to the UnionFind module
printAreas :: Coord -> UnionFind Coord -> Text
printAreas maxCoord uf =
  let
    roots = find' uf <$> range (coord 1 1, maxCoord)
    toChar = chr . (ord 'A' +)
    oneLine = pack $ toChar <$> compress roots
  in
    intercalate "\n" $ chunksOf (coerce $ toCol maxCoord) oneLine


compress :: [Coord] -> [Int]
compress roots =
  let
    addRep reps x = if member x reps
                    then reps
                    else insert x (size reps) reps
    repMap = foldl' addRep Map.empty roots
  in

    (repMap Map.!) <$> roots

connected :: UnionFind Coord -> (Coord, Coord) -> Bool
connected uf (x, y) = find' uf x == find' uf y

query :: UnionFind Coord -> UArray Coord Bool -> (Coord, Coord) -> Text
query uf bitmap q@(c1, _) =
  if connected uf q
  then toText $ bitmap ! c1
  else "neither"

toText :: Bool -> Text
toText True  = "decimal"
toText False = "binary"

solve :: Input -> [Text]
solve input =
  let
    uf = makeUF $ bitmap input
    responses = query uf (bitmap input) <$> queries input
  in
    responses

main :: IO ()
main =
  do
    contents <- getContents
    solveIO contents

solveIO :: Text -> IO ()
solveIO contents =
  do
    input <- unsafeParse parser contents
    putStrLn . intercalate "\n" $ solve input

-- Copied from Array 0.5.8.0 as Kattis uses an older version
-- =========================================================

{-# INLINE (!?) #-}
-- | Returns 'Just' the element of an immutable array at the specified index,
-- or 'Nothing' if the index is out of bounds.
--
-- @since 0.5.6.0
(!?) :: (IArray a e, Ix i) => a i e -> i -> Maybe e
(!?) arr i = let b = bounds arr in
             if inRange b i
             then Just $ unsafeAt arr $ unsafeIndex b i
             else Nothing
