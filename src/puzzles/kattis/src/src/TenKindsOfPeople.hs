{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

-- FIXME: The code is very messy and difficult to follow, this needs massive
-- cleanup
--
-- Create a type to differentiate the different types of coordinates, because we are mixing zero and 1 based x-y and row-column coordinates, which is very confusing
module TenKindsOfPeople where

import           Perlude

import           Control.Lens         (both, over, view)
import           Control.Monad        (foldM_, when)
import           Control.Monad.ST     (ST, runST)
import           Control.Monad.Zip    (mzip)
import qualified Prelude
-- import           Data.Bidim           (Bidim, boundaries, cell, fromList,
--                                        fromText)
import           Data.Array.Base      (IArray, UArray, array, bounds, indices,
                                       (!), (!?))
import           Data.Bifunctor       (Bifunctor (..))
import           Data.Char            (ord)
import           Data.Coerce
import           Data.Either          (fromRight)
import           Data.Foldable        (foldl', traverse_)
import           Data.Ix              (Ix (..))
import           Data.List.NonEmpty   (NonEmpty (..), nonEmpty)
import           Data.Map.Strict      (insert, member, size)
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromJust, fromMaybe)
import           Data.Text            (chunksOf, intercalate)
import           Data.Tuple           (swap)
import           GHC.Char             (chr)
import           Internal.UnionFind   (MutableUnionFind, UnionFind, find', new,
                                       toUnionFind, union)
import           Text.Parsec          (count, manyTill, newline, noneOf, sepBy,
                                       sepEndBy, space)
import           Text.Parsec.Char     (anyChar)
import           Text.Parsec.Parselib (Parser, bit, digitsAsNum, parse, text,
                                       unsafeParse)


-- Types to make sure that we don't mix Rows with Columns. Everything is
-- 1-indexed, so we don't box for that

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

data Input = Input {
  maxCoord :: Coord,
  bitmap   :: UArray Coord Bool,
  queries  :: [(Coord, Coord)]
  } deriving stock Show

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
    indices = coord <$> [1..toRow maxCoord] <*> [1..toCol maxCoord]
  in
    array (origin, maxCoord) $ zip indices (concat bits)

origin :: Coord
origin = coord 1 1

forceNonEmpty :: [a] -> NonEmpty a
forceNonEmpty = fromJust . nonEmpty

parseQuery :: Parser (Coord, Coord)
parseQuery =
  (,)
  <$> parseCoord <* space
  <*> parseCoord

-- Coord is 1-based (c, r)
-- toUFIndex :: Int -> Coord -> Int
-- toUFIndex columns (Coord x y) =
--   (x - 1) + columns * (y - 1)

-- toCoord :: Int -> Int -> Coord
-- toCoord columns n = (n `mod` columns + 1, n `div` columns + 1)

makeMutableUF :: UArray Coord Bool -> ST s (MutableUnionFind s Coord)
makeMutableUF bitmap =
  do
    uf <- uncurry new $ bounds bitmap
    traverse_ (connectNeighbours uf bitmap) (indices bitmap)
    return uf

connectNeighbours :: MutableUnionFind s Coord -> UArray Coord Bool -> Coord -> ST s ()
connectNeighbours uf bitmap x =
  do
    connectIfSame uf bitmap x (coordUp x)
    connectIfSame uf bitmap x (coordRight x)

connectIfSame :: MutableUnionFind s Coord -> UArray Coord Bool -> Coord -> Coord -> ST s ()
connectIfSame uf bitmap x y = when (sameBit bitmap x y) $ union uf x y

-- Works for any coords, returns false if any of the coords is out of bounds
sameBit :: UArray Coord Bool -> Coord -> Coord -> Bool
sameBit bitmap x y = fromMaybe False $ (==) <$> bitmap !? x <*> bitmap !? y

makeUF :: UArray Coord Bool -> UnionFind Coord
makeUF bitmap =
  runST $ makeMutableUF bitmap >>= toUnionFind

-- TODO: This is very messy, probably a fold can make it clearer, or a better UF
-- interface with monadic behaviour
--
-- Coord is the coordinate of the element in focus
-- Unions consecutive, equal elements in a row, left to right
unionRightwards :: Int -> MutableUnionFind s Int -> Coord -> NonEmpty Bool -> ST s ()
unionRightwards columns uf coord (h :| t) =
  unionRightwards' columns uf coord h t

unionRightwards' :: Int -> MutableUnionFind s Int -> Coord -> Bool -> [Bool] -> ST s ()
unionRightwards' _columns _uf _coord _current [] = pure ()
unionRightwards' columns uf coord current (h:t)  = undefined
  -- let
  --   convert = toUFIndex columns
  -- in do
  --   when (current == h) $ union uf (convert coord) (convert $ columnRight coord)
  --   unionRightwards' columns uf (columnRight coord) h t

-- columnRight :: Coord -> Coord
-- columnRight (x, y) = (x + 1, y)

-- rowUp :: Coord -> Coord
-- rowUp (x, y) = (x, y - 1)

-- rowDown :: Coord -> Coord
-- rowDown (x, y) = (x, y + 1)

unionDownwards ::
  Int
  -> MutableUnionFind s Int
  -> Coord
  -> NonEmpty Bool
  -> [NonEmpty Bool]
  -> ST s ()
unionDownwards columns uf coord previousRow []             = pure ()
unionDownwards columns uf coord previousRow (currentRow:t) = undefined
  -- do
  --   unionRightwards columns uf coord currentRow
  --   unionRows columns uf coord previousRow currentRow
  --   unionDownwards columns uf (rowDown coord) currentRow t

unionRows ::
  Int
  -> MutableUnionFind s Int
  -> Coord
  -> NonEmpty Bool
  -> NonEmpty Bool
  -> ST s ()
unionRows columns uf coord previousRow currentRow = undefined
  -- let
  --   convert = toUFIndex columns
  --   zipped = mzip previousRow currentRow
  --   f coord' (prev, cur) =
  --     do
  --       when (prev == cur) $ union uf (convert  coord') (convert $ rowUp coord')
  --       return $ columnRight coord'
  -- in
  --   foldM_ f coord zipped

-- Useful for debugging. Prints a map where each root is printed for each
-- location in the original bitmap
printAreas :: Int -> Int -> UnionFind Int -> Text
printAreas rows columns uf =
  undefined
  -- let
  --   roots = find' uf <$> take (rows * columns) [0..]
  --   toChar = chr . (ord 'A' +)
  --   text = pack $ toChar <$> compress roots
  -- in
  --   intercalate "\n" $ chunksOf columns text


-- compress :: [Int] -> [Int]
-- compress roots =
--   let
--     addRep reps x = if member x reps
--                     then reps
--                     else insert x (size reps) reps
--     repMap = foldl' addRep Map.empty roots
--   in

--     (repMap !) <$> roots

-- FIXME This is a hack because our original parser did not return a bidim which
-- was a mistake We'll fix this in the parser, but that impacts how we create
-- the UnionFind, so we patch it for now
-- getBidim :: Text -> Bidim Bool
-- getBidim = undefined

-- solve :: Input -> Bidim Bool -> IO ()
-- solve input bidim =
--   let
--     uf = makeUF (rows input) (columns input) (bitmap input)
--     responses = query bidim uf <$> queriesToCoords (queries input)
--   in
--     traverse_ putStrLn responses

-- queriesToCoords :: [(Coord, Coord)] -> [(Coord, Coord)]
-- queriesToCoords = fmap $ over both swap

connected :: UnionFind Int -> Int -> (Coord, Coord) -> Bool
connected uf columns (x, y) = undefined
  -- let
  --   f coord' = find' uf (toUFIndex columns coord')
  -- in
  --   f x == f y

-- toColumns :: Bidim a -> Int
-- toColumns bidim =
--   let
--     (_, (x, y)) = view boundaries bidim
--   in
--     x + 1

-- query :: Bidim Bool -> UnionFind -> (Coord, Coord) -> Text
-- query bidim uf q@((r1, c1), (r2, c2)) =
--   if connected uf (toColumns bidim) q
--   then toText $ queryCoordToBit bidim (c1, r1)
--   else "neither"

toText :: Bool -> Text
toText True  = "decimal"
toText False = "binary"

-- The problem specifies coordinates as 1-based indexes like (row, column),
-- whereas our standard is 0-based x,y coordinates (y growing downwards in this
-- case, we should probably standardise that better)
-- queryCoordToBit :: Bidim Bool -> Coord -> Bool
-- queryCoordToBit bidim (r, c) =
--   let
--     coord = (c - 1, r - 1)
--   in
--     fromJust $ view (cell coord) bidim

-- TODO We'll get rid of this
fromRight' :: Either a b -> b
fromRight' (Right a) = a
fromRight' _         = error "Got left"

solve :: Text -> UnionFind Coord
solve textInput =
  let
    input = fromRight' $ parse parser textInput
  in
    makeUF $ bitmap input

main :: IO ()
main =
  do
    contents <- getContents
    solveIO contents

solveIO :: Text -> IO ()
solveIO contents = undefined
  -- do
  --   bidim <- unsafeParse hackParser contents
  --   input <- unsafeParse parser contents
  --   solve input bidim
