{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day13 where

import           Perlude

import           Advent.Templib       (Day (..), getInput', getParsedInput)
import           Control.Lens         (_1, _2, over, preview, view)
import           Data.Functor         (($>))
import           Data.List            (all, elemIndex, find, maximumBy)
import           Data.Maybe           (fromJust, mapMaybe)
import           Text.Parsec          (char, sepBy, (<|>))
import           Text.Parsec.Parselib (digitsAsNum)
import           Text.Parsec.Text     (Parser)

-- TODO: Write the solution for this, it was solved in the repl, hastily

day :: Day
day = D13

example :: Text
example = "foo"

getInput :: IO Text
getInput = getInput' day

parser :: Parser [(Int, Int)]
parser = toSchedule . view _2 <$> parseInput

toSchedule :: [Maybe Int] -> [(Int, Int)]
toSchedule l =
  let f (x :: (Maybe Int, Int)) = (,) <$> view _1 x <*> preview _2 x
  in mapMaybe f $ zip l [0..]

parseInput :: Parser (Int, [Maybe Int])
parseInput = (,) <$> (digitsAsNum <* char '\n') <*> (parseSchedule <* char '\n')

parseSchedule :: Parser [Maybe Int]
parseSchedule =
  let bus = (Just <$> digitsAsNum) <|> (char 'x' $> Nothing)
  in bus `sepBy` char ','

-- TODO: We can remove this now, as we have an actual parser
buses :: [Int]
buses = [13, 41, 37, 419, 19, 23, 29, 421, 17]

shifts :: [Int]
shifts = [0, 3, 7, 13, 32, 36, 42, 44, 61]

input :: [(Int, Int)]
input = [(13,0),(41,3),(37,7),(419,13),(19,32),(23,36),(29,42),(421,44),(17,61)]

exampleBuses :: [Int]
exampleBuses = [1789,37,47,1889]

exampleShift :: [Int]
exampleShift = [0..3]

exampleSchedule :: [(Int, Int)]
exampleSchedule = zip exampleBuses exampleShift

takeeach :: Int -> [a] -> [a]
takeeach n (h:t) = h : (takeeach n $ drop (n - 1) t)


example2 :: [(Int, Int)]
example2 = [(17, 0), (13, 2), (19, 3)]

example3 :: [(Int, Int)]
example3 = [(67, 0), (7, 1), (59, 2), (61, 3)]

example4 :: [(Int, Int)]
example4 = [(67, 0), (7, 2), (59, 3), (61, 4)]

example5 :: [(Int, Int)]
example5 = [(67, 0), (7, 1), (59, 3), (61, 4)]

-- (bus, shift) -> infinite list of candidates for the timestamp for the first bus)
candidates :: [(Int, Int)] -> [Int]
candidates schedule =
  let (bus, shift) = maximumBy (\x y -> fst x `compare` fst y) schedule
  in (+ (-shift)) . (* bus) <$> [0..]

coprime :: [Int] -> Int -> Bool
coprime factors x = notElem 0 $ (x `mod`) <$> factors

candidates2 :: [(Int, Int)] -> [Int]
candidates2 schedule =
  let
    (_bus1, 0) = head schedule
    primes = view _1 <$> tail schedule
  in
    filter (coprime primes) $ candidates schedule

candidates3 multiplier busId shift =
  let
    newSeq = (`mod` busId) <$> (multiplier *) <$> [0..]
    solIndex = fromJust $ elemIndex (busId - (shift `mod` busId)) newSeq
  in
    solIndex * multiplier

solveConstrain base multiplier busId shift =
  let
    previousSolutions = (+ base) . (* multiplier) <$> [0..]
    targetValue = busId - (shift `mod` busId)
  in do
    previousPasses <- elemIndex targetValue $ (`mod` busId) <$> previousSolutions
    pure $ previousPasses * multiplier
    pure $ take 10 previousSolutions

step previousSol multiplier busId shift =
  let
    candidates = take busId $ (+ previousSol) . (* multiplier) <$> [0..]
    targetValue = busId - (shift `mod` busId)
    test = (== targetValue) . (`mod` busId)
  in
    (find test candidates, candidates, targetValue)

test :: [(Int, Int)] -> Int -> Bool
test schedule candidate =
  let mods = uncurry (flip mod) <$> over (traverse . _2) (+ candidate) schedule
  in all (== 0) mods

-- This solves up to 6 buses, then chokes
solve2 :: [(Int, Int)] -> Int
solve2 schedule = head . dropWhile (not . test schedule) . candidates $ schedule

-- We are using the idea that if N is a solution for buses b1, ..., bn, then n + (b1*b2*...*bn) is also a solution for that. And since all buses are primes, then that sequence modulo any bus (including b(n+1)) generates a new sequence with b elements, so we can just find the solution for b(n+1) in that sequence

-- solve :: [(Int, Int)] -> Maybe Int
-- solve =
--   let
--     f (previousSolution, mult) (bus, shift) =
--       let
--         newSolution = step previousSolution mult bus shift
--       in
--         (newSolution, bus*mult)
--   in foldl'


main :: IO ()
main = do
  input <- getParsedInput day parser

  putStr "Solution 1: "
  print $ "NA"

  putStr "Solution 2: "
  print . solve2 $ zip buses shifts
