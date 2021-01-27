{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day18 where

import           Advent.Perlude        as Perlude
import qualified Prelude               (show)

import           Data.Foldable         (foldl')
import           Data.Functor          (($>))
import           Text.Parsec           (char, many, sepEndBy, spaces, (<?>),
                                        (<|>))

import           Advent.Templib        (Day (..), getInput', getParsedInput)
import           Advent.Templib.Parsec (Parser, digitsAsNum)

day :: Day
day = D18

data Op = Plus
    | Times
    deriving stock (Eq)

data Tok = Digit Int
    | Open
    | Close
    | Operator Op
    deriving stock (Eq)

instance Perlude.Show Tok where
  show Open         = "("
  show Close        = ")"
  show (Operator c) = Prelude.show c
  show (Digit d)    = Prelude.show d

instance Perlude.Show Op where
  show Plus  = "+"
  show Times = "*"

example :: Text
example = "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"

getInput :: IO Text
getInput = getInput' D18

parser :: Parser [[Tok]]
parser = many parseToken `sepEndBy` char '\n'

parseToken :: Parser Tok
parseToken =
      (char '(' <* spaces) $> Open
  <|> (char ')' <* spaces) $> Close
  <|> parseValue <* spaces
  <|> parseOperator <* spaces
  <?> "digit or operator"

parseValue :: Parser Tok
parseValue = Digit <$> digitsAsNum

parseOperator :: Parser Tok
parseOperator =
  let op = (char '*' $> Times) <|> (char '+' $> Plus)
  in Operator <$> op

precedence1 :: Tok -> Tok -> Bool
precedence1 _ Open = False
precedence1 _ _    = True

precedence2 :: Tok -> Tok -> Bool
precedence2 _ Open                           = False
precedence2 (Operator Plus) (Operator Times) = False
precedence2 _ _                              = True

toPostfix :: [Tok] -> [Tok]
toPostfix = generalisedToPostfix precedence1

-- TODO: This is ugly and unreadable, make it pretty and readable

-- a `precedenceF` b returns whether a has lower precedence than b
generalisedToPostfix :: (Tok -> Tok -> Bool) -> [Tok] -> [Tok]
generalisedToPostfix precedenceF expr =
  let
    f (stack, acc) d@(Digit _) = (stack, acc ++ [d])
    f (stack, acc) op@(Operator _) =
      (op : dropWhile (precedenceF op) stack, acc ++ takeWhile (precedenceF op) stack)
    f (stack, acc) Close =
      (tail . dropWhile (/= Open) $ stack, acc ++ takeWhile (/= Open) stack)
    f (stack, acc) Open = (Open : stack, acc)

    (finalStack, finalAcc) = foldl' f ([], []) expr

  in finalAcc ++ finalStack

eval :: [Tok] -> Int
eval =
  let
    f stack (Digit d)                 = d : stack
    f (a : b : stack) op@(Operator _) = operate op a b : stack
    f _ _                             = undefined
  in
    head . foldl' f []

operate :: Tok -> Int -> Int -> Int
operate (Operator Plus)  = (+)
operate (Operator Times) = (*)
operate _                = undefined

solution1 :: [[Tok]] -> Int
solution1 = sum . fmap (eval . toPostfix)

solution2 :: [[Tok]] -> Int
solution2 = sum . fmap (eval . generalisedToPostfix precedence2)

main :: IO ()
main = do
  input <- getParsedInput day parser

  putStr "Solution 1: "
  print . solution1 $ input

  putStr "Solution 2: "
  print . solution2 $ input
