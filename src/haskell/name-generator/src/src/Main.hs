import qualified Data.Random as Random
import qualified Data.Set    as Set

{-# ANN module "HLint: ignore Use camelCase" #-}

consonants :: Set.Set Char
consonants = Set.fromList ['p', 't', 'k', 'm', 'n', 's', 'l']

random_consonant :: Random.RVar Char
random_consonant = Random.randomElement $ Set.toList consonants

vowels :: Set.Set Char
vowels = Set.fromList ['a', 'e', 'i', 'o', 'u']

random_vowel :: Random.RVar Char
random_vowel = Random.randomElement $ Set.toList vowels

main :: IO ()
main = do
  c1 <- Random.sample random_consonant
  v <- Random.sample random_vowel
  c2 <- Random.sample random_consonant
  putStrLn [c1, v, c2]
