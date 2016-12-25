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

random_syllable :: [Random.RVar Char] -> Random.RVar [Char]
random_syllable random_chars = sequence (Random.sample <$> random_chars)

main :: IO ()
main = do
  vowel <- Random.sample $ random_syllable [random_consonant, random_vowel, random_consonant]
  putStrLn vowel
