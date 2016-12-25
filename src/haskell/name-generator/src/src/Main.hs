import qualified Data.Random as Random
import qualified Data.Set    as Set

{-# ANN module "HLint: ignore Use camelCase" #-}

simple_consonant_set :: Set.Set Char
simple_consonant_set = Set.fromList ['p', 't', 'k', 'm', 'n', 's', 'l']

simple_vowel_set :: Set.Set Char
simple_vowel_set = Set.fromList ['a', 'e', 'i', 'o', 'u']

draw_from_set :: Set.Set a -> Random.RVar a
draw_from_set set = Random.randomElement $ Set.toList set

random_syllable :: [Random.RVar Char] -> Random.RVar [Char]
random_syllable random_chars = sequence (Random.sample <$> random_chars)

main :: IO ()
main =
  let consonant = draw_from_set simple_consonant_set
      vowel = draw_from_set simple_vowel_set
  in do
    syllable <- Random.sample $ random_syllable [consonant, vowel, consonant]
    putStrLn syllable
