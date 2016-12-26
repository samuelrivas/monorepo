import qualified Data.List          as List
import qualified Data.Random        as Random
import qualified Data.Set           as Set
import qualified System.IO          as SIO
import qualified System.IO.Encoding as Encoding

{-# ANN module "HLint: ignore Use camelCase" #-}

data ConsFlavour = Minimal
                 | English
                 | Piraha
                 | Hawaiian
                 | Greenland
                 | Arabic
                 | ArabicLite
                 | EnglishLite
data VowFlavour = Standard | Aiu | Eou
type ConsSet = Set.Set String
type VowSet = Set.Set String

consonant_set :: ConsFlavour -> ConsSet
consonant_set flavour =
  let strings Minimal =
        ["p", "t", "k", "m", "n", "s", "l"]
      strings English =
        ["p", "t", "k", "b", "d", "g", "m",
         "n", "l", "r", "s", "sh", "z", "zh", "ch"]
      strings Piraha =
        ["p", "t", "k", "m", "n", "h"]
      strings Hawaiian =
        ["h", "k", "l", "m", "n", "p", "w", "tt"]
      strings Greenland =
        ["p", "t", "k", "q", "v", "s", "g", "r", "m", "n", "ng", "l", "j"]
      strings Arabic =
        ["t", "k", "s", "sh", "d", "b", "q", "gh",
         "x", "m", "n", "l", "r", "w", "j"]
      strings ArabicLite =
        ["t", "k", "d", "g", "m", "n", "s", "sh"]
      strings EnglishLite =
        ["p", "t", "k", "b", "d", "g", "m", "n", "s", "z",
         "zh", "ch", "h", "j", "w"]
  in
    Set.fromList $ strings flavour

vowel_set :: VowFlavour -> VowSet
vowel_set flavour =
  let strings Standard = ["a", "e", "i", "o", "u"]
      strings Aiu      = ["a", "i", "u"]
      strings Eou      = ["e", "o", "u"]
  in
    Set.fromList $ strings flavour

draw_from_set :: Set.Set a -> Random.RVar a
draw_from_set set = Random.randomElement $ Set.toList set

random_syllable :: [Random.RVar String] -> Random.RVar [String]
random_syllable random_chars = sequence (Random.sample <$> random_chars)

main :: IO ()
main =
  let consonant = draw_from_set $ consonant_set Arabic
      vowel     = draw_from_set $ vowel_set Standard
      structure = [consonant, vowel, consonant]
  in do
    syllable <- Random.sample $ random_syllable structure
    SIO.hSetEncoding SIO.stdout Encoding.utf8
    putStrLn $ List.intercalate "-" syllable
