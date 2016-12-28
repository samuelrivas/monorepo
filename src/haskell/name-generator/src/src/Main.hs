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
type ConsSet = Set.Set String

data VowFlavour = Standard | Aiu | Eou
type VowSet = Set.Set String

data LetterType = Consonant
                | Sibilant
                | Liquid
                | Final
                | Vowel
data SyllableComponent = Optional LetterType | Mandatory LetterType

type LetterGen = LetterType -> Random.RVar String
type ComponentGen = SyllableComponent -> Random.RVar String
type SyllableStruct = [SyllableComponent]

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

sibilants :: ConsSet
sibilants = Set.fromList ["s", "sh", "f"]

liquids :: ConsSet
liquids = Set.fromList ["l", "r", "w", "j"]

finals:: ConsFlavour -> ConsSet
finals flavour =
  let selection = Set.fromList ["m", "n", "s", "k", "ng", "sh", "zh"]
  in Set.intersection (consonant_set flavour) selection

vowel_set :: VowFlavour -> VowSet
vowel_set flavour =
  let strings Standard = ["a", "e", "i", "o", "u"]
      strings Aiu      = ["a", "i", "u"]
      strings Eou      = ["e", "o", "u"]
  in
    Set.fromList $ strings flavour

draw_from_set :: Set.Set a -> Random.RVar a
draw_from_set set = Random.randomElement $ Set.toList set

random_syllable :: SyllableStruct -> ComponentGen -> Random.RVar [String]
random_syllable syllableStruct componentGen =
  sequence (componentGen <$> syllableStruct)

basic_letter_gen :: LetterGen
basic_letter_gen Consonant = draw_from_set (consonant_set Arabic)
basic_letter_gen Vowel     = draw_from_set (vowel_set Standard)
basic_letter_gen Sibilant  = draw_from_set sibilants
basic_letter_gen Liquid    = draw_from_set liquids
basic_letter_gen Final     = draw_from_set (finals Arabic)

basic_component_gen :: ComponentGen
basic_component_gen (Optional letter_type) =
  let empty_if_false False = return ""
      empty_if_false True  = basic_component_gen (Mandatory letter_type)
  in
    Random.sample (Random.Uniform True False) >>= empty_if_false
basic_component_gen (Mandatory letter_type) = basic_letter_gen letter_type

basic_syllable_struct :: SyllableStruct
basic_syllable_struct = [Mandatory Sibilant,
                         Optional Consonant,
                         Mandatory Vowel,
                         Mandatory Final]
main :: IO ()
main =
  let struct = basic_syllable_struct
      gen    = basic_component_gen
  in do
    syllable <- Random.sample $ random_syllable struct gen
    SIO.hSetEncoding SIO.stdout Encoding.utf8
    putStrLn $ List.intercalate "-" syllable
