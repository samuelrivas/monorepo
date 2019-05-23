import qualified Control.Monad                    as Control
import qualified Data.Char                        as Char
import qualified Data.List                        as List
import qualified Data.Random                      as Random
import qualified Data.Random.Distribution.Poisson as Poisson
import qualified Data.Set                         as Set
import qualified System.IO                        as SIO

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

data VowFlavour = Standard
                | Aiu
                | Eou
                | ExtraAei
                | ExtraU
                | FiveAi
                | ExtraAou
type VowSet = Set.Set String

data LetterType = Consonant
                | Sibilant
                | Liquid
                | Final
                | Vowel
data SyllableComponent = Optional LetterType | Mandatory LetterType

type LetterGen = LetterType -> Random.RVar String
type ComponentGen = SyllableComponent -> Random.RVar String
type SyllableGen = Random.RVar [String]
type LengthGen = Random.RVar Int
type SyllableStruct = [SyllableComponent]

data ConsonantOrthography = Slavic
                          | German
                          | French
                          | Chinese

data VowelOrthography = Acutes
                      | Umlauts
                      | Welsh
                      | Diphthongs
                      | Doubles

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
  let strings vowset =
        case vowset of
         Standard -> ["a", "e", "i", "o", "u"]
         Aiu      -> ["a", "i", "u"]
         Eou      -> ["e", "o", "u"]
         ExtraAei -> ["a", "e", "i", "o", "u", "A", "E", "I"]
         ExtraU   -> ["a", "e", "i", "o", "u", "U"]
         FiveAi   -> ["a", "i", "u", "A", "I"]
         ExtraAou -> ["a", "e", "i", "o", "u", "A", "O", "U"]
  in
    Set.fromList $ strings flavour

draw_from_set :: Set.Set a -> Random.RVar a
draw_from_set set = Random.randomElement $ Set.toList set

maybe_invalid_syllable :: SyllableStruct -> ComponentGen -> Random.RVar [String]
maybe_invalid_syllable syllableStruct componentGen =
  sequence (componentGen <$> syllableStruct)

-- This can hang if the distribution doesn't offer enough chances for valid
-- syllables
random_syllable :: SyllableStruct -> ComponentGen -> SyllableGen
random_syllable syllableStruct componentGen =
  let to_maybe f x = if f x then Just x else Nothing
      maybe_syllable = to_maybe valid_syllable <$>
        maybe_invalid_syllable syllableStruct componentGen
  in repeat_until_just maybe_syllable

repeat_until_just :: Random.RVar (Maybe a) -> Random.RVar a
repeat_until_just action =
  action >>= maybe (repeat_until_just action) return

basic_letter_gen :: LetterGen
basic_letter_gen Consonant = draw_from_set (consonant_set English)
basic_letter_gen Vowel     = draw_from_set (vowel_set ExtraU)
basic_letter_gen Sibilant  = draw_from_set sibilants
basic_letter_gen Liquid    = draw_from_set liquids
basic_letter_gen Final     = draw_from_set (finals Piraha)

basic_component_gen :: ComponentGen
basic_component_gen (Optional letter_type) =
  let empty_if_false False = return ""
      empty_if_false True  = basic_component_gen (Mandatory letter_type)
  in
    Random.sample (Random.Uniform True False) >>= empty_if_false
basic_component_gen (Mandatory letter_type) = basic_letter_gen letter_type

basic_syllable_struct :: SyllableStruct
basic_syllable_struct = [Mandatory Consonant,
                         Optional Liquid,
                         Mandatory Vowel,
                         Optional Final]

bigrams :: [String] -> [(String, String)]
bigrams (x:y:t) = (x, y) : (bigrams (y:t))
bigrams _       = []

valid_syllable :: [String] -> Bool
valid_syllable syllable =
  let filtered = filter (/= "") syllable
  in List.all valid_bigram (bigrams filtered)

hard_bigrams :: Set.Set (String, String)
hard_bigrams =
  let cartesian_prod xs ys = Set.fromList [(x, y) | x <- xs, y <- ys]
      shf = cartesian_prod ["s", "sh", "f"] ["s", "sh"]
      rl = cartesian_prod ["r", "l"] ["r", "l"]
  in
    Set.union shf rl

valid_bigram :: (String, String) -> Bool
valid_bigram bigram@(x, y) = (x /= y) && (not $ Set.member bigram hard_bigrams)

-- TODO Consnants and vowels should be separate types so that we can easily map
-- both ortographies
spell :: ConsonantOrthography -> VowelOrthography -> String -> String
spell Slavic _ "sh"    = "š"
spell Slavic _ "zh"    = "ž"
spell Slavic _ "ch"    = "č"
spell Slavic _ "gh"    = "ǧ"

spell German _ "sh"    = "sch"
spell German _ "ch"    = "tsch"
spell German _ "x"     = "ch"

spell French _ "sh"    = "ch"
spell French _ "zh"    = "j"
spell French _ "ch"    = "tch"
spell French _ "x"     = "kh"

spell Chinese _ "sh"   = "x"
spell Chinese _ "ch"   = "q"
spell Chinese _ "gh"   = "j"

spell _ Acutes "A"     = "á"
spell _ Acutes "E"     = "é"
spell _ Acutes "I"     = "í"
spell _ Acutes "O"     = "ó"
spell _ Acutes "U"     = "ú"

spell _ Umlauts "A"    = "ä"
spell _ Umlauts "E"    = "ë"
spell _ Umlauts "I"    = "ï"
spell _ Umlauts "O"    = "ö"
spell _ Umlauts "U"    = "ü"

spell _ Welsh "A"      = "â"
spell _ Welsh "E"      = "ê"
spell _ Welsh "I"      = "î"
spell _ Welsh "O"      = "ô"
spell _ Welsh "U"      = "û"

spell _ Diphthongs "A" = "au"
spell _ Diphthongs "E" = "ei"
spell _ Diphthongs "I" = "ie"
spell _ Diphthongs "O" = "ou"
spell _ Diphthongs "U" = "uo"

spell _ Doubles "A"    = "aa"
spell _ Doubles "E"    = "ee"
spell _ Doubles "I"    = "ii"
spell _ Doubles "O"    = "oo"
spell _ Doubles "U"    = "uu"

spell _ _ "tt"         = "'"
spell _ _ "x"          = "kh"
spell _ _ "j"          = "y"
spell _ _ x            = x

poisson_length_gen :: LengthGen
poisson_length_gen = (+1) <$> Poisson.poisson (1::Double)

word_gen :: SyllableGen -> LengthGen -> Random.RVar [[String]]
word_gen syllableGen lengthGen =
  do
    len <- Random.sample lengthGen
    Random.sample $ Control.replicateM len syllableGen
    -- Random.sample $ sequence (replicate len syllableGen)

format_syllable :: [String] -> String
format_syllable = List.intercalate "-"

format_word :: [[String]] -> String
format_word syllables = List.intercalate "/" (format_syllable <$> syllables)

print_word :: ConsonantOrthography -> VowelOrthography -> [[String]] -> String
print_word cons_orth vowel_orth =
  capitalise . concatMap (concatMap (spell cons_orth vowel_orth))

capitalise :: String -> String
capitalise ""    = ""
capitalise (h:t) = Char.toUpper h : t

main :: IO ()
main =
  let struct = basic_syllable_struct
      gen = basic_component_gen
      syllable = random_syllable struct gen
      syllable_length = poisson_length_gen
      consonant_orthography = Slavic
      vowel_orthography = Doubles

  in do
    word <- Random.sample $ word_gen syllable syllable_length
    SIO.hSetEncoding SIO.stdout SIO.utf8
    putStrLn $ format_word word
    putStrLn $ print_word consonant_orthography vowel_orthography word
