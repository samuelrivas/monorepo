-- These are for hacking around, remember not to commit them
-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE ScopedTypeVariables   #-}

import           Prelude                    hiding (getLine, head, last, print,
                                             putStr)

import           Control.Applicative        ((<|>))
import           Control.Lens               (Lens', assign, lens, modifying,
                                             over, set, toListOf, view)
import           Control.Monad              (forever, guard, unless, when)
import           Control.Monad.Fail         (MonadFail)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Loops        (whileM_)
import           Control.Monad.Reader       (ask, asks)
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Monad.State.Class  (MonadState, get, gets, modify, put)
import           Control.Monad.State.Lazy   (execStateT)
import           Control.Monad.Trans.Maybe  (MaybeT, runMaybeT)
import           Data.Foldable              (fold, traverse_)
import           Data.Generics.Labels       ()
import           Data.List                  (nub, permutations, sort)
import           Data.Maybe                 (fromMaybe)
import           Data.MultiSet              (MultiSet, delete, distinctElems,
                                             empty, insert, member, occur)
import           Data.Random                (MonadRandom, RVar, sample, shuffle)
import           Game
import           GHC.Generics               (Generic)
import           Util                       (addHistory, head, last, print,
                                             readline, uncons)

{-# ANN module "HLint: ignore Use camelCase" #-}

data Colour = Red | Blue | Green | White
  deriving (Show, Eq, Ord)

data Location = Key Colour | Sun Colour | Moon Colour
  deriving stock (Eq, Ord)

instance Show Location where
  show (Key c)  = "K" <> concise_show c
  show (Sun c)  = "S" <> concise_show c
  show (Moon c) = "M" <> concise_show c

data Dream = Door Colour | Nightmare
  deriving stock (Ord, Eq)

data Card = Location Location | Dream Dream
  deriving stock (Ord, Eq)

instance Show Dream where
  show Nightmare = "N"
  show (Door c)  = "D" <> concise_show c

instance Show Card where
  show (Location l) = show l
  show (Dream d)    = show d

concise_show :: Colour -> String
concise_show = head . show

colour :: Lens' Location Colour
colour = lens get_colour set_colour

get_colour :: Location -> Colour
get_colour = \case
  Key c -> c
  Sun c -> c
  Moon c -> c

set_colour :: Location -> Colour -> Location
set_colour (Key _) c  = Key c
set_colour (Sun _) c  = Sun c
set_colour (Moon _) c = Moon c

matching_symbols :: Location -> Location -> Bool
matching_symbols (Key _) (Key _)   = True
matching_symbols (Sun _) (Sun _)   = True
matching_symbols (Moon _) (Moon _) = True
matching_symbols _ _               = False

separate_types :: [Card] -> ([Location], [Dream])
separate_types cards =
  let
    separate_types_acc [] acc =
      acc
    separate_types_acc (Location l : rest) (ls, ds) =
      separate_types_acc rest (l:ls, ds)
    separate_types_acc (Dream d : rest) (ls, ds) =
      separate_types_acc rest (ls, d:ds)
    (ls', ds') = separate_types_acc cards ([], [])
  in
    (reverse ls', reverse ds')

is_key :: Location -> Bool
is_key (Key _) = True
is_key _       = False

is_door :: Card -> Bool
is_door (Dream (Door _)) = True
is_door _                = False

data Status =
    Uninitialised
  | Placing
  | Prophecy
  | SolvingNightmare
  | SolvingDoor Colour
  | Lost
  | Won
  deriving (Show, Eq)

-- FIXME: Move to a data module, hide accessors and rename to remove underscore
data Labirynth = Labirynth
  { _current :: [Location],
    _past    :: [Location]
  } deriving stock Generic

-- FIXME: Move to a data module, hide accessors and rename to remove underscore
data OnirimState = OnirimState
  { osDoors     :: MultiSet Colour,
    osDeck      :: [Card],
    osLabirynth :: Labirynth,
    osDiscards  :: [Card],
    osHand      :: [Location],
    osLimbo     :: [Dream],
    osStatus    :: Status
  } deriving stock Generic

-- FIXME: Rearrange could be safer...
data OnirimTransition =
    InitialSetup
  | Discard Location
  | Place Location
  | OpenDoor Colour
  | IgnoreDoor Colour
  | DiscardHand
  | DiscardKey Colour
  | CloseDoor Colour
  | Discard5
  | Rearrange [Card]
  deriving Show

showDoors :: MultiSet Colour -> String
showDoors doors =
  let
    showColour c = case occur c doors of
      0 -> "XX"
      1 -> "X" <> concise_show c
      _ -> concise_show c <> concise_show c
  in
    unwords . fmap showColour $ [Red, Blue, Green, White]

instance GameState OnirimState OnirimTransition Bool where
  next_state = next_onirim_state
  transitions = onirim_transitions
  score = (Won ==) <$> asks osStatus

instance Show Labirynth where
  show l = show_cards (view #_past l) <> "|"
    <> show_cards (view #_current l)

instance Show OnirimState where
  show s =
    unlines
    ["Doors: " <> showDoors (view #osDoors s),
     "Labirynth: " <> show (view #osLabirynth s),
     "Deck: " <> show_cards (view #osDeck s),
     "Hand: " <> show_cards (view #osHand s),
     "Discards: " <> show_cards (view #osDiscards s),
     "Limbo: " <> show_cards (view #osLimbo s),
     "Status: " <> show (view #osStatus s)
    ]

show_cards :: Show a => [a] -> String
show_cards = unwords . fmap show

initial_onirim_state :: OnirimState
initial_onirim_state =
  OnirimState
    empty
    []
    (Labirynth [] [])
    []
    []
    []
    Uninitialised

all_colours :: [Colour]
all_colours = [Red, Blue, Green, White]

dreams :: [Dream]
dreams =
  fold
  [ Door <$> all_colours,
    Door <$> all_colours,
    replicate 10 Nightmare
  ]

locations :: [Location]
locations =
  fold
  [ replicate 9 (Sun Red),
    replicate 8 (Sun Blue),
    replicate 7 (Sun Green),
    replicate 6 (Sun White),
    Moon <$> all_colours,
    Moon <$> all_colours,
    Moon <$> all_colours,
    Moon <$> all_colours,
    Key <$> all_colours,
    Key <$> all_colours,
    Key <$> all_colours
  ]

-- FIXME: How to verify that this is in sync with next_state?
onirim_transitions :: (MonadReader OnirimState m) => m [OnirimTransition]
onirim_transitions = do
  status <- asks osStatus
  hand <- asks osHand
  doors <- asks osDoors
  deck <- asks osDeck
  case status of
    Uninitialised    -> return [InitialSetup]
    SolvingDoor c    -> return [OpenDoor c, IgnoreDoor c]
    SolvingNightmare ->
      return . concat $
      [ discard_key hand,
        close_door doors,
        discard_5 deck,
        [DiscardHand]
      ]
    -- FIXME: filter place to only those that can be placed
    Placing            -> return $ (Discard <$> hand) ++ (Place <$> hand)
    Won                -> return []
    Lost               -> return []
    Prophecy           -> prophecy_transitions

prophecy_transitions :: (MonadReader OnirimState m) => m [OnirimTransition]
prophecy_transitions =
  let
    valid l = case reverse l of
      (c : _) -> not . is_door $ c
      _       -> True
  in do
    hand <- asks . view $ #osDeck
    if null hand then
      return []
    else
      return $ Rearrange <$> (filter valid . permutations . take 5 $ hand)

discard_key :: [Location] -> [OnirimTransition]
discard_key = fmap DiscardKey . nub . fmap get_colour . filter is_key

discard_5 :: [Card] -> [OnirimTransition]
discard_5 deck = guard (length deck >= 5) >> [Discard5]

close_door :: MultiSet Colour -> [OnirimTransition]
close_door = fmap CloseDoor . distinctElems

-- FIXME: We need to make it possible for StateDistribution to signal failure to
-- sample, otherwise we need the runMaybeT below, which hides errors (sampling a
-- failure just leaves the state intact)

-- FIXME: There is a lot of boilerplate in these functions, try to find better
-- abstractions
next_onirim_state ::
     MonadReader OnirimState m
  => MonadFail m
  => OnirimTransition
  -> m (StateDistribution OnirimState)
next_onirim_state InitialSetup = do
  assert_status Uninitialised
  state <- ask
  return . Stochastic $ do
    (hand, deck) <- initial_hand_and_deck
    return $ state
      { osHand = hand,
        osDeck = deck,
        osStatus = Placing
      }

next_onirim_state (Discard location) = do
  assert_status Placing
  Just hand <- remove_location location <$> asks osHand
  discards <- (Location location :) <$> asks osDiscards
  state <- ask

  let new_state = set #osHand hand $
                  set #osDiscards discards
                  state

  if is_key location then
    return . Deterministic $ set #osStatus Prophecy new_state
  else
    return . Stochastic $
    flip execStateT state $ runMaybeT $ do
      put new_state
      draw

-- FIXME: Open the doors when possible here
-- FIXME: Win the game when opening the last door
next_onirim_state (Place location) = do
  assert_status Placing
  Just hand <- remove_location location <$> asks osHand
  (labirynth, doorM) <- place location
  state <- ask
  return . Stochastic $
    flip execStateT state $ runMaybeT $ do
      assign #osHand hand
      assign #osLabirynth labirynth

      open_door_M doorM
      draw

next_onirim_state (OpenDoor c) = do
  assert_status $ SolvingDoor c
  Just hand <- remove_location (Key c) <$> asks osHand
  doors <- insert c <$> asks osDoors
  state <- ask
  return . Stochastic $
    flip execStateT state $ runMaybeT $ do
      put $ state
        { osDoors = doors,
          osHand = hand,
          osStatus = Placing
        }
      draw

next_onirim_state (IgnoreDoor c) = do
  assert_status $ SolvingDoor c
  state <- ask
  return . Stochastic $
    flip execStateT state $ runMaybeT $ do
      modifying #osLimbo (Door c :)
      assign #osStatus Placing
      draw

next_onirim_state (CloseDoor c) = do
  assert_status SolvingNightmare
  doors <- asks osDoors
  unless (c `member` doors) $ fail "cannot close a door you didn't open"
  state <- ask
  return . Stochastic $
    flip execStateT state $ runMaybeT $ do
      modifying #osLimbo (Door c :)
      modifying #osDoors $ delete c
      modifying #osDiscards (Dream Nightmare :)
      assign #osStatus Placing
      draw

next_onirim_state (DiscardKey c) = do
  assert_status SolvingNightmare
  Just hand <- remove_location (Key c) <$> asks osHand
  state <- ask
  return . Stochastic $
    flip execStateT state $ runMaybeT $ do
      assign #osHand hand
      assign #osStatus Placing
      modifying #osDiscards ([Dream Nightmare, Location (Key c)] ++)
      draw

next_onirim_state DiscardHand = do
  assert_status SolvingNightmare
  hand <- asks osHand
  discards <- asks osDiscards
  state <- ask
  return . Stochastic $
    flip execStateT state $ runMaybeT $ do
      put $ state
        { osHand = [],
          osStatus = Placing,
          osDiscards = Dream Nightmare : (Location <$> hand) ++ discards
        }
      restore_hand

next_onirim_state Discard5 = do
  assert_status SolvingNightmare
  (to_discard, rest) <- splitAt 5 <$> asks osDeck

  when (length to_discard /= 5) $
    fail "Cannot discard 5, the deck doesn't have enough cards"

  discards <- asks osDiscards
  limbo <- asks osLimbo
  state <- ask

  let
    (ls, ds) = separate_types to_discard
  return . Stochastic $
    flip execStateT state $ runMaybeT $ do
      put $ state
        { osStatus = Placing,
          osDeck = rest,
          osDiscards = Dream Nightmare : ((Location <$> ls) ++ discards),
          osLimbo = ds ++ limbo
        }
      draw

next_onirim_state (Rearrange cards) = do
  assert_status Prophecy
  state <- ask
  assert_same_cards cards (take 5 $ view #osDeck state)

  return . Stochastic $
    flip execStateT state $ runMaybeT $ do
      to_discard <- last cards
      modifying #osDeck ((init cards ++) . drop (length cards))
      modifying #osDiscards (to_discard :)
      assign #osStatus Placing
      draw

open_door_M :: MonadRandom m => MonadState OnirimState m => Maybe Colour -> m ()
open_door_M Nothing = pure ()
open_door_M (Just c) = do
  doors <- gets osDoors
  when (occur c doors < 2) $
    modifying #osDoors (insert c)
  shuffle_cards

assert_status :: MonadFail m => MonadReader OnirimState m => Status -> m ()
assert_status expected = do
  status <- asks osStatus
  unless (expected == status) $
    fail $ "Must be in " <> show expected <> " but is in " <> show status

assert_same_cards :: MonadFail m => [Card] -> [Card] -> m ()
assert_same_cards x y =
  unless (sort x == sort y) (fail "Rearranging illegal cards in prophecy")

remove_location :: Location -> [Location] -> Maybe [Location]
remove_location card cards =
  case break (card ==) cards of
    (before, _ : after) -> Just $ before ++ after
    _                   -> Nothing

initial_hand_and_deck :: RVar ([Location], [Card])
initial_hand_and_deck = do
  (hand, rest) <- splitAt 5 <$> shuffle locations

  when (length hand /= 5) $ fail "Not enough location cards in deck"

  let
    location_cards = Location <$> rest
    dream_cards = Dream <$> dreams

  reshuffled <- shuffle $ location_cards ++ dream_cards
  return (hand, reshuffled)

shuffle_cards ::
     MonadState OnirimState m
  => MonadRandom m
  => m ()
shuffle_cards = do
  deck <- gets osDeck
  limbo <- gets osLimbo
  shuffled <-  sample . shuffle $ deck ++ (Dream <$> limbo)
  modify $ \s -> s { osDeck = shuffled, osLimbo = [] }

pick_top ::
     MonadState OnirimState m
  => MonadFail m
  => m Card
pick_top = do
  (top, rest) <- gets osDeck >>= uncons
  modify $ \s -> s { osDeck = rest }
  return top

-- FIXME: Go into lost if cannot draw
-- Fill the hand up to 5, stopping if hitting actionable dreams
draw ::
     MonadState OnirimState m
  => MonadRandom m
  => MonadFail m
  => m ()
draw = do
  top <- pick_top
  hand <- gets osHand
  case top of
    Location location -> do
      let
        new_hand = location : hand

      modify $ \s -> s
        { osHand = new_hand,
          osStatus = Placing
        }

      if length new_hand < 5
        then draw
        else reshuffle_limbo

    Dream Nightmare -> modify $ \s -> s { osStatus = SolvingNightmare }

    Dream (Door c) -> solve_door c

restore_hand ::
     MonadState OnirimState m
  => MonadRandom m
  => MonadFail m
  => m ()
restore_hand = do
  whileM_ ((<5) . length <$> gets osHand) pick_location
  reshuffle_limbo

pick_location ::
     MonadState OnirimState m
  => MonadRandom m
  => MonadFail m
  => m ()
pick_location = do
  top <- pick_top
  hand <- gets osHand
  limbo <- gets osLimbo
  case top of
    Location location -> modify $ \s -> s { osHand = location : hand }
    Dream dream       -> modify $ \s -> s { osLimbo = dream : limbo }

reshuffle_limbo ::
     MonadState OnirimState m
  => MonadRandom m
  => m ()
reshuffle_limbo = do
  limbo <- gets osLimbo
  unless (null limbo) shuffle_cards

solve_door ::
     MonadState OnirimState m
  => MonadRandom m
  => MonadFail m
  => Colour -> m ()
solve_door c = do
  hand <- gets (view #osHand)
  if  Key c `elem` hand
    then assign #osStatus $ SolvingDoor c
    else do
      modifying #osLimbo (Door c :)
      draw

labirynth_last :: MonadReader OnirimState m => m (Maybe Location)
labirynth_last = do
  labirynth <- asks osLabirynth
  return $
        (last . view #_current $ labirynth)
    <|> (last . view #_past    $ labirynth)

can_place :: MonadReader OnirimState m => Location -> m Bool
can_place location  =
  maybe True (not . matching_symbols location) <$> labirynth_last

-- FIXME: use current and past properly
-- FIXME: open doors when appropriate
place ::
     MonadReader OnirimState m
  => MonadFail m
  => Location
  -> m (Labirynth, Maybe Colour)
place location = do
  valid <- can_place location
  unless valid $ fail ("Cannot place " <> show location)

  -- if this is a different colour, restart head
  -- if this is the same colour and is three then open door, empty head
  -- otherwise just append

  labirynth <- over #_current (++ [location]) <$> asks osLabirynth

  pure $ fromMaybe
         (labirynth, Nothing)
         (open_door_from_labirynth labirynth <|> restart_labirynth labirynth)

open_door_from_labirynth :: Labirynth -> Maybe (Labirynth, Maybe Colour)
open_door_from_labirynth lab = do
  [a, b, c] <- pure . toListOf (#_current . traverse . colour) $ lab
  guard $ a == b && b == c
  pure (over #_past (++ view #_current lab) $
        set #_current [] lab,
        Just c)

restart_labirynth :: Labirynth -> Maybe (Labirynth, Maybe Colour)
restart_labirynth lab = do
  (a : b : _) <- pure . reverse . toListOf (#_current . traverse . colour) $ lab
  guard $ a /= b
  (h, t) <- uncons . reverse . view #_current $ lab
  pure (over #_past (++ reverse t) $
        set #_current [h] lab,
        Nothing)

-- FIXME: Show errors, and provide a way to bail out
insist :: Monad m => MaybeT m a -> m a
insist x = do
  res <- runMaybeT x
  case res of
    Just a  -> pure a
    Nothing -> insist x

parse_user_input :: MonadFail m => String -> m OnirimTransition
parse_user_input input =
  let
    location args = assume_one args >>= parse_location
    parse_colour' args = assume_one args >>= parse_colour
  in do
    (cmd, args) <- uncons . words $ input
    case cmd of
      "place"       -> Place <$> location args
      "discard"     -> Discard <$> location args
      "discardhand" -> pure DiscardHand
      "discard5"    -> pure Discard5
      "closedoor"   -> CloseDoor <$> parse_colour' args
      "discardkey"  -> DiscardKey <$> parse_colour' args
      "opendoor"    -> OpenDoor <$> parse_colour' args
      "ignoredoor"  -> IgnoreDoor <$> parse_colour' args
      "rearrange"   -> Rearrange <$> parse_cards args
      _             -> fail $ "Unknown transition " <> cmd

assume_one :: Show a => MonadFail m => [a] -> m a
assume_one [a]  = pure a
assume_one many = fail $ "we wanted one element, but got " <> show many

un_maybe :: MonadFail m => Maybe a -> m a
un_maybe (Just a) = pure a
un_maybe Nothing  = fail "Wanted Just, got Nothing"

-- FIXME: Unbreak this horribleness
parse_card :: MonadFail m => String -> m Card
parse_card c = do
  location <- runMaybeT (Location <$> parse_location c)
  dream <- runMaybeT (Dream <$> parse_dream c)
  un_maybe (location <|> dream)

parse_cards :: MonadFail m => [String] -> m [Card]
parse_cards = traverse parse_card

parse_location :: MonadFail m => String -> m Location
parse_location ('M' : c) = Moon <$> parse_colour c
parse_location ('S' : c) = Sun <$> parse_colour c
parse_location ('K' : c) = Key <$> parse_colour c
parse_location invalid   = fail $ "Invalid location " <> invalid

parse_dream :: MonadFail m => String -> m Dream
parse_dream "N"       = pure Nightmare
parse_dream ('D' : c) = Door <$> parse_colour c
parse_dream invalid   = fail $ "Invalid dream " <> invalid

parse_colour :: MonadFail m => String -> m Colour
parse_colour "R"     = pure Red
parse_colour "B"     = pure Blue
parse_colour "G"     = pure Green
parse_colour "W"     = pure White
parse_colour invalid = fail $ "Invalid colour " <> invalid

get_transition ::
     MonadState OnirimState m
  => MonadFail m
  => MonadIO m
  => m OnirimTransition
get_transition = do
  get >>= print
  fromMaybe "" <$> readline "Your move: " >>= parse_user_input

-- TODO: Tie this better with the parser
warmup_history :: MonadIO m => m ()
warmup_history =
  traverse_ addHistory
   ["place",
    "discard",
    "discardhand",
    "discard5",
    "closedoor",
    "discardkey",
    "opendoor",
    "ignoredoor",
    "rearrange"
   ]

user_step ::
     MonadState OnirimState m
  => MonadFail m
  => MonadRandom m
  => MonadIO m
  => m ()
user_step = insist get_transition >>= apply_transition

-- FIXME: The user loop is very wonky. If we try to "cheat" we just crash the
-- program. Additionally, wehn inputing invalid commands we won't see why they
-- are invalid, just that the state didn't advance
main :: IO ()
main = do
  state <- flip execStateT initial_onirim_state $ do
    warmup_history
    apply_transition InitialSetup
    forever user_step
  print state
