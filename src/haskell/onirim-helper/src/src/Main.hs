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

import           Prelude                    hiding (head)

import           Control.Applicative        ((<|>))
import           Control.Lens               (assign, modifying, set, view)
import           Control.Monad              (guard, unless, when)
import           Control.Monad.Fail         (MonadFail)
import           Control.Monad.Loops        (whileM_)
import           Control.Monad.Reader       (ask, asks)
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Monad.State.Class  (MonadState, gets, modify, put)
import           Control.Monad.State.Lazy   (execStateT)
import           Control.Monad.Trans.Maybe  (runMaybeT)
import           Data.Foldable              (fold)
import           Data.Generics.Labels       ()
import           Data.List                  (nub, permutations, sort)
import           Data.MultiSet              (MultiSet, delete, distinctElems,
                                             empty, insert, member, occur)
import           Data.Random                (MonadRandom, RVar, sample, shuffle)
import           Game
import           GHC.Generics               (Generic)
import           Util                       (head, uncons)

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

-- FIXME: Can we use lenses here to get the wrapped element?
instance Show Card where
  show (Location l) = show l
  show (Dream d)    = show d

concise_show :: Colour -> String
concise_show = head . show

get_colour :: Location -> Colour
get_colour = \case
  Key c -> c
  Sun c -> c
  Moon c -> c

matching_symbols :: Location -> Location -> Bool
matching_symbols (Key _) (Key _)   = True
matching_symbols (Sun _) (Sun _)   = True
matching_symbols (Moon _) (Moon _) = True
matching_symbols _ _               = False

-- is_location :: Card -> Bool
-- is_location (Location _) = True
-- is_location (Dream _)    = False

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
      1 -> "X" <> show c
      _ -> show c <> show c
  in
    unwords . fmap showColour $ [Red, Blue, Green, White]

instance GameState OnirimState OnirimTransition Bool where
  next_state = next_onirim_state
  transitions = onirim_transitions
  score = (Won ==) <$> asks osStatus

instance Show Labirynth where
  show l = showCards (view #_past l) <> "|"
    <> showCards (view #_current l)

instance Show OnirimState where
  show s =
    unlines
    ["Doors: " <> showDoors (view #osDoors s),
     "Labirynth: " <> show (view #osLabirynth s),
     "Deck: " <> showCards (view #osDeck s),
     "Hand: " <> showCards (view #osHand s),
     "Discards: " <> showCards (view #osDiscards s),
     "Limbo: " <> showCards (view #osLimbo s),
     "Status: " <> show (view #osStatus s)
    ]

showCards :: Show a => [a] -> String
showCards = unwords . fmap show

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

onirim_transitions :: (MonadReader OnirimState m) => m [OnirimTransition]
onirim_transitions = do
  status <- asks osStatus
  hand <- asks osHand
  doors <- asks osDoors
  deck <- asks osDeck
  case status of
    Uninitialised      -> return [InitialSetup]
    SolvingDoor colour -> return [OpenDoor colour, IgnoreDoor colour]
    SolvingNightmare   ->
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
    valid = not . is_door . last
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

next_onirim_state (Place location) = do
  assert_status Placing
  Just hand <- remove_location location <$> asks osHand
  labirynth <- place location
  state <- ask
  return . Stochastic $
    flip execStateT state $ runMaybeT $ do
      put $ state { osHand = hand, osLabirynth = labirynth }
      -- Check if we can open a door, and shuffle in that case
      draw

next_onirim_state (OpenDoor colour) = do
  assert_status $ SolvingDoor colour
  Just hand <- remove_location (Key colour) <$> asks osHand
  doors <- insert colour <$> asks osDoors
  state <- ask
  return . Stochastic $
    flip execStateT state $ runMaybeT $ do
      put $ state
        { osDoors = doors,
          osHand = hand,
          osStatus = Placing
        }
      draw

next_onirim_state (IgnoreDoor colour) = do
  assert_status $ SolvingDoor colour
  state <- ask
  return . Stochastic $
    flip execStateT state $ runMaybeT $ do
      modifying #osLimbo (Door colour :)
      assign #osStatus Placing
      draw

next_onirim_state (CloseDoor colour) = do
  assert_status SolvingNightmare
  doors <- asks osDoors
  unless (colour `member` doors) $ fail "cannot close a door you didn't open"
  state <- ask
  return . Stochastic $
    flip execStateT state $ runMaybeT $ do
      modifying #osLimbo (Door colour :)
      modifying #osDoors $ delete colour
      modifying #osDiscards (Dream Nightmare :)
      assign #osStatus Placing
      draw

next_onirim_state (DiscardKey colour) = do
  assert_status SolvingNightmare
  Just hand <- remove_location (Key colour) <$> asks osHand
  state <- ask
  return . Stochastic $
    flip execStateT state $ runMaybeT $ do
      assign #osHand hand
      assign #osStatus Placing
      modifying #osDiscards ([Dream Nightmare, Location (Key colour)] ++)
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
      modifying #osDeck ((init cards ++) . drop (length cards))
      modifying #osDeck (last cards :)
      assign #osStatus Placing
      draw

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

    Dream (Door colour) -> solve_door colour

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
solve_door colour = do
  hand <- gets (view #osHand)
  if  Key colour `elem` hand
    then assign #osStatus $ SolvingDoor colour
    else do
      modifying #osLimbo (Door colour :)
      draw

labirynth_last :: MonadReader OnirimState m => m (Maybe Location)
labirynth_last = do
  labirynth <- asks osLabirynth
  return $
        (head . view #_current $ labirynth)
    <|> (head . view #_past    $ labirynth)

can_place :: MonadReader OnirimState m => Location -> m Bool
can_place location  =
  maybe True (not . matching_symbols location) <$> labirynth_last

-- Return Just if a door can be opened
place ::
     MonadReader OnirimState m
  => MonadFail m
  => Location
  -> m Labirynth
place location = do
  valid <- can_place location
  unless valid $ fail ("Cannot place " <> show location)

  lab <- asks osLabirynth
  return $ lab { _past = location : _past lab }

main :: IO ()
-- main = execStateT force_game initial_onirim_state >>= print
main = (flip execStateT initial_onirim_state . runMaybeT $ force_n_steps 10) >>= print
