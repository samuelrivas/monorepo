-- These are for hacking around, remember not to commit them
-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

import Data.Maybe (catMaybes)
import Data.List (nub)
import           Control.Monad             (unless, when)
import           Control.Monad.Fail        (MonadFail)
import           Control.Monad.State.Class (MonadState, get, gets, modify, put)
import           Control.Monad.State.Lazy  (execStateT)
import           Control.Monad.Trans.Maybe (runMaybeT)
import           Data.Foldable             (fold)
import           Data.MultiSet             hiding (fold, null, filter)
import           Data.Random               (MonadRandom, RVar, sample, shuffle)
import           Game
import           Util                      (uncons)

{-# ANN module "HLint: ignore Use camelCase" #-}

data Colour = Red | Blue | Green | White
  deriving (Show, Eq, Ord)

data Dream = Door Colour | Nightmare
  deriving (Show, Eq)

data Location = Key Colour | Sun Colour | Moon Colour
  deriving (Show, Eq)

data Card = Location Location | Dream Dream
  deriving (Show, Eq)

data Status =
    Uninitialised
  | Placing
  | Prophecy
  | SolvingNightmare
  | SolvingDoor Colour
  | Lost
  | Won
  deriving (Show, Eq)

data OnirimState = OnirimState
  { osDoors     :: MultiSet Colour,
    osDeck      :: [Card],
    osLabirynth :: [Location],
    osDiscards  :: [Card],
    osHand      :: [Location],
    osLimbo     :: [Dream],
    osStatus    :: Status
  } deriving Show

data OnirimTransition =
    InitialSetup
  | Discard Location
  | OpenDoor Colour
  | IgnoreDoor Colour
  | DiscardHand
  | DiscardKey Colour
  | CloseDoor Colour
  | Discadrd5
  deriving Show

instance GameState OnirimState OnirimTransition Bool where
  next_state = next_onirim_state
  transitions = onirim_transitions
  score = (Won ==) <$> gets osStatus

initial_onirim_state :: OnirimState
initial_onirim_state =
  OnirimState
    empty
    []
    []
    []
    []
    []
    Uninitialised

all_colours :: [Colour]
all_colours = [Red, Blue, Green, White]

-- dreams :: [Card]
-- dreams =
--   fold
--   [ Dream . Door <$> all_colours,
--     Dream . Door <$> all_colours,
--     replicate 10 $ Dream Nightmare
--   ]
dreams :: [Dream]
dreams =
  fold
  [ Door <$> all_colours,
    replicate 2 $ Nightmare
  ]

locations :: [Location]
locations =
  fold
  [-- replicate 9 (Location . Sun $ Red),
    replicate 8 (Sun $ Blue),
    replicate 7 (Sun $ Green),
    replicate 6 (Sun $ White),
    Moon <$> all_colours,
    Moon <$> all_colours,
    Moon <$> all_colours,
    Moon <$> all_colours,
    -- Key <$> all_colours,
    -- Key <$> all_colours,
    Key <$> all_colours
  ]

onirim_transitions :: MonadState OnirimState m => m [OnirimTransition]
onirim_transitions = do
  status <- gets osStatus
  hand <- gets osHand
  doors <- gets osDoors
  deck <- gets osDeck
  return $
    case status of
      Uninitialised      -> [InitialSetup]
      SolvingDoor colour -> [OpenDoor colour, IgnoreDoor colour]
      -- SolvingNightmare   ->
      --   concat
      --   [ discard_key hand,
      --     close_door doors,
      --     discard_5 deck,
      --     [DiscardHand]
      --   ]
      _                  -> Discard <$> hand

-- discard_key :: [Card] -> [OnirimTransition]
-- discard_key = nub $ get_colour <$> filter is_key 

next_onirim_state ::
     MonadState OnirimState m
  => MonadFail m
  => OnirimTransition
  -> m (StateDistribution OnirimState)
next_onirim_state InitialSetup = do
  state <- get
  return . Stochastic $ do
    (hand, deck) <- initial_hand_and_deck
    return $ state
      { osHand = hand,
        osDeck = deck,
        osStatus = Placing
      }

next_onirim_state (Discard location) = do
  Just hand <- remove_location location <$> gets osHand
  discards <- (Location location :) <$> gets osDiscards
  state <- get
  return . Stochastic $
    flip execStateT state $ runMaybeT $ do
      put $ state { osHand = hand, osDiscards = discards }
      draw

next_onirim_state (OpenDoor colour) = do
  Just hand <- remove_location (Key colour) <$> gets osHand
  doors <- insert colour <$> gets osDoors
  state <- get
  return . Stochastic $
    flip execStateT state $ runMaybeT $ do
      put $ state
        { osDoors = doors,
          osHand = hand,
          osStatus = Placing
        }
      draw

next_onirim_state (IgnoreDoor colour) = do
  limbo <- gets osLimbo
  state <- get
  return . Stochastic $
    flip execStateT state $ runMaybeT $ do
      put $ state
        { osLimbo = Door colour : limbo,
          osStatus = Placing
        }
      draw

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
        else do
          limbo <- gets osLimbo
          unless (null limbo) shuffle_cards

    Dream Nightmare -> modify $ \s -> s { osStatus = SolvingNightmare }

    Dream (Door colour) -> solve_door colour

solve_door ::
     MonadState OnirimState m
  => MonadRandom m
  => MonadFail m
  => Colour -> m ()
solve_door colour = do
  hand <- gets osHand
  limbo <- gets osLimbo
  if  Key colour `elem` hand
    then modify $ \s -> s { osStatus = SolvingDoor colour }
    else do
      modify $ \s -> s { osLimbo = Door colour : limbo }
      draw

main :: IO ()
-- main = execStateT force_game initial_onirim_state >>= print
main = (flip execStateT initial_onirim_state . runMaybeT $ force_n_steps 10) >>= print
