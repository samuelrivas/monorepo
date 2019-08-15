-- These are for hacking around, remember not to commit them
-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

import           Control.Monad.State.Lazy  (execStateT)
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Data.Foldable             (fold)
import           Data.Map.Strict           (Map, empty)
import           Data.Random               (RVar, shuffle)
import           Game
import           Util                      (uncons)

{-# ANN module "HLint: ignore Use camelCase" #-}

data Colour = Red | Blue | Green | White
  deriving Show

data Type = Key | Sun | Moon
  deriving Show

data Dream = Door Colour | Nightmare
  deriving Show

data Card = Location Colour Type | Dream Dream
  deriving Show

data OnirimState = OnirimState
  { doors      :: Map Colour Int,
    deck       :: [Card],
    labirynth  :: [Card],
    discards   :: [Card],
    hand       :: [Card],
    unshuffled :: Bool
  } deriving Show

data OnirimTransition =
    InitialSetup
  | Discard
  deriving Show

instance GameState OnirimState OnirimTransition Bool where
  next_state = next_onirim_state
  transitions = onirim_transitions
  score = null . deck

initial_onirim_state :: OnirimState
initial_onirim_state =
  OnirimState
    empty
    onirim_deck
    []
    []
    []
    True

onirim_deck :: [Card]
onirim_deck =
  let
    colours = [Red, Blue, Green, White]
    moon_of colour = Location colour Moon
  in
  fold
  [ replicate 9 (Location Red Sun),
    replicate 8 (Location Blue Sun),
    replicate 7 (Location Green Sun),
    replicate 6 (Location White Sun),
    moon_of <$> colours,
    Dream . Door <$> colours,
    Dream . Door <$> colours,
    replicate 10 $ Dream Nightmare
  ]

next_onirim_state ::
     Monad m
  => OnirimTransition
  -> OnirimState
  -> MaybeT m (StateDistribution OnirimState)
next_onirim_state InitialSetup state' =
  return . Stochastic . shuffle_deck $ state'

next_onirim_state Discard state' =
  do
    (top, cards) <- uncons . deck $ state'
    return . Stochastic $ do
      new_deck <- shuffle cards
      return $ state' { deck = new_deck, discards = top : discards state' }

type TransitionGen = OnirimState -> [OnirimTransition]

initial_setup_transition :: TransitionGen
initial_setup_transition st = [InitialSetup | unshuffled st]

discard_transition :: TransitionGen
discard_transition st =
  let
    is_shuffled = not . unshuffled $ st
    more_cards = not . null . deck $ st
  in
    [Discard | is_shuffled && more_cards]

all_transition_gen :: [TransitionGen]
all_transition_gen =
  [ initial_setup_transition,
    discard_transition
  ]

onirim_transitions :: OnirimState -> [OnirimTransition]
onirim_transitions st = fold $ ($ st) <$> all_transition_gen

shuffle_deck :: OnirimState -> RVar OnirimState
shuffle_deck state' =
  do
    shuffled <- shuffle $ deck state'
    return $ state' { deck = shuffled, unshuffled = False }

main :: IO ()
-- main = execStateT force_game initial_onirim_state >>= print
main = (flip execStateT initial_onirim_state . runMaybeT $ force_n_steps 10) >>= print
