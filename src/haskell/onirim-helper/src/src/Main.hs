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

data Card = Location Type Colour | Dream Dream
  deriving Show

is_location :: Card -> Bool
is_location (Location _ _) = True
is_location (Dream _) = False

data Status =
    Uninitialised
  | Placing
  | Prophecy
  | SolvingNightmare
  | SolvingDoor
  | Lost
  | Won
  deriving (Show, Eq)

data OnirimState = OnirimState
  { osDoors     :: Map Colour Int,
    osDeck      :: [Card],
    osLabirynth :: [Card],
    osDiscards  :: [Card],
    osHand      :: [Card],
    osStatus    :: Status
  } deriving Show

data OnirimTransition =
    InitialSetup
  | Discard
  deriving Show

instance GameState OnirimState OnirimTransition Bool where
  next_state = next_onirim_state
  transitions = onirim_transitions
  score state = osStatus state == Won

initial_onirim_state :: OnirimState
initial_onirim_state =
  OnirimState
    empty
    []
    []
    []
    []
    Uninitialised

all_colours = [Red, Blue, Green, White]

dreams :: [Card]
dreams =
  fold
  [ Dream . Door <$> all_colours,
    Dream . Door <$> all_colours,
    replicate 10 $ Dream Nightmare
  ]

locations :: [Card]
locations =
  fold
  [ replicate 9 (Location Sun Red),
    replicate 8 (Location Sun Blue),
    replicate 7 (Location Sun Green),
    replicate 6 (Location Sun White),
    Location Moon <$> all_colours,
    Location Moon <$> all_colours,
    Location Moon <$> all_colours,
    Location Moon <$> all_colours,
    Location Key <$> all_colours,
    Location Key <$> all_colours,
    Location Key <$> all_colours
  ]

onirim_transitions :: OnirimState -> [OnirimTransition]
onirim_transitions st =
  case osStatus st of
    Uninitialised -> [InitialSetup]
    _             -> [Discard]

next_onirim_state ::
     Monad m
  => OnirimTransition
  -> OnirimState
  -> MaybeT m (StateDistribution OnirimState)
-- next_onirim_state InitialSetup state' =
--    return . Stochastic . shuffle_deck $ state' { osStatus = Placing }
next_onirim_state InitialSetup state =
  return . Stochastic $ do
    (hand, rest) <- splitAt 5 <$> shuffle locations
    let
      state_with_hand = state
        { osHand = hand,
          osDeck = rest ++ dreams,
          osStatus = Placing
        }
    shuffle_deck state_with_hand

next_onirim_state Discard state =
  do
    (top, rest) <- uncons . osDeck $ state
    return . Deterministic $ state
      { osDeck = rest,
        osDiscards = top : osDiscards state
      }

shuffle_deck :: OnirimState -> RVar OnirimState
shuffle_deck state =
  do
    shuffled <- shuffle $ osDeck state
    return $ state { osDeck = shuffled }

main :: IO ()
-- main = execStateT force_game initial_onirim_state >>= print
main = (flip execStateT initial_onirim_state . runMaybeT $ force_n_steps 10) >>= print
