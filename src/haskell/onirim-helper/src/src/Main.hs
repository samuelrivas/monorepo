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
  { osDoors      :: Map Colour Int,
    osDeck       :: [Card],
    osLabirynth  :: [Card],
    osDiscards   :: [Card],
    osHand       :: [Card],
    status       :: Status
  } deriving Show

data OnirimTransition =
    InitialSetup
  | Discard
  deriving Show

instance GameState OnirimState OnirimTransition Bool where
  next_state = next_onirim_state
  transitions = onirim_transitions
  score state = status state == Won

initial_onirim_state :: OnirimState
initial_onirim_state =
  OnirimState
    empty
    onirim_deck
    []
    []
    []
    Uninitialised

onirim_deck :: [Card]
onirim_deck =
  let
    colours = [Red, Blue, Green, White]
  in
  fold
  [ replicate 9 (Location Sun Red),
    replicate 8 (Location Sun Blue),
    replicate 7 (Location Sun Green),
    replicate 6 (Location Sun White),
    Location Moon <$> colours,
    Location Moon <$> colours,
    Location Moon <$> colours,
    Location Moon <$> colours,
    Location Key <$> colours,
    Location Key <$> colours,
    Location Key <$> colours,
    Dream . Door <$> colours,
    Dream . Door <$> colours,
    replicate 10 $ Dream Nightmare
  ]

onirim_transitions :: OnirimState -> [OnirimTransition]
onirim_transitions = const []

next_onirim_state ::
     Monad m
  => OnirimTransition
  -> OnirimState
  -> MaybeT m (StateDistribution OnirimState)
next_onirim_state InitialSetup state' =
  return . Stochastic . shuffle_deck $ state'

next_onirim_state Discard state' =
  do
    (top, cards) <- uncons . osDeck $ state'
    return . Stochastic $ do
      new_deck <- shuffle cards
      return $ state'
        { osDeck = new_deck,
          osDiscards = top : osDiscards state'
        }

shuffle_deck :: OnirimState -> RVar OnirimState
shuffle_deck state' =
  do
    shuffled <- shuffle $ osDeck state'
    return $ state' { osDeck = shuffled }

main :: IO ()
-- main = execStateT force_game initial_onirim_state >>= print
main = (flip execStateT initial_onirim_state . runMaybeT $ force_n_steps 10) >>= print
