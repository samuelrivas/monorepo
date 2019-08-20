-- These are for hacking around, remember not to commit them
-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

import           Control.Exception         (assert)
import           Control.Monad.State.Class (MonadState, get, gets, modify, put)
import           Control.Monad.State.Lazy  (execStateT)
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Data.Foldable             (fold)
import           Data.Map.Strict           (Map, empty)
import           Data.Random               (MonadRandom, RVar, sample, shuffle)
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
  { osDoors     :: Map Colour Int,
    osDeck      :: [Card],
    osLabirynth :: [Card],
    osDiscards  :: [Card],
    osHand      :: [Card],
    osStatus    :: Status
  } deriving Show

data OnirimTransition =
    InitialSetup
  | Discard Card
  deriving Show

instance GameState OnirimState OnirimTransition Bool where
  next_state = next_onirim_state
  transitions = onirim_transitions
  score =  (Won ==) <$>gets osStatus

initial_onirim_state :: OnirimState
initial_onirim_state =
  OnirimState
    empty
    []
    []
    []
    []
    Uninitialised

all_colours :: [Colour]
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

onirim_transitions :: MonadState OnirimState m => m [OnirimTransition]
onirim_transitions = do
  status <- gets osStatus
  hand <- gets osHand
  return $
    case status of
      Uninitialised -> [InitialSetup]
      _             -> Discard <$> hand

next_onirim_state ::
     MonadState OnirimState m
  => OnirimTransition
  -> MaybeT m (StateDistribution OnirimState)
next_onirim_state InitialSetup = do
  state <- get
  return . Stochastic $ do
    (hand, rest) <- splitAt 5 <$> shuffle locations
    let
      state_with_hand = state
        { osHand = hand,
          osDeck = rest ++ dreams,
          osStatus = Placing
        }
    shuffle_deck state_with_hand

initial_hand_and_deck :: RVar ([Card], [Card])
initial_hand_and_deck = do
  (hand, rest) <- splitAt 5 <$> shuffle locations

  return $ assert (length hand == 5) ()

  reshuffled <- shuffle $ rest ++ dreams
  return (hand, reshuffled)

-- next_onirim_state (Discard _) =
--   do
--     (top, rest) <- uncons . osDeck $ state
--     return . Deterministic $ state
--       { osDeck = rest,
--         osDiscards = top : osDiscards state
--       }

shuffle_deck :: OnirimState -> RVar OnirimState
shuffle_deck state = do
  shuffled <- shuffle $ osDeck state
  return $ state { osDeck = shuffled }

shuffle_deck_s ::
     MonadState OnirimState m
  => MonadRandom m
  => m ()
shuffle_deck_s = do
  deck <- gets osDeck >>= sample . shuffle
  modify (\s -> s { osDeck = deck })

main :: IO ()
-- main = execStateT force_game initial_onirim_state >>= print
main = (flip execStateT initial_onirim_state . runMaybeT $ force_n_steps 10) >>= print
