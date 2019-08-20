-- These are for hacking around, remember not to commit them
-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

import           Control.Monad             (unless, when)
import           Control.Monad.Fail        (MonadFail)
import           Control.Monad.State.Class (MonadState, get, gets, modify, put)
import           Control.Monad.State.Lazy  (execStateT)
import           Control.Monad.Trans.Maybe (runMaybeT)
import           Data.Foldable             (fold)
import           Data.Map.Strict           (Map, empty)
import           Data.Random               (MonadRandom, RVar, sample, shuffle)
import           Game
import           Util                      (uncons)

{-# ANN module "HLint: ignore Use camelCase" #-}

data Colour = Red | Blue | Green | White
  deriving (Show, Eq)

data Type = Key | Sun | Moon
  deriving (Show, Eq)

data Dream = Door Colour | Nightmare
  deriving (Show, Eq)

data Card = Location Type Colour | Dream Dream
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
  { osDoors     :: Map Colour Int,
    osDeck      :: [Card],
    osLabirynth :: [Card],
    osDiscards  :: [Card],
    osHand      :: [Card],
    osLimbo     :: [Card],
    osStatus    :: Status
  } deriving Show

data OnirimTransition =
    InitialSetup
  | Discard Card
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
dreams :: [Card]
dreams =
  fold
  [ Dream . Door <$> all_colours,
    replicate 2 $ Dream Nightmare
  ]

locations :: [Card]
locations =
  fold
  [-- replicate 9 (Location Sun Red),
    replicate 8 (Location Sun Blue),
    replicate 7 (Location Sun Green),
    replicate 6 (Location Sun White),
    Location Moon <$> all_colours,
    Location Moon <$> all_colours,
    Location Moon <$> all_colours,
    Location Moon <$> all_colours,
    -- Location Key <$> all_colours,
    -- Location Key <$> all_colours,
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

next_onirim_state (Discard card) = do
  Just hand <- remove_card card <$> gets osHand
  discards <- (card :) <$> gets osDiscards
  state <- get
  return . Stochastic $
    flip execStateT state $ runMaybeT $ do
      put $ state { osHand = hand, osDiscards = discards }
      draw

remove_card :: Card -> [Card] -> Maybe [Card]
remove_card card cards =
  case break (card ==) cards of
    (before, _ : after) -> Just $ before ++ after
    _                   -> Nothing

initial_hand_and_deck :: RVar ([Card], [Card])
initial_hand_and_deck = do
  (hand, rest) <- splitAt 5 <$> shuffle locations

  when (length hand /= 5) $ fail "Not enough location cards in deck"

  reshuffled <- shuffle $ rest ++ dreams
  return (hand, reshuffled)

shuffle_cards ::
     MonadState OnirimState m
  => MonadRandom m
  => m ()
shuffle_cards = do
  deck <- gets osDeck
  limbo <- gets osLimbo
  shuffled <-  sample . shuffle $ deck ++ limbo
  modify $ \s -> s { osDeck = shuffled, osLimbo = [] }

pick_top ::
     MonadState OnirimState m
  => MonadFail m
  => m Card
pick_top = do
  (top, rest) <- gets osDeck >>= uncons
  modify $ \s -> s { osDeck = rest }
  return top

-- Fill the hand up to 5, stopping when hitting dreams
draw ::
     MonadState OnirimState m
  => MonadRandom m
  => MonadFail m
  => m ()
draw = do
  top <- pick_top
  hand <- gets osHand
  let new_hand = top : hand
  case top of
    Location _ _ -> do
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
  if  Location Key colour `elem` hand
    then modify $ \s -> s { osStatus = SolvingDoor colour }
    else do
      modify $ \s -> s { osLimbo = (Dream $ Door colour) : limbo }
      draw

main :: IO ()
-- main = execStateT force_game initial_onirim_state >>= print
main = (flip execStateT initial_onirim_state . runMaybeT $ force_n_steps 10) >>= print
