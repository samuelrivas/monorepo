-- These are for hacking around, remember not to commit them
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}

import           Control.Monad.Fail
import           Control.Monad.Loops
import           Control.Monad.State.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.List                 hiding (uncons, head)
import           Data.Map.Strict           hiding (null)
import           Data.Random
import           Prelude                   hiding (fail, head)

{-# ANN module "HLint: ignore Use camelCase" #-}

data StateDistribution a =
    Deterministic a
  | Stochastic (RVar a)

instance Distribution StateDistribution a where
  rvar (Deterministic a) = pure a
  rvar (Stochastic a)    = a

-- | The GameState class. We represent the game state as a function taking a
-- transition and returning a distribution of next states
--
-- Note that you need to wrap a specific state for each game, since we need to
-- make the transition type depend on the state in order to prevent ambiguity in
-- the score function
class (Ord score, Bounded score)
  => GameState state trans score | state -> trans, state -> score where

  next_state :: (Monad m) => trans -> state -> MaybeT m (StateDistribution state)
  transitions :: state -> [trans]
  score :: state -> score

data Colour = Red | Blue | Green | White
data Type = Key | Sun | Moon
data Dream = Door Colour | Nightmare
data Card = Location Colour Type | Dream Dream

data OnirimState = OnirimState
  { doors     :: Map Colour Int,
    deck      :: [Card],
    labirynth :: [Card],
    discards  :: [Card],
    hand      :: [Card]
  }

data OnirimTransition = Discard

instance GameState OnirimState OnirimTransition Bool where
  next_state = next_onirim_state
  transitions = onirim_transitions
  score = null . deck

uncons :: (MonadFail m) => [a] -> m (a, [a])
uncons (x:xs) = return (x, xs)
uncons []     = fail "cannot uncons []"

head :: (MonadFail m) => [a] -> m a
head (x:_) = return x
head [] = fail "cannot head []"

next_onirim_state ::
  Monad m
  => OnirimTransition
  -> OnirimState
  -> MaybeT m (StateDistribution OnirimState)
next_onirim_state _ state' =
  do
    (_, cards) <- uncons . deck $ state'
    return . Deterministic $ state' { deck = cards }

onirim_transitions :: OnirimState -> [OnirimTransition]
onirim_transitions st = deck st >>= return [Discard]

apply_transition ::
     GameState state transition score
  => MonadState state m
  => MonadRandom m
  => transition -> MaybeT m ()
apply_transition transition =
      get
  >>= next_state transition
  >>= lift . sample
  >>= put

-- XXX Create a better game monad so that we don't need to do transitions <$>
-- get all the time
force_move ::
     GameState state transition score
  => MonadState state m
  => MonadRandom m
  => MaybeT m ()
force_move =
  get
  >>= head . transitions
  >>= apply_transition

force_game ::
     GameState state transition score
  => MonadState state m
  => MonadRandom m
  => m ()
force_game = whileJust_ (runMaybeT force_move) return

main :: IO ()
main = putStrLn "hi!"
