-- These are for hacking around, remember not to commit them
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}

import           Control.Monad.Fail
import           Control.Monad.Loops
import           Control.Monad.State.Class
import           Control.Monad.State.Lazy     hiding (fail)
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Foldable
import           Data.List                    hiding (head, uncons)
import           Data.Map.Strict              hiding (map, null)
import           Data.Maybe
import           Data.Random
import           Data.Random.Internal.Source  (MonadRandom (..),
                                               getRandomPrimFrom)
import           Data.Random.Source.DevRandom
import           Prelude                      hiding (fail, head)

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
--
-- A state is considered final if the list of transitions from it is empty
class (Ord score, Bounded score)
  => GameState state trans score | state -> trans, state -> score where
  next_state :: (Monad m) => trans -> state -> MaybeT m (StateDistribution state)
  transitions :: state -> [trans]
  score :: state -> score

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

uncons :: (MonadFail m) => [a] -> m (a, [a])
uncons (x:xs) = return (x, xs)
uncons []     = fail "cannot uncons []"

head :: (MonadFail m) => [a] -> m a
head (x:_) = return x
head []    = fail "cannot head []"

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

get_transitions ::
     GameState state transition score
  => MonadState state m
  => m [transition]
get_transitions = transitions <$> get

is_final ::
     GameState state transition score
  => MonadState state m
  => m Bool
is_final = not . null <$> get_transitions

force_move ::
     GameState state transition score
  => MonadState state m
  => MonadRandom m
  => MaybeT m ()
force_move = get_transitions >>= head >>= apply_transition

force_game ::
     GameState state transition score
  => MonadState state m
  => MonadRandom m
  => m ()
--force_game = whileJust_ (runMaybeT force_move) return
force_game = whileJust_ (runMaybeT force_move) return

force_n_steps ::
     GameState state transition score
  => MonadState state m
  => MonadRandom m
  => Int -> MaybeT m ()
force_n_steps steps = replicateM_ steps force_move

-- XXX Why is this instance not there? And, heck, why are there two MonadRandom
-- implementations (there is an instance for Control.Monad.Random.Class)
-- instance MonadRandom m => MonadRandom (StateT s m) where
instance MonadIO m => MonadRandom (StateT s m) where
  getRandomPrim = liftIO . getRandomPrimFrom DevURandom

main :: IO ()
-- main = execStateT force_game initial_onirim_state >>= print
main = (flip execStateT initial_onirim_state . runMaybeT $ force_n_steps 10) >>= print
