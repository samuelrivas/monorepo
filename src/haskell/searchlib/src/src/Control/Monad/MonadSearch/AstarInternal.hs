{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module Control.Monad.MonadSearch.AstarInternal (
  AstarContext (AstarContext),
  AstarConfig (AstarConfig)
  ) where

import           Perlude

import qualified Prelude

import           Control.Monad.Reader          (Reader)
import           Data.PriorityQueue.FingerTree (PQueue)
import           GHC.Generics                  (Generic)

data AstarContext n node nodeStore = AstarContext
    { openNodes :: PQueue n node
    , nodeStore :: nodeStore
    }
    deriving stock (Show, Generic)

-- FIXME wrap, at least, h and c so that they have different types
data AstarConfig n node nodeStore pc = AstarConfig
    { h              :: node -> Reader pc n
    , c              :: node -> Reader pc n
    , explode        :: node -> Reader pc [node]
    , isGoal         :: node -> Reader pc Bool
    , rememberNode   :: nodeStore -> node -> Reader pc nodeStore
    , seenNode       :: nodeStore -> node -> Reader pc Bool
    , privateContext :: pc
    }
    deriving stock (Generic)

instance Show (AstarConfig n node nodeMem context) where
  show = const "AstarConfig"
