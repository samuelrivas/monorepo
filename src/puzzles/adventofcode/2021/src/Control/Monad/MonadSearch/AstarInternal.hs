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
import           Data.HashSet                  (HashSet)
import           Data.PriorityQueue.FingerTree (PQueue)
import           GHC.Generics                  (Generic)

data AstarContext n node nodeMem = AstarContext
    { openNodes :: PQueue n node
    , seenNodes :: HashSet nodeMem
    }
    deriving stock (Show, Generic)

-- FIXME wrap, at least, h and c so that they have different types
data AstarConfig n node nodeMem pc = AstarConfig
    { h              :: node -> Reader pc n
    , c              :: node -> Reader pc n
    , explode        :: node -> Reader pc [node]
    , isGoal         :: node -> Reader pc Bool
    , nodeToMem      :: node -> Reader pc nodeMem
    , privateContext :: pc
    }
    deriving stock (Generic)

instance Show (AstarConfig n node nodeMem context) where
  show = const "AstarConfig"
