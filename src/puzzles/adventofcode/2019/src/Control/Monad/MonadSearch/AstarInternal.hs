{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module Control.Monad.MonadSearch.AstarInternal (
  AstarContext (AstarContext),
  AstarConfig (AstarConfig)
  ) where

import           Control.Monad.Reader          (Reader)
import           Data.HashSet                  (HashSet)
import           Data.PriorityQueue.FingerTree (PQueue)
import           GHC.Generics                  (Generic)

data AstarContext node nodeMem = AstarContext
    { openNodes :: PQueue Int node
    , seenNodes :: HashSet nodeMem
    }
    deriving stock (Show, Generic)

-- FIXME wrap, at least, h and c so that they have different types
data AstarConfig node nodeMem pc = AstarConfig
    { h              :: node -> Reader pc Int
    , c              :: node -> Reader pc Int
    , explode        :: node -> Reader pc [node]
    , isGoal         :: node -> Reader pc Bool
    , nodeToMem      :: node -> Reader pc nodeMem
    , privateContext :: pc
    }
    deriving stock (Generic)

instance Show (AstarConfig node nodeMem context) where
  show = const "AstarConfig"
