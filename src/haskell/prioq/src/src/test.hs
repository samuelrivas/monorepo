{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE OverloadedStrings      #-}

import           Perlude

import           Control.Lens            (over, view)
import           Control.Monad           (when)
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Control.Monad.ST        (ST)
import           Data.Generics.Labels    ()
import           Data.IORef              (IORef, newIORef, readIORef,
                                          writeIORef)
import           Data.STRef              (STRef, newSTRef, readSTRef,
                                          writeSTRef)
import           Data.Vector             (MVector, Vector, freeze)
import           Data.Vector.Mutable     (length, new, unsafeGrow, unsafeTake,
                                          write)
import           GHC.Generics            (Generic)

-- TODO Move this to its own library
class MutableRef m r | m -> r where
  newRef :: a -> m (r a)
  writeRef :: r a -> a -> m ()
  readRef :: r a -> m a

modifyRef :: Monad m => MutableRef m r => r a -> (a -> a) -> m ()
modifyRef r f =
  do
    a <- readRef r
    writeRef r (f a)

instance MutableRef IO IORef where
  newRef = newIORef
  writeRef = writeIORef
  readRef = readIORef

instance MutableRef (ST s) (STRef s)  where
  newRef = newSTRef
  writeRef = writeSTRef
  readRef = readSTRef

-- Mutable lists with efficient appending and automatic, in place, resizing.
data DList s ref a = DList {
  storage :: ref (MVector s a),
  size    :: ref Int -- first position that is empty (could be beyond boundaries)
  } deriving stock (Generic)

minSize :: Int
minSize = 2

empty :: PrimMonad m => MutableRef m ref => m (DList (PrimState m) ref a)
empty =
  do
    v <- new minSize
    DList <$> newRef v <*> newRef 0

getSize :: MutableRef m ref => DList s ref a -> m Int
getSize = readRef . view #size

insert :: PrimMonad m => MutableRef m ref => DList (PrimState m) ref a -> a -> m ()
insert l a =
  do
    maybeResize l
    size <- getSize l
    v <- readRef (view #storage l)
    write v size a
    modifyRef (view #size l) (+1)

toVector :: PrimMonad m => MutableRef m ref =>
  DList (PrimState m) ref a -> m (Vector a)
toVector l =
  do
    size <- getSize l
    v <- readRef (view #storage l)
    freeze . unsafeTake size $ v

maybeResize :: PrimMonad m => MutableRef m ref =>
  DList (PrimState m) ref a -> m ()
maybeResize l =
  do
    size <- getSize l
    storage <- readRef (view #storage l)
    when (size == Data.Vector.Mutable.length storage) $ do
      newStorage <- unsafeGrow storage size
      writeRef (view #storage l) newStorage

main :: IO ()
main =
  do
    l <- empty
    insert l (42 :: Int)
    insert l (43 :: Int)
    insert l (44 :: Int)
    insert l (45 :: Int)
    insert l (46 :: Int)
    putStrLn "size:"
    h <- readRef $ view #size l
    print h
    putStrLn "length:"
    s <- readRef $ view #storage l
    print $ Data.Vector.Mutable.length s
    toVector l >>= print
