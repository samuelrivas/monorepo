import           Control.Exception         (Exception (..), SomeException,
                                            catch, evaluate, throw)
import           Control.Monad.Catch       (MonadThrow (..))
import           Control.Monad.Error.Class (MonadError)
import           Data.Either               (fromRight)
import           Data.Maybe                (fromJust)
import           Data.Typeable             (typeOf)
import           GHC.Stack                 (callStack, prettyCallStack)
import           GHC.Stack.Types           (CallStack, HasCallStack)

-- Pure functions with alternative return
-- ======================================
maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

-- Exception not intended fro control flow, one would use maybeHead instead
exceptionHead :: [a] -> a
exceptionHead = fromJust . maybeHead

eitherHead :: [a] -> Either String a
eitherHead []    = Left "trying to head an empty list"
eitherHead (a:_) = Right a

exceptionHead' :: HasCallStack => [a] -> a
exceptionHead' = either error id . eitherHead

-- Pure functions with error recovery
-- ==================================
data OurException = OurException CallStack

instance Show OurException where
  show (OurException cs) = "OurException: " ++ (prettyCallStack cs)

instance Exception OurException

exceptionHead'' :: HasCallStack => [a] -> a
exceptionHead'' []    = throw $ OurException callStack
exceptionHead'' (a:_) = a

maybeHead'' :: [a] -> IO (Maybe a)
maybeHead'' as =
  catch (Just <$> evaluate (exceptionHead'' as))
  (\(_ :: OurException) -> pure Nothing)

-- Using alternative return in monadic style
-- =========================================
heads :: ([a], [a]) -> Maybe (a, a)
heads (a, b) = (,) <$> maybeHead a <*> maybeHead b

-- With mixed alternative return style
heads' :: ([a], [a]) -> Maybe (a, a)
heads' (a, b) =
  (,) <$> maybeHead a <*> (either (const Nothing) Just . eitherHead $ b)

-- We still keep exception semantics if we use exceptionHead''
heads'' :: ([a], [a]) -> (a, a)
heads'' (xs, ys) = (exceptionHead'' xs, exceptionHead'' ys)

avoidException :: Int
avoidException = snd $ heads'' ([], [1])

-- Note that using monads, the above can't work. The function below returns
-- Nothing
avoidException' :: Maybe Int
avoidException' = snd <$> heads ([], [1])

-- Using MonadThrow
-- ================

headMThrow :: HasCallStack => MonadThrow m => [a] -> m a
headMThrow []    = throwM $ OurException callStack
headMThrow (a:_) = pure a

headMThrowEither :: [a] -> Either SomeException a
headMThrowEither = headMThrow

headMThrowMaybe ::  [a] -> Maybe a
headMThrowMaybe = headMThrow

headMThrowIO :: [a] -> IO a
headMThrowIO = headMThrow

headMThrowException :: HasCallStack => [a] -> a
headMThrowException = either throw id . headMThrow

-- We can match the exception in pure code, even though this is cumbersome
headMThrowDefault :: a -> [a] -> a
headMThrowDefault d as =
  case headMThrow as of
    Left e  ->
      case fromException e of
        Just (OurException _) -> d
        Nothing               -> throw e
    Right a -> a

-- Using MonadFail with error
headFail :: HasCallStack => MonadFail m => [a] -> m a
headFail []    = fail "empty list"
headFail (a:_) = pure a

headFailMaybe :: [a] -> Maybe a
headFailMaybe = headFail

-- There is no instance for Either
