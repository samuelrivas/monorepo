import           Control.Exception (Exception, SomeException, catch, evaluate,
                                    throw)
import           Data.Either       (fromRight)
import           Data.Maybe        (fromJust)
import           Data.Typeable     (typeOf)
import           GHC.Stack.Types   (HasCallStack)

-- Pure functions with alternative return
-- ======================================
maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

-- Exception not intended fro control flow, one would use maybeHead instead
exceptionHead :: HasCallStack => [a] -> a
exceptionHead = fromJust . maybeHead

eitherHead :: [a] -> Either String a
eitherHead []    = Left "trying to head an empty list"
eitherHead (a:_) = Right a

exceptionHead' :: HasCallStack => [a] -> a
exceptionHead' = either error id . eitherHead

-- Pure functions with error recovery
-- ==================================
data OurException = OurException
  deriving Show

instance Exception OurException

exceptionHead'' :: HasCallStack => [a] -> a
exceptionHead'' []    = throw OurException
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
