import           Control.DeepSeq             (NFData, force)
import           Control.Parallel.Strategies (Eval, Strategy, rpar, rparWith,
                                              rseq, runEval, using)
import           Data.List                   (splitAt)

parPair :: Strategy a -> Strategy b -> Strategy (a, b)
-- parPair (a, b) = do
--   a' <- rpar a
--   b' <- rpar b
--   pure (a', b')
parPair sa sb =
  evalPair (rparWith sa) (rparWith sb)

evalPair :: Strategy a -> Strategy b -> Strategy (a, b)
evalPair sa sb (a, b) = do
  a' <- sa a
  b' <- sb b
  pure (a', b')

rdeepseq :: NFData a => Strategy a
rdeepseq = rseq . force

deepPair :: (NFData a, NFData b) => Strategy (a, b)
deepPair =
  evalPair rdeepseq rdeepseq

parMap :: (a -> b) -> [a] -> [b]
parMap f xs = fmap f xs `using` parList rseq

evalList :: Strategy a -> Strategy [a]
evalList _ [] = pure []
evalList sa (a:as) = do
  a' <- sa a
  as' <- evalList sa as
  pure $ a' : as'

parList :: Strategy a -> Strategy [a]
parList sa = evalList $ rparWith sa

main = putStrLn "foo"
