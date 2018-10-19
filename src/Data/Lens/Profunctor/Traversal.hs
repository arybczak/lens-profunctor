module Data.Lens.Profunctor.Traversal
  ( Traversal
  , traversed
  , traverseOf
  , both2
  , both3
  ) where

import Data.Profunctor
import Data.Profunctor.Traversing

import Data.Lens.Profunctor.Types

-- | Map each element of a structure targeted by a 'Traversal', evaluate these
-- actions from left to right, and collect the results.
traverseOf :: Applicative f => Traversal a b s t -> (a -> f b) -> s -> f t
traverseOf t = runStar . t . Star

----------------------------------------

-- | Traverse a 'Traversable' functor.
traversed :: Traversable t => Traversal a b (t a) (t b)
traversed = wander traverse

both2 :: Traversal a b (a, a) (b, b)
both2 = wander $ \t (a1, a2) -> (,) <$> t a1 <*> t a2

both3 :: Traversal a b (a, a, a) (b, b, b)
both3 = wander $ \t (a1, a2, a3) -> (,,) <$> t a1 <*> t a2 <*> t a3
