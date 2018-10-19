module Data.Lens.Profunctor.Fold
  ( Fold
  , viewN
  , preview
  , foldMapOf
  , foldrOf
  , foldlOf
  , foldrOf'
  , foldlOf'
  , toListOf
  , sequenceOf_
  , traverseOf_
  , allOf
  , anyOf
  , folded
  , folding
  ) where

import Data.Foldable
import Data.Monoid
import Data.Profunctor
import Data.Profunctor.Traversing

import Data.Bicontravariant
import Data.Lens.Profunctor.Types

-- | View the result of folding over all the results of a 'Fold' or 'Traversal'
-- that points at a monoidal value.
viewN :: Monoid a => Fold a b s t -> s -> a
viewN optic = runForget (optic (Forget id))

-- | Unrestricted version of 'view01'. Previews the first value of a 'Fold', if
-- there is any.
preview :: Fold a b s t -> s -> Maybe a
preview optic = getFirst . runForget (optic (Forget (First . Just)))

----------------------------------------

-- | Maps and then folds all foci of a `Fold`.
foldMapOf :: Monoid r => Fold a b s t -> (a -> r) -> s -> r
foldMapOf optic = runForget . optic . Forget

-- | Right fold over a `Fold`.
foldrOf :: Fold a b s t -> (a -> r -> r) -> r -> s -> r
foldrOf optic step begin =
  flip appEndo begin . foldMapOf optic (Endo . step)

-- | Left fold over a `Fold`.
foldlOf :: Fold a b s t -> (r -> a -> r) -> r -> s -> r
foldlOf optic step begin =
  flip appEndo begin . getDual . foldMapOf optic (Dual . Endo . flip step)

-- | Right fold over a `Fold`, strictly.
foldrOf' :: Fold a b s t -> (a -> r -> r) -> r -> s -> r
foldrOf' optic step begin xs = foldlOf optic f (Endo id) xs `appEndo` begin
  where
    f (Endo k) x = Endo $ \z -> k $! step x z

-- | Left fold over a `Fold`, strictly.
foldlOf' :: Fold a b s t -> (r -> a -> r) -> r -> s -> r
foldlOf' optic step begin xs = foldrOf optic f (Endo id) xs `appEndo` begin
  where
    f x (Endo k) = Endo $ \z -> k $! step z x

-- | Collects the foci of a `Fold` into a list.
toListOf :: Fold a b s t -> s -> [a]
toListOf optic = foldrOf optic (:) []

----------------------------------------

sequenceOf_
  :: Applicative f
  => Fold (f a) b s t
  -> s
  -> f ()
sequenceOf_ optic = foldrOf optic (*>) (pure ())

traverseOf_
  :: Applicative f
  => Fold a b s t
  -> (a -> f r)
  -> s
  -> f ()
traverseOf_ optic f = foldrOf optic ((*>) . f) (pure ())

----------------------------------------

allOf :: Fold a b s t -> (a -> Bool) -> s -> Bool
allOf optic p = getAll . foldMapOf optic (All . p)

anyOf :: Fold a b s t -> (a -> Bool) -> s -> Bool
anyOf optic p = getAny . foldMapOf optic (Any . p)

----------------------------------------

-- | Folds over a 'Foldable' container.
folded :: Foldable f => Fold a b (f a) t
folded = contrasecond (\_ -> ()) . wander traverse_

folding :: Foldable f => (s -> f a) -> Fold a b s t
folding f = contrafirst f . folded
