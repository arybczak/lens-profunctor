module Data.Lens.Profunctor.Fold
  ( Fold
  , Fold'
  , view
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
  ) where

import Data.Monoid
import Data.Profunctor
import Data.Lens.Profunctor.Types

type Fold r s t a b = Optic (Forget r) s t a b

type Fold' r s a = Fold r s s a a

-- | View the value pointed to by a 'Iso', 'Lens' or a 'Getter' or the result of
-- folding over all the results of a 'Fold' or 'Traversal' that points at a
-- monoidal value.
view :: Fold a a b s t -> s -> a
view optic = foldMapOf optic id

-- | Unrestricted version of 'affview'. Previews the first value of a 'Fold', if
-- there is any.
preview :: Fold (First a) a b s t -> s -> Maybe a
preview optic = getFirst . runForget (optic (Forget (First . Just)))

----------------------------------------

-- | Maps and then folds all foci of a `Fold`.
foldMapOf :: Fold r a b s t -> (a -> r) -> s -> r
foldMapOf optic = runForget . optic . Forget

-- | Right fold over a `Fold`.
foldrOf :: Fold (Endo r) a b s t -> (a -> r -> r) -> r -> s -> r
foldrOf optic step begin =
  flip appEndo begin . foldMapOf optic (Endo . step)

-- | Left fold over a `Fold`.
foldlOf :: Fold (Dual (Endo r)) a b s t -> (r -> a -> r) -> r -> s -> r
foldlOf optic step begin =
  flip appEndo begin . getDual . foldMapOf optic (Dual . Endo . flip step)

-- | Right fold over a `Fold`, strictly.
foldrOf' :: Fold (Dual (Endo (Endo r))) a b s t -> (a -> r -> r) -> r -> s -> r
foldrOf' optic step begin xs = foldlOf optic f (Endo id) xs `appEndo` begin
  where
    f (Endo k) x = Endo $ \z -> k $! step x z

-- | Left fold over a `Fold`, strictly.
foldlOf' :: Fold (Endo (Endo r)) a b s t -> (r -> a -> r) -> r -> s -> r
foldlOf' optic step begin xs = foldrOf optic f (Endo id) xs `appEndo` begin
  where
    f x (Endo k) = Endo $ \z -> k $! step z x

-- | Collects the foci of a `Fold` into a list.
toListOf :: Fold (Endo [a]) a b s t -> s -> [a]
toListOf optic = foldrOf optic (:) []

----------------------------------------

sequenceOf_
  :: Applicative f
  => Fold (Endo (f ())) (f a) b s t
  -> s
  -> f ()
sequenceOf_ optic = foldrOf optic (*>) (pure ())

traverseOf_
  :: Applicative f
  => Fold (Endo (f ())) a b s t
  -> (a -> f r)
  -> s
  -> f ()
traverseOf_ optic f = foldrOf optic ((*>) . f) (pure ())

----------------------------------------

allOf :: Fold All a b s t -> (a -> Bool) -> s -> Bool
allOf optic p = getAll . foldMapOf optic (All . p)

anyOf :: Fold Any a b s t -> (a -> Bool) -> s -> Bool
anyOf optic p = getAny . foldMapOf optic (Any . p)

----------------------------------------

-- | Folds over a 'Foldable' container.
folded :: (Foldable f, Monoid r) => Fold r a b (f a) r
folded (Forget a) = Forget (foldMap a)
