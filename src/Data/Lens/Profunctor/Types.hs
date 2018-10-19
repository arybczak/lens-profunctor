{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Lens.Profunctor.Types
  ( PStore(..)
  , Optic
  , Iso
  , Lens
  , Prism
  , AffineTraversal
  , Traversal
  , Setter
  , Getter
  , PrismaticGetter
  , AffineFold
  , Fold
  , Review
  ) where

import Data.Bifunctor
import Data.Profunctor
import Data.Profunctor.Traversing

import Data.Bicontravariant

-- | Taken from mezzolens library. For more information see
-- https://r6research.livejournal.com/28432.html
data PStore a b t = PStore (b -> t) a
  deriving Functor

-- | Missing instance.
instance Cochoice (Forget r) where
  unleft  (Forget r) = Forget (r . Left)
  unright (Forget r) = Forget (r . Right)

----------------------------------------

type Optic p a b s t = p a b -> p s t

type Iso a b s t =
  forall p. Profunctor p => Optic p a b s t

type Lens a b s t =
  forall p. Strong p => Optic p a b s t

type Prism a b s t =
  forall p. Choice p => Optic p a b s t

type AffineTraversal a b s t =
  forall p. (Choice p, Strong p) => Optic p a b s t

-- p = Star
type Traversal a b s t =
  forall p. Traversing p => Optic p a b s t

-- p = (->)
type Setter a b s t =
  forall p. Mapping p => Optic p a b s t

-- p = Forget. This is needed because without it 're' would turn 'Prism' into
-- 'Getter', but 'Getter' into 'Review', hence re . re /= id.
type PrismaticGetter a s =
  forall p. Cochoice p => Optic p a a s s

-- p = Forget, view1 for extraction of exactly one value.
type Getter a s =
  forall p. (Bicontravariant p, Cochoice p) => Optic p a a s s

-- p = ForgetM, view01 for extraction of at most one value.
type AffineFold a b s t =
  forall p. (Bicontravariant p, Choice p, Cochoice p, Strong p) => Optic p a b s t

-- p = Forget (+ Monoid), viewN for extraction of multiple elements and folding
-- them into one with Monoid instance.
type Fold a b s t =
  forall p. (Bicontravariant p, Cochoice p, Traversing p) => Optic p a b s t

-- p = Tagged, review for reviewing Iso or Prism.
type Review b t =
  forall p. (Bifunctor p, Choice p) => Optic p b b t t
