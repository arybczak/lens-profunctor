{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Lens.Profunctor.Types
  ( Optic
  , Iso
  , Lens
  , Prism
  , AffineTraversal
  , AffineFold
  , Traversal
  , Fold
  , Getter
  , Review
  ) where

import Data.Bifunctor
import Data.Profunctor
import Data.Profunctor.Traversing

import Data.Bicontravariant

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

type AffineFold a b s t =
  forall p. (Bicontravariant p, Choice p, Cochoice p, Strong p) => Optic p a b s t

type Traversal a b s t =
  forall p. Traversing p => Optic p a b s t

type Fold a b s t =
  forall p. (Bicontravariant p, Cochoice p, Traversing p) => Optic p a b s t

type Getter a s =
  forall p. (Bicontravariant p, Cochoice p, Strong p) => Optic p a a s s

type Review b t =
  forall p. (Bifunctor p, Choice p) => Optic p b b t t
