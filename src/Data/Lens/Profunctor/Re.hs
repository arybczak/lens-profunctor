module Data.Lens.Profunctor.Re
  ( Re(..)
  , re
  ) where

import Data.Bifunctor
import Data.Profunctor

import Data.Bicontravariant
import Data.Lens.Profunctor.Types

newtype Re p a b s t = Re { unRe :: p t s -> p b a }

instance Profunctor p => Profunctor (Re p a b) where
  dimap f g (Re p) = Re (p . dimap g f)

instance Bicontravariant p => Bifunctor (Re p a b) where
  bimap f g (Re p) = Re (p . cimap g f)

instance Bifunctor p => Bicontravariant (Re p a b) where
  cimap f g (Re p) = Re (p . bimap g f)

instance Choice p => Cochoice (Re p a b) where
  unleft  (Re r) = Re (r . left')
  unright (Re r) = Re (r . right')

instance Cochoice p => Choice (Re p a b) where
  left'  (Re r) = Re (r . unleft)
  right' (Re r) = Re (r . unright)

instance Strong p => Costrong (Re p a b) where
  unfirst  (Re r) = Re (r . first')
  unsecond (Re r) = Re (r . second')

instance Costrong p => Strong (Re p a b) where
  first'  (Re r) = Re (r . unfirst)
  second' (Re r) = Re (r . unsecond)

----------------------------------------

re :: Optic (Re p a b) a b s t -> Optic p t s b a
re optic = unRe (optic (Re id))
