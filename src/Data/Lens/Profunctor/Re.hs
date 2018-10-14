module Data.Lens.Profunctor.Re where

import Data.Profunctor
import Data.Lens.Profunctor.Types

newtype Re p a b s t = Re { unRe :: p t s -> p b a }

instance Profunctor p => Profunctor (Re p t s) where
  dimap f g (Re p) = Re (p . dimap g f)

instance Choice p => Cochoice (Re p t s) where
  unleft (Re r) = Re (r . left')
  unright (Re r) = Re (r . right')

instance Cochoice p => Choice (Re p t s) where
  left' (Re r) = Re (r . unleft)
  right' (Re r) = Re (r . unright)

instance Strong p => Costrong (Re p t s) where
  unfirst (Re r) = Re (r . first')
  unsecond (Re r) = Re (r . second')

instance Costrong p => Strong (Re p t s) where
  first' (Re r) = Re (r . unfirst)
  second' (Re r) = Re (r . unsecond)

instance Cochoice (Forget r) where
  unleft  (Forget r) = Forget (r . Left)
  unright (Forget r) = Forget (r . Right)

----------------------------------------

re :: Optic (Re p a b) a b s t -> Optic p t s b a
re optic = unRe (optic (Re id))
