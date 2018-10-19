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
  bimap f g (Re p) = Re (p . contrabimap g f)

instance Bifunctor p => Bicontravariant (Re p a b) where
  contrabimap f g (Re p) = Re (p . bimap g f)

instance Choice p => Cochoice (Re p a b) where
  unleft  (Re p) = Re (p . left')
  unright (Re p) = Re (p . right')

instance Cochoice p => Choice (Re p a b) where
  left'  (Re p) = Re (p . unleft)
  right' (Re p) = Re (p . unright)

----------------------------------------

-- | Inverts optics, turning 'Iso' into 'Iso', 'Prism' into 'PrismaticGetter'
-- (and back) and 'Getter' into 'Review' (and back).
re :: Optic (Re p a b) a b s t -> Optic p t s b a
re optic = unRe (optic (Re id))
