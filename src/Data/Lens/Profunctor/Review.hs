module Data.Lens.Profunctor.Review
  ( Review
  , review
  , unto
  ) where

import Data.Profunctor
import Data.Tagged
import Data.Lens.Profunctor.Types

type Review b t = Optic Tagged b b t t

review :: Review b t -> b -> t
review optic = unTagged . optic . Tagged

unto :: (b -> t) -> Review b t
unto f = rmap f . Tagged . unTagged
