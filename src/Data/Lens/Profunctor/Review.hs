module Data.Lens.Profunctor.Review
  ( Review
  , review
  , unto
  ) where

import Data.Bifunctor
import Data.Profunctor
import Data.Tagged
import Data.Void

import Data.Lens.Profunctor.Types

review :: Review b t -> b -> t
review optic = unTagged . optic . Tagged

unto :: (b -> t) -> Review b t
unto f = first absurd . dimap absurd f
