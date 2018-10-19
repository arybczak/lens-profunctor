module Data.Lens.Profunctor.Setter
  ( Setter
  , over
  , set
  , setting
  , mapped
  ) where

import Data.Profunctor
import Data.Lens.Profunctor.Types

over :: Setter a b s t -> (a -> b) -> s -> t
over = id

set :: Setter a b s t -> b -> s -> t
set optic = over optic . const

----------------------------------------

setting :: ((a -> b) -> s -> t) -> Setter a b s t
setting f = dimap (PStore id) (\(PStore g s) -> f g s) . map'

mapped :: Functor f => Setter a b (f a) (f b)
mapped = setting fmap
