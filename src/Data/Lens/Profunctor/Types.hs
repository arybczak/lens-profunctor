module Data.Lens.Profunctor.Types
  ( Optic
  ) where

type Optic p a b s t = p a b -> p s t
