module Data.Lens.Profunctor.AffTraversal
  ( AffineTraversal
  , atraversal
  ) where

import Data.Profunctor

import Data.Lens.Profunctor.Types

atraversal :: (s -> Either t a) -> (s -> b -> t) -> AffineTraversal a b s t
atraversal match update =
  dimap (\s -> (match s, update s))
        (\(etb, f) -> either id f etb)
  . first'
  . right'
