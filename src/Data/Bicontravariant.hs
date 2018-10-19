module Data.Bicontravariant
  ( Bicontravariant(..)
  )where

import Data.Profunctor

class Bicontravariant p where
  contrabimap :: (b -> a) -> (d -> c) -> p a c -> p b d
  contrabimap f g = contrafirst f . contrasecond g

  contrafirst :: (b -> a) -> p a c -> p b c
  contrafirst f = contrabimap f id

  contrasecond :: (c -> b) -> p a b -> p a c
  contrasecond = contrabimap id

  {-# MINIMAL contrabimap | (contrafirst, contrasecond) #-}

instance Bicontravariant (Forget r) where
  contrabimap f _ (Forget r) = Forget (r . f)
  contrafirst f (Forget r) = Forget (r . f)
  contrasecond _ (Forget r) = (Forget r)
