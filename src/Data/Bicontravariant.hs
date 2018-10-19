module Data.Bicontravariant
  ( Bicontravariant(..)
  )where

import Data.Profunctor

class Bicontravariant p where
  cimap :: (b -> a) -> (d -> c) -> p a c -> p b d
  cimap f g = cofirst f . cosecond g

  cofirst :: (b -> a) -> p a c -> p b c
  cofirst f = cimap f id

  cosecond :: (c -> b) -> p a b -> p a c
  cosecond = cimap id

  {-# MINIMAL cimap | (cofirst, cosecond) #-}

instance Bicontravariant (Forget r) where
  cimap f _ (Forget r) = Forget (r . f)
  cofirst f (Forget r) = Forget (r . f)
  cosecond _ (Forget r) = (Forget r)
