module Data.Lens.Profunctor.AffineFold
  ( AffineFold
  , view01
  , afolding
  ) where

import Data.Profunctor

import Data.Bicontravariant
import Data.Lens.Profunctor.Types

view01 :: AffineFold a b s t -> s -> Maybe a
view01 optic = runForgetM (optic (ForgetM Just))

afolding :: (s -> Maybe a) -> AffineFold a a s s
afolding f = contrabimap (\s -> maybe (Left s) Right (f s)) Left . right'

----------------------------------------

newtype ForgetM r a b = ForgetM { runForgetM :: a -> Maybe r }

instance Profunctor (ForgetM r) where
  dimap f _ (ForgetM k) = ForgetM (k . f)
  lmap f (ForgetM k) = ForgetM (k . f)
  rmap _ (ForgetM k) = ForgetM k

instance Strong (ForgetM r) where
  first' (ForgetM k) = ForgetM (k . fst)
  second' (ForgetM k) = ForgetM (k . snd)

instance Choice (ForgetM r) where
  left' (ForgetM k) = ForgetM (either k (const Nothing))
  right' (ForgetM k) = ForgetM (either (const Nothing) k)

instance Cochoice (ForgetM r) where
  unleft  (ForgetM k) = ForgetM (k . Left)
  unright (ForgetM k) = ForgetM (k . Right)

instance Bicontravariant (ForgetM r) where
  contrabimap f _ (ForgetM r) = ForgetM (r . f)
  contrafirst f (ForgetM r) = ForgetM (r . f)
  contrasecond _ (ForgetM r) = (ForgetM r)
