module Data.Lens.Profunctor.AffTraversal
  ( AffTraversal
  , afftraversal
  , affview
  ) where

import Data.Profunctor
import Data.Lens.Profunctor.Types

type AffTraversal a b s t = forall p. (Strong p, Choice p) => Optic p a b s t

afftraversal :: (s -> Either t a) -> (s -> b -> t) -> AffTraversal a b s t
afftraversal match update =
  dimap (\s -> (match s, update s))
        (\(etb, f) -> either id f etb)
  . first'
  . right'

affview :: AffTraversal a b s t -> s -> Maybe a
affview l = runForgetM (l (ForgetM Just))

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
