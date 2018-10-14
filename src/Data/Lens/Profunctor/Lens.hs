module Data.Lens.Profunctor.Lens
 ( Lens
 , Lens'
 , lens
 , vl
 , _1_2
 , _2_2
 , _1_22
 ) where

import Data.Profunctor
import Data.Lens.Profunctor.Types

-- | Concrete representation of a lens.
data LensC a b s t = LensC (s -> a) (s -> b -> t)

instance Profunctor (LensC a b) where
  dimap :: (s' -> s) -> (t -> t') -> LensC a b s t -> LensC a b s' t'
  dimap f g (LensC view_ update) = LensC (view_ . f) (\s -> g . update (f s))

instance Strong (LensC a b) where
  first' :: LensC a b s t -> LensC a b (s, c) (t, c)
  first' (LensC view_ update) = LensC (view_ . fst) (\(s, c) b -> (update s b, c))

  second' :: LensC a b s t -> LensC a b (c, s) (c, t)
  second' (LensC view_ update) = LensC (view_ . snd) (\(c, s) b -> (c, update s b))

----------------------------------------

type Lens a b s t = forall p. Strong p => Optic p a b s t

type Lens' a s = Lens a a s s

lens :: (s -> a) -> (s -> b -> t) -> Lens a b s t
lens view_ update =
  dimap (\s -> (view_ s, s))
        (\(b, s) -> update s b)
  . first'

{-# INLINE vl #-}
-- | Convert from a Van Laarhoven lens to a profunctor lens.
vl :: (forall f. Functor f => (a -> f b) -> s -> f t)
   -> Lens a b s t
vl l =
  dimap ((\(PStore f a) -> (f, a)) . l (PStore id))
        (\(f, b) -> f b)
  . second' -- p (b -> t, a) (b -> t, b)

-- | Taken from mezzolens library. For more information see
-- https://r6research.livejournal.com/28432.html
data PStore a b t = PStore (b -> t) a
  deriving Functor

----------------------------------------

_1_2 :: Lens a b (a, c) (b, c)
_1_2 = first'

_2_2 :: Lens a b (c, a) (c, b)
_2_2 = second'

_1_22 :: Lens a b (a, c) (b, c)
_1_22 = vl $ \f (a, c) -> (\x -> (x, c)) <$> f a
