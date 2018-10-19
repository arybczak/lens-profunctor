module Data.Lens.Profunctor.Iso
  ( Iso
  , iso
  , withIso
  , _Dual
  , _Endo
  , _All
  , _Any
  , _Sum
  , _Product
  , _First
  , _Last
  , _Alt
  ) where

import Control.Applicative
import Data.Monoid
import Data.Profunctor

import Data.Lens.Profunctor.Types

-- | Concrete representation of an isomorphism.
data IsoC a b s t = IsoC (s -> a) (b -> t)

instance Profunctor (IsoC a b) where
  dimap :: (s' -> s) -> (t -> t') -> IsoC a b s t -> IsoC a b s' t'
  dimap f g (IsoC from to) = IsoC (from . f) (g . to)

----------------------------------------

-- | Build a simple isomorphism from a pair of inverse functions.
iso :: (s -> a) -> (b -> t) -> Iso a b s t
iso = dimap

-- | Extract the two functions, one from @s -> a@ and one from @b -> t@ that
-- characterize an Iso.
withIso :: Iso a b s t -> ((s -> a) -> (b -> t) -> r) -> r
withIso optic k = case optic (IsoC id id) of
                    IsoC from to -> k from to

----------------------------------------

_Dual :: Iso (Dual a) (Dual b) a b
_Dual = iso Dual getDual

_Endo :: Iso (Endo a) (Endo b) (a -> a) (b -> b)
_Endo = iso Endo appEndo

_All :: Iso All All Bool Bool
_All = iso All getAll

_Any :: Iso Any Any Bool Bool
_Any = iso Any getAny

_Sum :: Num a => Iso (Sum a) (Sum b) a b
_Sum = iso Sum getSum

_Product :: Num a => Iso (Product a) (Product b) a b
_Product = iso Product getProduct

_First :: Iso (First a) (First b) (Maybe a) (Maybe b)
_First = iso First getFirst

_Last :: Iso (Last a) (Last b) (Maybe a) (Maybe b)
_Last = iso Last getLast

_Alt :: Alternative f => Iso (Alt f a) (Alt f b) (f a) (f b)
_Alt = iso Alt getAlt
