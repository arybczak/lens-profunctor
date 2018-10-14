module Data.Lens.Profunctor.Prism
  ( Prism
  , Prism'
  , prism
  , prism'
  , withPrism
  , only
  , _Left
  , _Right
  , _Just
  , _Nothing
  , _Show
  ) where

import Data.Profunctor
import Data.Lens.Profunctor.Types

-- | Concrete representation of a prism.
data PrismC a b s t = PrismC (s -> Either t a) (b -> t)

instance Profunctor (PrismC a b) where
  dimap :: (s' -> s) -> (t -> t') -> PrismC a b s t -> PrismC a b s' t'
  dimap f g (PrismC match build) =
    PrismC (either (Left . g) Right . match . f)
           (g . build)

instance Choice (PrismC a b) where
  left' :: PrismC a b s t -> PrismC a b (Either s c) (Either t c)
  left' (PrismC match build) =
    PrismC (either (either (Left . Left) Right . match) (Left . Right))
           (Left . build)

  right' :: PrismC a b s t -> PrismC a b (Either c s) (Either c t)
  right' (PrismC match build) =
    PrismC (either (Left . Left) (either (Left . Right) Right . match))
          (Right . build)

----------------------------------------

type Prism a b s t = forall p. Choice p => Optic p a b s t

type Prism' a s = Prism a a s s

prism :: (s -> Either t a) -> (b -> t) -> Prism a b s t
prism match build = dimap match (either id build) . right'

prism' :: (s -> Maybe a) -> (b -> s) -> Prism a b s s
prism' sma = prism (\s -> maybe (Left s) Right (sma s))

withPrism :: Prism a b s t -> ((s -> Either t a) -> (b -> t) -> r) -> r
withPrism optic k = case optic (PrismC Right id) of
                      PrismC match build -> k match build


----------------------------------------

only :: Eq a => a -> Prism' () a
only a = prism' (\s -> if s == a then Just () else Nothing) (const a)

_Left :: Prism a b (Either a c) (Either b c)
_Left = left'

_Right :: Prism a b (Either c a) (Either c b)
_Right = right'

_Just :: Prism a b (Maybe a) (Maybe b)
_Just = prism (maybe (Left Nothing) Right) Just

_Nothing :: Prism' () (Maybe a)
_Nothing = prism (maybe (Right ()) (Left . Just)) (const Nothing)

_Show :: (Read a, Show a) => Prism' a String
_Show = prism maybeRead show
  where
    maybeRead = \s -> case reads s of
      [(a, "")] -> Right a
      _         -> Left  s
