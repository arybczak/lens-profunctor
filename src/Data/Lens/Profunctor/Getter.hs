module Data.Lens.Profunctor.Getter
  ( Getter
  , view1
  , to
  , like
  ) where

import Data.Profunctor

import Data.Bicontravariant
import Data.Lens.Profunctor.Types

view1 :: Getter a s -> s -> a
view1 optic = runForget (optic (Forget id))

----------------------------------------

-- | Build a Getter from an arbitrary Haskell function.
to :: (s -> a) -> Getter a s
to f = contrabimap f f

like :: a -> Getter a s
like = to . const
