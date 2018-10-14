module Data.Lens.Profunctor.Getter
  ( Getter
  , to
  , like
  ) where

import Data.Profunctor
import Data.Lens.Profunctor.Types

type Getter a b s t = forall r. Optic (Forget r) a b s t

-- | Build a Getter from an arbitrary Haskell function.
to :: (s -> a) -> Getter a b s t
to f p = Forget (runForget p . f)

like :: a -> Getter a b s t
like = to . const
