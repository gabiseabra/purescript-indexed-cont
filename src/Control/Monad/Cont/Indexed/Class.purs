module Control.Monad.Cont.Indexed.Class where

import Control.Monad.Indexed (class IxMonad)

class IxMonad m <= IxMonadCont m where
  reset :: forall r o a . m a o o -> m r r a
  shift :: forall r o j a . ((forall i. a -> m i i o) -> m r j j) -> m r o a

