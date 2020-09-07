module Control.Monad.Cont.Indexed where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Monad.Cont as Cont
import Control.Monad.Cont.Indexed.Class (class IxMonadCont, shift)
import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxFunctor, class IxMonad, iapply, imap, ipure, (:>>=))
import Control.Monad.Reader (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.State (class MonadState, state)
import Control.Parallel (class Parallel, parallel, sequential)
import Control.Plus (class Plus, empty)
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)

newtype IxContT m r o a = Do ((a -> m o) -> m r)

derive instance newtypeIxContT :: Newtype (IxContT m r o a) _

instance ixFunctorIxContT :: Functor m => IxFunctor (IxContT m) where
  imap f m = Do $ \c -> runIxContT m (c <<< f)

instance ixBindIxContT :: (Functor m, Applicative m) => IxBind (IxContT m) where
  ibind c f = Do $ \k -> runIxContT c $ \a -> runIxContT (f a) k

instance ixApplyIxContT :: Applicative m => IxApply (IxContT m) where
  iapply f x = f :>>= \f' -> x :>>= \x' -> ipure (f' x')

instance ixApplicativeIxContT :: Applicative m => IxApplicative (IxContT m) where
  ipure a = Do $ \k -> k a

instance ixMonadIxContT :: Monad m => IxMonad (IxContT m)

instance ixMonadContIxContT :: Monad m => IxMonadCont (IxContT m) where
  reset e = Do $ \k -> runIxContT e pure >>= k
  shift e = Do $ \k -> e (\a -> Do (\k' -> k a >>= k')) `runIxContT` pure

callCC :: forall m r o a . Monad m => ((forall i b . a -> IxContT m o i b) -> IxContT m r o a) -> IxContT m r o a
callCC f = shift (\k -> f (adapt k) :>>= k)
  where
    adapt :: forall x i' b' . (a -> IxContT m o x x) -> a -> IxContT m o i' b'
    adapt k x = k x :>>= (Do <<< const <<< pure)

instance fonctorIxContT :: Functor m => Functor (IxContT m i j) where
  map = imap

instance applicativeIxContT :: Applicative m => Applicative (IxContT m i i) where
  pure = ipure

instance applyIxContT :: Applicative m => Apply (IxContT m r r) where
  apply = iapply

instance bindIxContT :: (Functor m, Applicative m) => Bind (IxContT m r r) where
  bind k m = k :>>= m

instance semigroupIxContT ::  Semigroup (m r) => Semigroup (IxContT m r o i) where
  append (Do f) (Do g) = Do $ \k -> f k <> g k

instance altIxContT ::  Alt m => Alt (IxContT m r o) where
  alt (Do f) (Do g) = Do $ \k -> f k <|> g k

instance monoidIxContT ::  Monoid (m r) => Monoid (IxContT m r o i) where
  mempty = Do $ \_ -> mempty

instance monadIxContT :: Monad m => Monad (IxContT m i i)

instance monadEffectIxContT :: MonadEffect m => MonadEffect (IxContT m i i) where
  liftEffect = ilift <<< liftEffect

instance monadAffIxContT :: MonadAff m => MonadAff (IxContT m i i) where
  liftAff = ilift <<< liftAff

instance monadContIxContT :: Monad m => Cont.MonadCont (IxContT m i i) where 
  callCC = callCC

instance parallelIxContT :: Parallel f m => Parallel (IxContT f i i) (IxContT m i i) where 
  parallel (Do f) = Do $ \k -> parallel $ f \i -> sequential $ k i
  sequential (Do f) = Do $ \k -> sequential $ f \i -> parallel $ k i

instance monadAskIxContT :: MonadAsk e m => MonadAsk e (IxContT m i i) where
  ask = ilift ask

instance monadReaderIxContT :: MonadReader e m => MonadReader e (IxContT m i i) where
  local f m = Do $ \k -> local f (runIxContT m k)

instance monadStateIxContT :: MonadState e m => MonadState e (IxContT m i i) where
  state = ilift <<< state

{-----------------------------------------------–-------------------------------------------}

return :: forall m r o i . Monad m => r -> IxContT m r o i
return = Do <<< const <<< pure

emptyCont :: forall m r o i . Plus m => IxContT m r o i
emptyCont = Do $ \_ -> empty

ilift :: forall m r a . Monad m => m a -> IxContT m r r a
ilift m = Do (m >>= _)

runIxContT :: forall m r o a . Functor m => IxContT m r o a -> (a -> m o) ->  m r 
runIxContT (Do m) f = m f

evalIxContT :: forall m r o . Applicative m => IxContT m r o o -> m r
evalIxContT = (flip runIxContT) pure
