module Control.Monad.Cont.Indexed where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Monad.Cont as Cont
import Control.Monad.Cont.Indexed.Class (class IxMonadCont, shift)
import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxFunctor, class IxMonad, iapply, imap, ipure, (:>>=))
import Control.Monad.Reader (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.State (class MonadState, state)
import Control.Parallel (class Parallel, parallel, sequential)
import Control.Plus (class Plus, empty)
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect.Aff.AVar as AVar
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

instance functorIxContT :: Functor m => Functor (IxContT m r o) where
  map = imap

instance applicativeIxContT :: Applicative m => Applicative (IxContT m r r) where
  pure = ipure

instance applyIxContT :: Applicative m => Apply (IxContT m r r) where
  apply = iapply

instance bindIxContT :: (Functor m, Applicative m) => Bind (IxContT m r r) where
  bind k m = k :>>= m

instance semigroupIxContT ::  Semigroup (m r) => Semigroup (IxContT m r o a) where
  append (Do f) (Do g) = Do $ \k -> f k <> g k

instance altIxContT ::  Alt m => Alt (IxContT m r o) where
  alt (Do f) (Do g) = Do $ \k -> f k <|> g k

instance plusIxContT :: Plus m => Plus (IxContT m r o) where
  empty = escM empty

instance alternativeIxContT :: Alternative m => Alternative (IxContT m r r)

instance monoidIxContT ::  Monoid (m r) => Monoid (IxContT m r o a) where
  mempty = escM mempty

instance monadIxContT :: Monad m => Monad (IxContT m r r)

instance monadEffectIxContT :: MonadEffect m => MonadEffect (IxContT m r r) where
  liftEffect = ilift <<< liftEffect

instance monadAffIxContT :: MonadAff m => MonadAff (IxContT m r r) where
  liftAff = ilift <<< liftAff

instance monadContIxContT :: Monad m => Cont.MonadCont (IxContT m r r) where 
  callCC = callCC

instance monadAskIxContT :: MonadAsk e m => MonadAsk e (IxContT m r r) where
  ask = ilift ask

instance monadReaderIxContT :: MonadReader e m => MonadReader e (IxContT m r r) where
  local f m = Do $ \k -> local f (runIxContT m k)

instance monadStateIxContT :: MonadState e m => MonadState e (IxContT m r r) where
  state = ilift <<< state

{-----------------------------------------------–------------------------------}

parApplyCont :: forall f m r a b
   . Parallel f m
  => Alt f
  => MonadAff m
  => IxContT m r r (a -> b)
  -> IxContT m r r a
  -> IxContT m r r b
parApplyCont cf ca = Do \k -> do
  rf <- liftAff $ AVar.empty
  ra <- liftAff $ AVar.empty
  let
    take :: forall x . AVar.AVar x -> m x
    take = liftAff <<< AVar.take
    put :: forall x . AVar.AVar x -> x -> m Unit
    put var = liftAff <<< (flip AVar.put) var
    cont' f a = k (f a)
    mf = runIxContT cf $ \f -> put rf f *> take ra >>= \a -> cont' f a
    ma = runIxContT ca $ \a -> put ra a *> take rf >>= \f -> cont' f a
  sequential $ parallel ma <|> parallel mf

newtype IxParContT m r o a = Par ((a -> m o) -> m r)

derive instance newtypeIxParContT :: Newtype (IxParContT m r o a) _

instance fonctorIxParContT :: (Parallel f m, Alt f, MonadAff m) => Functor (IxParContT m r r) where
  map f = parallel <<< map f <<< sequential

instance applicativeIxParContT :: (Parallel f m, Alt f, MonadAff m) => Applicative (IxParContT m r r) where
  pure = parallel <<< pure

instance applyIxParContT :: (Parallel f m, Alt f, MonadAff m) => Apply (IxParContT m r r) where
  apply cf ca = parallel $ parApplyCont (sequential cf) (sequential ca)

instance semigroupParContT :: (Parallel f m, Semigroup (f r)) => Semigroup (IxParContT m r r a) where
  append (Par c) (Par c') = Par $ \k -> sequential $ parallel (c k) <> parallel (c' k)

instance monoidIxParContT ::  (Parallel f m, Semigroup (f r), Monoid (m r)) => Monoid (IxParContT m r r a) where
  mempty = Par $ \_ -> mempty

instance altIxParContT :: (Parallel f m, Alt f, MonadAff m) => Alt (IxParContT m r r) where
  alt (Par c) (Par c') = Par $ \k -> sequential $ parallel (c k) <|> parallel (c' k)

instance plusIxParContT :: (Parallel f m, Alt f, Plus m, MonadAff m) => Plus (IxParContT m r r) where
  empty = Par $ \_ -> empty

instance alternativeIxParContT :: (Parallel f m, Alt f, Plus m, MonadAff m) => Alternative (IxParContT m r r)

instance parallelIxContT :: (Parallel f m, Alt f, MonadAff m) => Parallel (IxParContT m r r) (IxContT m r r) where 
  parallel = wrap <<< unwrap
  sequential = wrap <<< unwrap

{-----------------------------------------------–------------------------------}

escM :: forall m r o a . m r -> IxContT m r o a
escM = Do <<< const

esc :: forall m r o a . Applicative m => r -> IxContT m r o a
esc = escM <<< pure

ilift :: forall m r a . Monad m => m a -> IxContT m r r a
ilift m = Do (m >>= _)

fromContT :: forall m r a . Monad m => Cont.ContT r m a -> IxContT m r r a
fromContT = wrap <<< unwrap

toContT :: forall m r a . Monad m => IxContT m r r a -> Cont.ContT r m a
toContT = wrap <<< unwrap

runIxContT :: forall m r o a . Functor m => IxContT m r o a -> (a -> m o) ->  m r 
runIxContT (Do m) f = m f

evalIxContT :: forall m r o . Applicative m => IxContT m r o o -> m r
evalIxContT = (flip runIxContT) pure
