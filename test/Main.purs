module Test.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Cont.Indexed (IxContT(..), evalIxContT, esc, runIxContT)
import Control.Monad.Indexed ((:>>=))
import Control.Monad.Indexed.Qualified as Ix
import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_, parallel, sequential)
import Effect.Aff.Class (class MonadAff, liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldReturn)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (Config, defaultConfig, runSpec')

config :: Config
config = defaultConfig { timeout = Just $ Milliseconds 150.0 }

main :: Effect Unit
main = launchAff_ $ runSpec' config [ consoleReporter ] $ do
  functorSpec
  bindSpec
  applySpec
  semigroupSpec
  altSpec
  plusSpec
  monoidSpec
  monadSpec
  parallelSpec
  parallelAltSpec
  parallelPlusSpec

functorSpec :: Spec Unit
functorSpec = describe "IxContT/Functor" do
  it "maps a function" do
    let cont = ((*) 2) <$> pure 123
    evalIxContT cont `shouldReturn` 246

bindSpec :: Spec Unit
bindSpec = describe "IxContT/Bind" do
  it "bind" do
    let cont = pure 123 >>= (pure <<< ((*) 2))
    evalIxContT cont `shouldReturn` 246
  it "Ix.bind" do
    let cont = pure 123 :>>= \i -> Do $ \k -> show <$> k (i * 2)
    evalIxContT cont `shouldReturn` "246"

applySpec :: Spec Unit
applySpec = describe "IxContT/Apply" do
  it "apply" do
    let cont = (*) <$> pure 123 <*> pure 2
    evalIxContT cont `shouldReturn` 246
  it "Ix.apply" do
    let cont = Do (\k -> show <$> k ((*) 2)) `Ix.apply` Ix.pure 123
    evalIxContT cont `shouldReturn` "246"

semigroupSpec :: Spec Unit
semigroupSpec = describe "IxContT/Semigroup" do
  it "append" do
    let cont = esc 123 <> esc 321 <> mempty
    evalIxContT cont `shouldEqual` [123, 321]

altSpec :: Spec Unit
altSpec = describe "IxContT/Alt" do
  it "alt" do
    let cont = esc 123 <|> esc 321
    evalIxContT cont `shouldEqual` Just 123

plusSpec :: Spec Unit
plusSpec = describe "IxContT/Plus" do
  it "left identity" do
    let cont = empty <|> esc 123
    evalIxContT cont `shouldEqual` Just 123
  it "right identity" do
    let cont = esc 123 <|> empty
    evalIxContT cont `shouldEqual` Just 123

monoidSpec :: Spec Unit
monoidSpec = describe "IxContT/Monoid" do
  it "mempty" do
    let cont = mempty :: forall r i . IxContT Array r r i
    runIxContT cont (\_ -> [123]) `shouldEqual` []

monadSpec :: Spec Unit
monadSpec = describe "IxContT/Monad" do
  it "do" do
    let cont = do
          a <- pure 2
          b <- pure 2
          pure $ a * b
    runIxContT cont ((*) 2 >>> pure) `shouldReturn` 8
  it "Ix.do" do
    let cont = Ix.do
          a <- Ix.pure 2
          b <- Ix.pure 2
          Do $ \k -> show <$> k (a * b)
    runIxContT cont ((*) 2 >>> pure) `shouldReturn` "8"


after :: forall m r a . MonadAff m => Number -> a -> IxContT m r r a
after ms x = do
  liftAff $ delay $ Milliseconds ms
  pure x

parallelSpec :: Spec Unit
parallelSpec = describe "IxContT/Parallel" do
  it "sequential/parallel" do
    let cont = sequential
             $ (\a b c -> [a, b, c])
            <$> parallel (after 100.0 1)
            <*> parallel (after 100.0 2)
            <*> parallel (after 100.0 3)
    evalIxContT cont `shouldReturn` [1, 2, 3]

parallelAltSpec :: Spec Unit
parallelAltSpec = describe "IxParContT/Alt" do
  it "alt" do
    let cont = sequential
             $  parallel (after 100.0 1)
            <|> parallel (after 200.0 2)
    evalIxContT cont `shouldReturn` 1

parallelPlusSpec :: Spec Unit
parallelPlusSpec = describe "IxParContT/Plus" do
  it "left adentity" do
    let cont = sequential $  empty <|> parallel (after 100.0 1)
    evalIxContT cont `shouldReturn` 1
  it "right identity" do
    let cont = sequential $ parallel (after 100.0 1) <|> empty
    evalIxContT cont `shouldReturn` 1

