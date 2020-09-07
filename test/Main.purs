module Test.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Cont.Indexed (IxContT(..), emptyCont, evalIxContT, return, runIxContT)
import Control.Monad.Indexed ((:>>=))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldReturn)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] $ do
  functorSpec
  bindSpec
  applySpec
  semigroupSpec
  altSpec
  monoidSpec
  monadSpec

functorSpec :: Spec Unit
functorSpec = describe "functor IxContT" do
  it "map" do
    let cont = ((*) 2) <$> pure 123
    evalIxContT cont `shouldReturn` 246

bindSpec :: Spec Unit
bindSpec = describe "bind IxContT" do
  it "bind" do
    let cont = pure 123 >>= (pure <<< ((*) 2))
    evalIxContT cont `shouldReturn` 246
  it "Ix.bind" do
    let cont = pure 123 :>>= \i -> Do $ \k -> show <$> k (i * 2)
    evalIxContT cont `shouldReturn` "246"

applySpec :: Spec Unit
applySpec = describe "apply IxContT" do
  it "apply" do
    let cont = (*) <$> pure 123 <*> pure 2
    evalIxContT cont `shouldReturn` 246
  it "Ix.apply" do
    let cont = Do (\k -> show <$> k ((*) 2)) `Ix.apply` Ix.pure 123
    evalIxContT cont `shouldReturn` "246"

semigroupSpec :: Spec Unit
semigroupSpec = describe "semigroup IxContT" do
  it "append" do
    let cont = return 123 <> return 321 <> mempty
    evalIxContT cont `shouldEqual` [123, 321]

altSpec :: Spec Unit
altSpec = describe "alt IxContT" do
  it "alt" do
    let cont = emptyCont <|> return 123
    evalIxContT cont `shouldEqual` Just 123

monoidSpec :: Spec Unit
monoidSpec = describe "monoid IxContT" do
  it "mempty" do
    let cont = mempty :: forall r i . IxContT Array r r i
    runIxContT cont (\_ -> [123]) `shouldEqual` []

monadSpec :: Spec Unit
monadSpec = describe "monad IxContT" do
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
