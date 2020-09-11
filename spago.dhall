{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "indexed-cont"
, dependencies =
  [ "aff"
  , "effect"
  , "indexed-monad"
  , "parallel"
  , "psci-support"
  , "spec"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
