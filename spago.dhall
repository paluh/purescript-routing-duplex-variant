{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ dependencies =
  [ "assert"
  , "control"
  , "effect"
  , "either"
  , "prelude"
  , "record"
  , "routing-duplex"
  , "typelevel-prelude"
  , "unsafe-coerce"
  , "variant"
  ]
, name = "routing-duplex-variant"
, license = "BSD-3-Clause"
, packages = ./packages.dhall
, repository = "https://github.com/paluh/purescript-routing-duplex-variant.git"
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
