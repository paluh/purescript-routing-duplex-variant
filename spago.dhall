{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "assert"
    , "console"
    , "effect"
    , "request-duplex"
    , "typelevel-prelude"
    , "variant"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
