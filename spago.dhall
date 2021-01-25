{ name = "routing-duplex-variant"
, dependencies =
  [ "assert"
  , "console"
  , "effect"
  , "routing-duplex"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
