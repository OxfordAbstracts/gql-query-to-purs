{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "generics-rep"
  , "graphql-client"
  , "graphql-parser"
  , "halogen"
  , "numbers"
  , "ocelot"
  , "parsing"
  , "psci-support"
  , "routing-duplex"
  , "strings-extra"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
