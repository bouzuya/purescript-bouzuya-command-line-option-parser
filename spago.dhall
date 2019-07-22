{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "purescript-bouzuya-command-line-option-parser"
, dependencies =
    [ "foreign-object"
    , "prelude"
    , "psci-support"
    , "record"
    , "strings"
    , "test-unit"
    ]
, packages =
    ./packages.dhall
}
