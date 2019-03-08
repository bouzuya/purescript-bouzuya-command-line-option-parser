module Test.Bouzuya.CommandLineOption.OptionObject
  ( tests
  ) where

import Bouzuya.CommandLineOption.NamedOptionDefinition (withName)
import Bouzuya.CommandLineOption.OptionDefinition (arrayStringOption, booleanOption, maybeStringOption, stringOption, untyped)
import Bouzuya.CommandLineOption.OptionObject (OptionObject, getFirstValue, getValues, hasKey, parse)
import Bouzuya.CommandLineOption.OptionObject as OptionObject
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable as Foldable
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude (discard, map)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.CommandLineOption.OptionObject" do
  let
    f = parse { greedyArguments: false }
    f' = parse { greedyArguments: true }
    defs =
      [ withName "aString" (untyped (stringOption "a-string" (Just 'a') "<a>" "a string option" "a1"))
      , withName "bString" (untyped (maybeStringOption "b-string" (Just 'b') "<b>" "b string option" Nothing))
      , withName "cBoolean" (untyped (booleanOption "c-boolean" (Just 'c') "c boolean option"))
      , withName "dBoolean" (untyped (booleanOption "d-boolean" (Just 'd') "d boolean option"))
      , withName "eString" (untyped (arrayStringOption "e-string" (Just 'e') ["e1"] "e string option" []))
      ]
    defaults =
      o
        [ s "aString" "a1"
        , b "cBoolean" false
        , b "dBoolean" false
        , a "eString" []
        ]
    o :: Array (Tuple String (Maybe (Array String))) -> OptionObject
    o es =
      OptionObject.fromFoldable
        (Array.mapMaybe (\(Tuple k m) -> map (Tuple k) m) es)
    u :: OptionObject -> OptionObject -> OptionObject
    u = OptionObject.merge
    s :: String -> String -> Tuple String (Maybe (Array String))
    s k v = Tuple k (Just [v])
    b :: String -> Boolean -> Tuple String (Maybe (Array String))
    b k v = Tuple k (if v then Just [] else Nothing)
    a :: String -> Array String -> Tuple String (Maybe (Array String))
    a k v = Tuple k (Just v)
  suite "long (--foo bar)" do
    test "string option" do
      Assert.equal
        (Right
          { arguments: []
          , options: u (o [s "aString" "a1"]) defaults
          })
        (f defs [])
      Assert.equal
        (Right
          { arguments: []
          , options: u (o [s "aString" "a2"]) defaults
          })
        (f defs ["--a-string", "a2"])
      Assert.equal
        (Right
          { arguments: []
          , options: u (o [a "eString" ["e1", "e2"]]) defaults
          })
        (f defs ["--e-string", "e1", "--e-string", "e2"])
    test "boolean option" do
      Assert.equal
        (Right
          { arguments: []
          , options: u (o [b "cBoolean" false]) defaults
          })
        (f defs [])
      Assert.equal
        (Right
          { arguments: []
          , options: u (o [b "cBoolean" true]) defaults
          })
        (f defs ["--c-boolean"])
    test "string option and boolean option" do
      Assert.equal
        (Right
          { arguments: []
          , options:
              u
                (o
                  [ s "aString" "a2"
                  , b "cBoolean" true
                  ])
                defaults
          })
        (f
          defs
          [ "--a-string"
          , "a2"
          , "--c-boolean"
          ])
  suite "long (--foo=bar)" do
    test "string option" do
      Assert.equal
        (Right
          { arguments: []
          , options: u (o [s "aString" "a2"]) defaults
          })
        (f defs ["--a-string=a2"])
      Assert.equal
        (Right
          { arguments: []
          , options: u (o [a "eString" ["e1", "e2"]]) defaults
          })
        (f defs ["--e-string=e1", "--e-string=e2"])
    test "boolean option (ERROR)" do
      Assert.equal
        (Left "boolean option can't specify value") -- TODO: improve message
        (f defs ["--c-boolean=true"])
    test "string option and boolean option" do
      Assert.equal
        (Right
          { arguments: []
          , options:
              u
                (o
                  [ s "aString" "a2"
                  , b "cBoolean" true
                  ])
                defaults
          })
        (f
          defs
          [ "--a-string=a2"
          , "--c-boolean"
          ])
  suite "short (-f b)" do
    test "string option and boolean option" do
      Assert.equal
        (Right
          { arguments: []
          , options:
              u
                (o
                  [ s "aString" "a2"
                  , b "cBoolean" true
                  , a "eString" ["e1", "e2"]
                  ])
                defaults
          })
        (f
          defs
          [ "-a"
          , "a2"
          , "-c"
          , "-e"
          , "e1"
          , "-e"
          , "e2"
          ])
  suite "short (-f=b)" do
    test "string option and boolean option" do
      Assert.equal
        (Right
          { arguments: []
          , options:
              u
                (o
                  [ s "aString" "a2"
                  , b "cBoolean" true
                  , a "eString" ["e1", "e2"]
                  ])
                defaults
          })
        (f
          defs
          [ "-a=a2"
          , "-c"
          , "-e=e1"
          , "-e=e2"
          ])
  suite "short (-fg)" do
    test "string option and boolean option" do
      Assert.equal
        (Right
          { arguments: []
          , options:
              u
                (o
                  [ s "aString" "a2"
                  , b "cBoolean" true
                  , b "dBoolean" true
                  ])
                defaults
          })
        (f
          defs
          [ "-a=a2"
          , "-cd"
          ])
    test "-string=value (ERROR)" do
      Assert.equal
        (Left "-abc are boolean options") -- TODO: improve message
        (f defs ["-ab=string"])
    test "-boolean=value (ERROR)" do
      Assert.equal
        (Left "-abc=val is invalid format") -- TODO: improve message
        (f defs ["-cd=true"])
  suite "arguments" do
    test "arguments" do
      Assert.equal
        (Right { arguments: ["arg1", "arg2"], options: defaults })
        (f defs ["arg1", "arg2"])
    test "options arguments" do
      Assert.equal
        (Right
          { arguments: ["arg1", "arg2"]
          , options: u (o [ b "cBoolean" true]) defaults
          })
        (f defs ["-c", "arg1", "arg2"])
    test "arguments short option (ERROR)" do
      Assert.equal
        (Left "invalid option position") -- TODo: improve message
        (f defs ["arg1", "arg2", "-c"])
    test "arguments short options (ERROR)" do
      Assert.equal
        (Left "invalid option position") -- TODo: improve message
        (f defs ["arg1", "arg2", "-cd"])
  suite "other errors" do
    test "--unknown (ERROR)" do
      Assert.equal
        (Left "unknown option") -- TODO: improve message
        (f defs ["--unknown"])
    test "-u (ERROR)" do
      Assert.equal
        (Left "unknown option") -- TODO: improve message
        (f defs ["-u"])
    test "-cu (ERROR)" do
      Assert.equal
        (Left "unknown boolean option") -- TODO: improve message
        (f defs ["-cu"])
    test "no metavar (end) (ERROR)" do
      Assert.equal
        (Left "no metavar (end)") -- TODO: improve message
        (f defs ["--a-string"])
      Assert.equal
        (Left "no metavar (end)") -- TODO: improve message
        (f defs ["-a"])
    test "no metavar (next) (ERROR)" do
      Assert.equal
        (Left "no metavar (next)") -- TODO: improve message
        (f defs ["--a-string", "--c-boolean"])
      Assert.equal
        (Left "no metavar (next)") -- TODO: improve message
        (f defs ["-a", "--c-boolean"])
      Assert.equal
        (Left "no metavar (next)") -- TODO: improve message
        (f defs ["--a-string", "-c"])
      Assert.equal
        (Left "no metavar (next)") -- TODO: improve message
        (f defs ["-a", "-c"])
    test "--a-string a --a-string b (ERROR)" do
      Assert.equal
        (Left "many times") -- TODO: improve message
        (f defs ["--a-string", "a", "--a-string", "b"])
  test "getFirstValue" do
    Assert.equal Nothing (getFirstValue "unknown" defaults)
    Assert.equal (Just "a1") (getFirstValue "aString" defaults)
  test "getValues" do
    Assert.equal Nothing (getValues "unknown" defaults)
    Assert.equal (Just ["a1"]) (getValues "aString" defaults)
    Assert.equal
      (Just ["e1", "e2"])
      (getValues "eString" (u (o [a "eString" ["e1", "e2"]]) defaults))
  test "hasKey" do
    Assert.equal false (hasKey "unknown" defaults)
    Assert.equal true (hasKey "aString" defaults)

  suite "greedyArguments" do
    test "options" do
      let
        cases =
          [ Tuple
              { arguments: []
              , options: u (o [a "eString" ["e1", "e2"]]) defaults
              }
              ["--e-string", "e1", "--e-string", "e2"]
          , Tuple
              { arguments: []
              , options: u (o [a "eString" ["e1", "e2"]]) defaults
              }
              ["--e-string=e1", "--e-string=e2"]
          , Tuple
              { arguments: []
              , options: u (o [b "cBoolean" true]) defaults
              }
              ["--c-boolean"]
          , Tuple
              { arguments: []
              , options:
                  u
                    (o
                      [ s "aString" "a2"
                      , b "cBoolean" true
                      ])
                    defaults
              }
              [ "--a-string"
              , "a2"
              , "--c-boolean"
              ]
          , Tuple
              { arguments: []
              , options:
                  u
                    (o
                      [ s "aString" "a2"
                      , b "cBoolean" true
                      , a "eString" ["e1", "e2"]
                      ])
                    defaults
              }
              [ "-a"
              , "a2"
              , "-c"
              , "-e"
              , "e1"
              , "-e"
              , "e2"
              ]
          , Tuple
              { arguments: []
              , options:
                  u
                    (o
                      [ s "aString" "a2"
                      , b "cBoolean" true
                      , b "dBoolean" true
                      ])
                    defaults
              }
              [ "-a=a2"
              , "-cd"
              ]
          ]
      Foldable.for_ cases \(Tuple result args) -> do
        Assert.equal (Right result) (f' defs args)
    test "arguments" do
      let
        cases =
          [ Tuple
              (Right { arguments: ["arg1", "arg2"], options: defaults })
              ["arg1", "arg2"]
          , Tuple
              (Right
                { arguments: ["arg1", "arg2"]
                , options: u (o [ b "cBoolean" true]) defaults
                })
              ["-c", "arg1", "arg2"]
          , Tuple
              -- NOTE: greedyArguments: true
              (Right
                { arguments: ["arg1", "arg2", "-c"]
                , options: defaults
                })
              ["arg1", "arg2", "-c"]
          , Tuple
              -- NOTE: greedyArguments: true
              (Right
                { arguments: ["arg1", "arg2", "-cd"]
                , options: defaults
                })
              ["arg1", "arg2", "-cd"]
          , Tuple
              (Left "unknown option") -- TODO: improve message
              ["--unknown"]
          , Tuple
              (Left "unknown option") -- TODO: improve message
              ["-u"]
          , Tuple
              (Left "unknown boolean option") -- TODO: improve message
              ["-cu"]
          , Tuple
              (Left "no metavar (end)") -- TODO: improve message
              ["--a-string"]
          , Tuple
              (Left "no metavar (end)") -- TODO: improve message
              ["-a"]
          , Tuple
              (Left "no metavar (next)") -- TODO: improve message
              ["--a-string", "--c-boolean"]
          , Tuple
              (Left "no metavar (next)") -- TODO: improve message
              ["-a", "--c-boolean"]
          , Tuple
              (Left "no metavar (next)") -- TODO: improve message
              ["--a-string", "-c"]
          , Tuple
              (Left "no metavar (next)") -- TODO: improve message
              ["-a", "-c"]
          , Tuple
              (Left "many times") -- TODO: improve message
              ["--a-string", "a", "--a-string", "b"]
          ]
      Foldable.for_ cases \(Tuple expected args) -> do
        Assert.equal expected (f' defs args)
