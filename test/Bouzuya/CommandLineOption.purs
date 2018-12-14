module Test.Bouzuya.CommandLineOption
  ( tests
  ) where

import Bouzuya.CommandLineOption (arrayStringOption, booleanOption, maybeStringOption, parse, stringOption)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Prelude (discard)
import Test.Bouzuya.CommandLineOption.ObjectToRecord as ObjectToRecord
import Test.Bouzuya.CommandLineOption.OptionDefinition as OptionDefinition
import Test.Bouzuya.CommandLineOption.OptionObject as OptionObject
import Test.Bouzuya.CommandLineOption.RecordToArray as RecordToArray
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.CommandLineOption" do
  test "usage" do
    let
      defs =
        { s: stringOption "str" (Just 's') "<STRING>" "string option" "default"
        , b: booleanOption "bool" (Just 'b') "boolean option"
        , m: maybeStringOption "maybe" (Just 'm') "<MAYBE>" "maybe option" (Just "maybe default")
        , a: arrayStringOption "array" (Just 'a') ["<A1>", "<A2>"] "array option" ["a1", "a2"]
        }
    Assert.equal
      (Right
        { arguments: ["foo", "bar"]
        , options: { s: "a", b: true, m: Just "c", a: ["a1", "a2"] }
        })
      (parse defs ["--str", "a", "-b", "-m", "c", "foo", "bar"])
    Assert.equal
      (Right
        { arguments: []
        , options: { s: "a", b: false, m: Just "maybe default", a: ["a1", "a2"] }
        })
      (parse defs ["--str", "a"])
    Assert.equal
      (Right
        { arguments: []
        , options: { s: "default", b: false, m: Just "maybe default", a: ["Hello", "World"] }
        })
      (parse defs ["-a", "Hello", "-a", "World"])
  ObjectToRecord.tests
  OptionDefinition.tests
  OptionObject.tests
  RecordToArray.tests
