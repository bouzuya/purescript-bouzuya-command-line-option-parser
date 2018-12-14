module Test.Bouzuya.CommandLineOption.RecordToArray
  ( tests
  ) where

import Bouzuya.CommandLineOption (OptionDefinition, booleanOption, maybeStringOption, stringOption)
import Bouzuya.CommandLineOption.NamedOptionDefinition (NamedOptionDefinition, withName)
import Bouzuya.CommandLineOption.RecordToArray (toArray)
import Bouzuya.CommandLineOption.OptionDefinition (untyped)
import Data.Maybe (Maybe(..))
import Prelude ((==))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.CommandLineOption.RecordToArray" do
  let
    f = toArray

    defRecord ::
      { inFile :: OptionDefinition (Maybe String)
      , inFormat :: OptionDefinition String
      , outFormat :: OptionDefinition String
      , verbose :: OptionDefinition Boolean
      , version :: OptionDefinition Boolean
      }
    defRecord =
      { inFile: maybeStringOption "in-file" (Just 'f') "<file>" "input file" Nothing
      , inFormat: stringOption "in-format" (Just 'i') "<format>" "input file format" "json"
      , outFormat: stringOption "out-format" (Just 'o') "<format>" "output file format" "json"
      , verbose: booleanOption "verbose" (Just 'v') "verbose"
      , version: booleanOption "version" (Just 'V') "show version"
      }

    defArray :: Array NamedOptionDefinition
    defArray =
      [ withName "inFile" (untyped defRecord.inFile)
      , withName "inFormat" (untyped defRecord.inFormat)
      , withName "outFormat" (untyped defRecord.outFormat)
      , withName "verbose" (untyped defRecord.verbose)
      , withName "version" (untyped defRecord.version)
      ]

  test "example" do
    Assert.assert "==" (defArray == (f defRecord))
