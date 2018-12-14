module Test.Bouzuya.CommandLineOption.ObjectToRecord
  ( tests
  ) where

import Bouzuya.CommandLineOption.ObjectToRecord (class GetValue, toRecord)
import Bouzuya.CommandLineOption.OptionObject as OptionObject
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude (class Eq, class Show, map, show, (>>=))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

newtype MyInt = MyInt Int

derive instance eqMyInt :: Eq MyInt

instance getValueString :: GetValue MyInt where
  getValue k o = OptionObject.getFirstValue k o >>= fromString

instance showMyInt :: Show MyInt where
  show (MyInt i) = show i

fromString :: String -> Maybe MyInt
fromString s = map MyInt (Int.fromString s)

tests :: TestSuite
tests = suite "Bouzuya.CommandLineOption.ObjectToRecord" do
  test "String" do
    let
      obj =
        OptionObject.fromFoldable
          [ Tuple "k1" ["123"]
          ]
    Assert.equal
      (Just { k1: MyInt 123 })
      (toRecord obj)
