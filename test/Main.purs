module Test.Main
  ( main
  ) where

import Effect (Effect)
import Prelude (Unit)
import Test.Bouzuya.CommandLineOption as CommandLineOption
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  CommandLineOption.tests
