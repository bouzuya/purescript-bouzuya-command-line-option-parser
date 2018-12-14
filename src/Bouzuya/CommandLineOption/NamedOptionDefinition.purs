module Bouzuya.CommandLineOption.NamedOptionDefinition
  ( NamedOptionDefinition
  , getName
  , getOption
  , withName
  ) where

import Bouzuya.CommandLineOption.UntypedOptionDefinition (UntypedOptionDefinition)
import Prelude (class Eq)

type Name = String

data NamedOptionDefinition
  = NamedOptionDefinition Name UntypedOptionDefinition

derive instance eqNamedOptionDefinition :: Eq NamedOptionDefinition

getName :: NamedOptionDefinition -> String
getName (NamedOptionDefinition name _) = name

getOption :: NamedOptionDefinition -> UntypedOptionDefinition
getOption (NamedOptionDefinition _ o) = o

withName :: String -> UntypedOptionDefinition -> NamedOptionDefinition
withName = NamedOptionDefinition
