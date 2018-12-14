module Bouzuya.CommandLineOption.UntypedOptionDefinition
  ( Help
  , LongName
  , MetaVar
  , Multiple
  , ShortName
  , UntypedOptionDefinition
  , getDefaultValue
  , getLongName
  , getShortName
  , isValueMultiple
  , isValueRequired
  , option
  ) where

import Data.Maybe (Maybe(..))
import Data.String (CodePoint)
import Data.String as String
import Data.String.CodeUnits as CodeUnit
import Prelude (class Eq, join, map)

type Help = String

type LongName = String

type MetaVar = String

type Multiple = Boolean

data UntypedOptionDefinition
  = UntypedOptionDefinition
      LongName
      (Maybe ShortName)
      Help
      Multiple
      (Array MetaVar)
      (Maybe (Array String))

derive instance eqOptionDefinition :: Eq UntypedOptionDefinition

type ShortName = CodePoint

charFromCodePoint :: CodePoint -> Maybe Char
charFromCodePoint cp = CodeUnit.charAt 0 (String.singleton cp)

getDefaultValue :: UntypedOptionDefinition -> Maybe (Array String)
getDefaultValue (UntypedOptionDefinition _ _ _ _ [] _) = Nothing
getDefaultValue (UntypedOptionDefinition _ _ _ _ _ v) = v

getLongName :: UntypedOptionDefinition -> String
getLongName (UntypedOptionDefinition long _ _ _ _ _) = long

getShortName :: UntypedOptionDefinition -> Maybe Char
getShortName (UntypedOptionDefinition _ short _ _ _ _)
  = join (map charFromCodePoint short)

isValueMultiple :: UntypedOptionDefinition -> Boolean
isValueMultiple (UntypedOptionDefinition _ _ _ b _ _) = b

isValueRequired :: UntypedOptionDefinition -> Boolean
isValueRequired (UntypedOptionDefinition _ _ _ _ [] _) = false
isValueRequired (UntypedOptionDefinition _ _ _ _ _ _) = true

option ::
  LongName
  -> Maybe Char
  -> Help
  -> Multiple
  -> Array MetaVar
  -> Maybe (Array String)
  -> UntypedOptionDefinition
option l s h m n v =
  UntypedOptionDefinition l (map String.codePointFromChar s) h m n v
