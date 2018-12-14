module Bouzuya.CommandLineOption.OptionDefinition
  ( OptionDefinition
  , arrayStringOption
  , booleanOption
  , maybeStringOption
  , stringOption
  , untyped -- private
  ) where

import Bouzuya.CommandLineOption.NamedOptionDefinition (NamedOptionDefinition, withName)
import Bouzuya.CommandLineOption.RecordToArray as RecordToArray
import Bouzuya.CommandLineOption.UntypedOptionDefinition (Help, LongName, UntypedOptionDefinition, MetaVar, option)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Prelude (map)

instance toElementArrayString ::
  RecordToArray.ToElement
    (OptionDefinition (Array String))
    NamedOptionDefinition where
  toElement name a = withName name (untyped a)

instance toElementBoolean ::
  RecordToArray.ToElement
    (OptionDefinition Boolean)
    NamedOptionDefinition where
  toElement name a = withName name (untyped a)

instance toElementMaybeString ::
  RecordToArray.ToElement
    (OptionDefinition (Maybe String))
    NamedOptionDefinition where
  toElement name a = withName name (untyped a)

instance toElementString ::
  RecordToArray.ToElement
    (OptionDefinition String)
    NamedOptionDefinition where
  toElement name a = withName name (untyped a)

data OptionDefinition a
  = OptionDefinition UntypedOptionDefinition

arrayStringOption ::
  LongName
  -> Maybe Char
  -> Array MetaVar
  -> Help
  -> Array String
  -> OptionDefinition (Array String)
arrayStringOption l s m h v = OptionDefinition (option l s h true m (Just v))

booleanOption :: LongName -> Maybe Char -> Help -> OptionDefinition Boolean
booleanOption l s h = OptionDefinition (option l s h false [] Nothing)

untyped :: forall a. OptionDefinition a -> UntypedOptionDefinition
untyped (OptionDefinition d) = d

maybeStringOption ::
  LongName
  -> Maybe Char
  -> MetaVar
  -> Help
  -> Maybe String
  -> OptionDefinition (Maybe String)
maybeStringOption l s m h v =
  OptionDefinition (option l s h false [m] (map Array.singleton v))

stringOption ::
  LongName
  -> Maybe Char
  -> MetaVar
  -> Help
  -> String
  -> OptionDefinition String
stringOption l s m h v = OptionDefinition (option l s h false [m] (Just [v]))
