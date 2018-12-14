module Bouzuya.CommandLineOption.ObjectToRecord
  ( class GetValue
  , class OptionRecordBuilder
  , builder
  , getValue
  , toRecord
  ) where

import Bouzuya.CommandLineOption.OptionObject (OptionObject)
import Bouzuya.CommandLineOption.OptionObject as OptionObject
import Data.Maybe (Maybe)
import Data.Symbol as Symbol
import Prelude (compose, identity, map, pure, (<$>), (<*>))
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))

class GetValue a where
  getValue :: String -> OptionObject -> Maybe a

instance getValueArrayString :: GetValue (Array String) where
  getValue = OptionObject.getValues

instance getValueBoolean :: GetValue Boolean where
  getValue k o = pure (OptionObject.hasKey k o)

instance getValueString :: GetValue String where
  getValue = OptionObject.getFirstValue

instance getValueMaybeString :: GetValue (Maybe String) where
  getValue k o = pure (OptionObject.getFirstValue k o)

class OptionRecordBuilder (list :: RowList) (from :: # Type) (to :: # Type)
  | list -> from to where
  builder ::
    RLProxy list
    -> OptionObject
    -> Maybe (Builder (Record from) (Record to))

instance optionRecordBuilderConsBoolean ::
  ( GetValue ty'
  , OptionRecordBuilder t from from'
  , Row.Lacks l from'
  , Row.Cons l ty' from' to
  , Symbol.IsSymbol l
  ) => OptionRecordBuilder (Cons l ty' t) from to where
  builder _ o = compose <$> (map (Builder.insert l) v) <*> (builder t o)
    where
      l = Symbol.SProxy :: Symbol.SProxy l
      k = Symbol.reflectSymbol l
      v = getValue k o
      t = RLProxy :: RLProxy t

instance optionRecordBuilderNil :: OptionRecordBuilder Nil () () where
  builder _ _ = pure identity

toRecord ::
  forall rows list
   . RowToList rows list
  => OptionRecordBuilder list () rows
  => OptionObject
  -> Maybe (Record rows)
toRecord obj = Builder.build <$> (builder list obj) <*> (pure {})
  where
    list = RLProxy :: RLProxy list
