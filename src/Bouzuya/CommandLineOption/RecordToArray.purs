module Bouzuya.CommandLineOption.RecordToArray
  ( class ArrayBuilder
  , class ToElement
  , build
  , toArray
  , toElement
  ) where

import Data.Array as Array
import Data.Symbol (SProxy(..), reflectSymbol)
import Data.Symbol as Symbol
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record as Record
import Type.Data.RowList (RLProxy(..))

class ArrayBuilder (list :: RowList) (from :: # Type) (to :: # Type) (ty :: Type)
  | list -> from to where
  build :: RLProxy list -> Record to -> Array ty -> Array ty

instance arrayBuilderCons ::
  ( ArrayBuilder t from from' ty'
  , Row.Cons l ty from' to
  , Row.Lacks l from'
  , Symbol.IsSymbol l
  , ToElement ty ty'
  ) => ArrayBuilder (Cons l ty t) from to ty' where
  build _ o a =
    build t (Record.delete l o) (Array.snoc a (toElement k v))
    where
      l = SProxy :: SProxy l
      k = reflectSymbol l
      v = Record.get l o
      t = RLProxy :: RLProxy t

instance arrayBuilderNil :: ArrayBuilder Nil () () ty' where
  build _ _ a = a

class ToElement a b where
  toElement :: String -> a -> b

toArray ::
  forall rows list ty
   . RowToList rows list
  => ArrayBuilder list () rows ty
  => Record rows
  -> Array ty
toArray obj = build list obj []
  where
    list = RLProxy :: RLProxy list
