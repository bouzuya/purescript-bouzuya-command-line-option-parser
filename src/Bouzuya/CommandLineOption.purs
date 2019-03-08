module Bouzuya.CommandLineOption
  ( class DefsToVals
  , module OptionDefinition
  , parse
  , parseWithOptions
  ) where

import Bouzuya.CommandLineOption.NamedOptionDefinition (NamedOptionDefinition)
import Bouzuya.CommandLineOption.ObjectToRecord (class OptionRecordBuilder)
import Bouzuya.CommandLineOption.ObjectToRecord as ObjectToRecord
import Bouzuya.CommandLineOption.OptionDefinition (OptionDefinition)
import Bouzuya.CommandLineOption.OptionDefinition (OptionDefinition, arrayStringOption, booleanOption, maybeStringOption, stringOption) as OptionDefinition
import Bouzuya.CommandLineOption.OptionObject as OptionObject
import Bouzuya.CommandLineOption.RecordToArray (class ArrayBuilder)
import Bouzuya.CommandLineOption.RecordToArray as RecordToArray
import Data.Either (Either, note)
import Prelude (bind, pure)
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Type.Row (class ListToRow)

type Parsed r =
  { arguments :: Array String
  , options :: Record r
  }

class DefsToVals (l1 :: RowList) (l2 :: RowList) | l1 -> l2
instance defsToValsCons :: DefsToVals t1 t2 => DefsToVals (Cons l (OptionDefinition a) t1) (Cons l a t2)
instance defsToValsNil :: DefsToVals Nil Nil

parse :: forall r1 r2 l1 l2 l3
   . RowToList r1 l1
  => ArrayBuilder l1 () r1 NamedOptionDefinition
  => RowToList r2 l2
  => OptionRecordBuilder l2 () r2
  => DefsToVals l1 l3
  => ListToRow l3 r2
  => Record r1
  -> Array String
  -> Either String (Parsed r2)
parse = parseWithOptions { greedyArguments: false }

parseWithOptions :: forall r1 r2 l1 l2 l3
   . RowToList r1 l1
  => ArrayBuilder l1 () r1 NamedOptionDefinition
  => RowToList r2 l2
  => OptionRecordBuilder l2 () r2
  => DefsToVals l1 l3
  => ListToRow l3 r2
  => { greedyArguments :: Boolean }
  -> Record r1
  -> Array String
  -> Either String (Parsed r2)
parseWithOptions parseOptions defs ss = do
  -- Typed* -> Named*
  ds <- pure (RecordToArray.toArray defs)
  -- Named* -> command-line options -> Object
  parsed <- OptionObject.parse parseOptions ds ss
  options <- note "toRecord error" (ObjectToRecord.toRecord parsed.options) -- Object -> Record
  pure { arguments: parsed.arguments, options }
