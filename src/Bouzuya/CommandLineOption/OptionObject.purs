module Bouzuya.CommandLineOption.OptionObject
  ( OptionObject
  , ParsedOption
  , fromFoldable -- TODO: remove
  , getFirstValue
  , getValues
  , hasKey
  , merge -- TODO: remove
  , parse
  ) where

import Bouzuya.CommandLineOption.NamedOptionDefinition (NamedOptionDefinition, getName, getOption)
import Bouzuya.CommandLineOption.UntypedOptionDefinition (getDefaultValue, getLongName, getShortName, isValueMultiple, isValueRequired)
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Foldable (class Foldable, foldl, foldM, foldlDefault)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String (CodePoint)
import Data.String as String
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Prelude (class Eq, class Functor, class Show, Unit, bind, const, discard, map, not, pure, show, unit, (<>), (==), (>>=), (||))

data OptionName = Long String | Short CodePoint

-- case lookup k o of
--   Nothing -> ...  -- Boolean: false / String: ERROR / Maybe String: Nothing
--   Just [] -> ...  -- Boolean: true  / String: ERROR / Maybe String: ERROR
--   Just [v] -> ... -- Boolean: ERROR / String: v     / Maybe String: Just v
--   Just _ -> ...   -- (TODO) Boolean: ERROR / String: ERROR / Maybe String: ERROR
newtype OptionObject = OptionObject (Object (Array String))

derive instance eqOptionObject :: Eq OptionObject

instance showOptionObject :: Show OptionObject where
  show (OptionObject o) = "(OptionObject " <> show o <> ")"

type ParsedOption = { arguments :: Array String, options :: OptionObject }

addBooleanOptionValue ::
  NamedOptionDefinition
  -> OptionObject
  -> Either String OptionObject
addBooleanOptionValue d o = insertOrUpdate d [] o

addStringOptionValue ::
  NamedOptionDefinition
  -> String
  -> OptionObject
  -> Either String OptionObject
addStringOptionValue d v o = insertOrUpdate d [v] o

assert' :: String -> Boolean -> Either String Unit
assert' s b = if b then Right unit else Left s

defaultValues :: Array NamedOptionDefinition -> OptionObject
defaultValues defs =
  OptionObject
    (foldlDefault
      (\o d -> Object.alter (const (getDefaultValue (getOption d))) (getName d) o)
      Object.empty
      defs)

findByOptionName :: OptionName -> Array NamedOptionDefinition -> Maybe NamedOptionDefinition
findByOptionName (Long l) =
  Array.find (\d -> getLongName (getOption d) == l)
findByOptionName (Short s) =
  Array.find (\d -> map String.codePointFromChar (getShortName (getOption d)) == Just s)

-- TODO: remove
fromFoldable ::
  forall f
   . Foldable f
  => Functor f
  => f (Tuple String (Array String))
  -> OptionObject
fromFoldable f =
  OptionObject (Object.fromFoldable (map g f))
  where
    g (Tuple k v) = Tuple k v

getFirstValue :: String -> OptionObject -> Maybe String
getFirstValue k (OptionObject o) = Object.lookup k o >>= Array.head

getValues :: String -> OptionObject -> Maybe (Array String)
getValues k (OptionObject o) = Object.lookup k o

hasKey :: String -> OptionObject -> Boolean
hasKey k (OptionObject o) = Object.member k o

-- insert or update (append)
insertOrUpdate ::
  NamedOptionDefinition
  -> Array String
  -> OptionObject
  -> Either String OptionObject
insertOrUpdate d v (OptionObject o) = do
  let k = getName d
  assert' "many times" (isValueMultiple (getOption d) || not (Object.member k o))
  pure (OptionObject (Object.alter (\m -> Just ((fromMaybe [] m) <> v)) k o))

parse ::
  { greedyArguments :: Boolean }
  -> Array NamedOptionDefinition
  -> Array String
  -> Either String ParsedOption
parse { greedyArguments } defs ss = do
  { arguments, options, processing } <-
    foldM
      f
      { arguments: []
      , endOptions: false
      , options: OptionObject Object.empty
      , processing: Nothing
      }
      ss
  _ <- assert' "no metavar (end)" (isNothing processing)
  Right { arguments, options: merge options (defaultValues defs) }
  where
    f a@{ arguments, endOptions: true, options } s =
      Right a { arguments = Array.snoc arguments s }
    f a@{ arguments, endOptions: false, options, processing: Nothing } s =
      case parse' s of
        [] -> -- foo
          -- TODO: long == "" -- double hyphen (--) support
          Right
            (a
              { arguments = Array.snoc arguments s
              , endOptions = greedyArguments
              })
        [{ name, value: valueMaybe }] -> do -- -a or --abc
          _ <- assert' "invalid option position" (Array.null arguments)
          def <- note "unknown option" (findByOptionName name defs)
          case valueMaybe of
            Just value ->
              if isValueRequired (getOption def) then do
                o <- addStringOptionValue def value options
                Right a { options = o }
              else
                Left "boolean option can't specify value" -- TODO: add option name
            Nothing ->
              if isValueRequired (getOption def) then
                Right a { processing = Just def }
              else do
                o <- addBooleanOptionValue def options
                Right a { options = o }
        shortOptions -> do -- -abc
          _ <- assert' "invalid option position" (Array.null arguments)
          foldM
            (\a'@{ options: options' } { name, value: valueMaybe } -> do
              def <- note "unknown boolean option" (findByOptionName name defs) -- TODO: add option name
              _ <- assert' "-abc are boolean options" (not (isValueRequired (getOption def))) -- TODO: add option names
              _ <- assert' "-abc=val is invalid format" (isNothing valueMaybe) -- TODO: add option
              o <- addBooleanOptionValue def options'
              Right a' { options = o })
            a
            shortOptions
    f a@{ options, endOptions: false, processing: Just def } s =
      case parse' s of
        [] -> do
          o <- addStringOptionValue def s options
          Right
            a
              { options = o
              , processing = Nothing
              }
        _ ->
          Left "no metavar (next)"

parse' :: String -> Array { name :: OptionName, value :: Maybe String }
parse' s =
  let
    hyphen = String.Pattern "-"
    -- "foo" -> { name: "foo", value: Nothing }
    -- "foo=" -> { name: "foo", value: Just "" }
    -- "foo=bar" -> { name: "foo", value: Just "bar" }
    split s' =
      case String.indexOf (String.Pattern "=") s' of
        Nothing -> { name: s', value: Nothing }
        Just index ->
          let { after, before } = String.splitAt index s'
          in { name: before, value: Just (String.drop 1 after) }
  in
    case String.stripPrefix hyphen s of
      Nothing -> []
      Just s' ->
        case String.stripPrefix hyphen s' of
          Just s'' ->
            let { name, value } = split s''
            in [{ name: Long name, value }]
          _ ->
            let { name, value } = split s'
            in
              map
                (\cp -> { name: Short cp, value })
                (String.toCodePointArray name)

-- TODO: remove
merge :: OptionObject -> OptionObject -> OptionObject
merge (OptionObject o1) (OptionObject o2) =
  OptionObject
    (foldl
      (\o k ->
        case Object.lookup k o1 of
          Nothing -> o
          Just v -> Object.insert k v o)
      o2
      (Object.keys o1))
