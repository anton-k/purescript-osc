module Network.Osc
  ( Port
  , Osc (..)
  , OscValue (..)
  , newWebsocketPort
  , oscToJson
  , oscFromJson
  , getOscPathArgs
  , getAddressPathArgs
  , OscCase
  , toOscCase
  , class ReadOsc
  , readOsc
  , oscArity
  , oscCase
  , oscCase_
  ) where

import Prelude
import Effect (Effect)
import JSON (JSON)
import JSON as J
import JSON.Object as JO
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Traversable (traverse, traverse_)
import Data.Show.Generic (genericShow)
import Data.Generic.Rep (class Generic)
import Data.String as String
import Data.Array as Array
import Data.Either (hush)
import Data.Int (round, toNumber)
import Type.Proxy (Proxy (..))

type Port =
  { send :: Osc -> Effect Unit
  , listen :: (Osc -> Effect Unit) -> Effect Unit
  }

type Osc =
  { address :: String
  , args :: Array OscValue
  }

data OscValue
  = OscFloat Number
  | OscDouble Number
  | OscInt Int
  | OscBoolean Boolean
  | OscString String

derive instance eqOscValue :: Eq OscValue
derive instance ordOscValue :: Ord OscValue
derive instance genericOscValue :: Generic OscValue _

instance showOscValue :: Show OscValue where
  show = genericShow

newWebsocketPort :: String -> Effect Port
newWebsocketPort url = do
  port <- _oscNewWebSocketPort url
  _oscPortOpen port
  pure
    { send: \msg -> _oscPortSend port (oscToJson msg)
    , listen: \call -> _oscPortOnMessage port (traverse_ call <<< oscFromJson)
    }

oscToJson :: Osc -> JSON
oscToJson osc =
  J.fromJObject $ JO.fromEntries
    [ Tuple "address" (J.fromString osc.address)
    , Tuple "args" (J.fromArray $ map oscValueToJson osc.args)
    ]

oscValueToJson :: OscValue -> JSON
oscValueToJson = case _ of
  OscFloat val -> oscArgJson "f" (J.fromNumber val)
  OscDouble val -> oscArgJson "d" (J.fromNumber val)
  OscInt val -> oscArgJson "i" (J.fromInt val)
  OscBoolean val -> oscArgJson "b" (J.fromBoolean val)
  OscString val -> oscArgJson "s" (J.fromString val)

oscFromJson :: JSON -> Maybe Osc
oscFromJson json = do
  obj <- J.toJObject json
  address <- J.toString =<< JO.lookup "address" obj
  args <- traverse oscValueFromJson =<< J.toArray =<< JO.lookup "args" obj
  pure
    { address: address
    , args: args
    }

oscValueFromJson :: JSON -> Maybe OscValue
oscValueFromJson json = do
  J.case_
    onUnit
    onBoolean
    onNumber
    onString
    onArray
    onObject
    json
    where
    onUnit = const Nothing
    onBoolean = Just <<< OscBoolean
    onNumber = Just <<< OscDouble
    onString = Just <<< OscString
    onArray = const Nothing
    onObject = const Nothing

oscArgJson :: String -> JSON -> JSON
oscArgJson ty val = J.fromJObject $ JO.fromEntries
  [ Tuple "type" (J.fromString ty)
  , Tuple "value" val
  ]

type OscCase a = Osc -> Maybe a

toOscCase :: forall a b . ReadOsc a => String -> (a -> b) -> OscCase b
toOscCase template act msg = do
  args <- getOscPathArgs template msg
  vals <- readOsc args
  pure (act vals)

oscCase :: forall a . Osc -> Array (OscCase a) -> Maybe a
oscCase msg cases = do
  headTail <- Array.uncons cases
  case headTail.head msg of
    Nothing -> oscCase msg headTail.tail
    Just eff -> Just eff

oscCase_ :: Osc -> Array (OscCase (Effect Unit)) -> (Osc -> Effect Unit) -> Effect Unit
oscCase_ msg cases def =
  fromMaybe (def msg) (oscCase msg cases)

-- | Class to parse typed arguments from the list of OScValue. Useful in the function @toOscCase@.
class ReadOsc a where
  readOsc :: Array OscValue -> Maybe a
  oscArity :: Proxy a -> Int

instance unitReadOsc :: ReadOsc Unit where
  readOsc = case _ of
    [] -> Just unit
    _ -> Nothing
  oscArity = const 0

instance intReadOsc :: ReadOsc Int where
  readOsc = case _ of
    [OscInt n] -> Just n
    [OscDouble d] -> Just (round d)
    _ -> Nothing
  oscArity = const 1

instance numberReadOsc :: ReadOsc Number where
  readOsc = case _ of
    [OscDouble d] -> Just d
    [OscFloat d] -> Just d
    [OscInt d] -> Just (toNumber d)
    _ -> Nothing
  oscArity = const 1

instance stringReadOsc :: ReadOsc String where
  readOsc = case _ of
    [OscString str] -> Just str
    [OscFloat a] -> Just (show a)
    [OscDouble a] -> Just (show a)
    [OscBoolean a] -> Just (show a)
    [OscInt a] -> Just (show a)
    _ -> Nothing
  oscArity = const 1

instance booleanReadOsc :: ReadOsc Boolean where
  readOsc = case _ of
    [OscBoolean str] -> Just str
    _ -> Nothing
  oscArity = const 1

instance pairReadOsc :: (ReadOsc a, ReadOsc b) => ReadOsc (Tuple a b) where
  oscArity _ = oscArity (Proxy :: Proxy a) + oscArity (Proxy :: Proxy b)
  readOsc vals = do
    a <- readOsc splited.before
    b <- readOsc splited.after
    pure (Tuple a b)
    where
      splited = Array.splitAt (oscArity (Proxy :: Proxy a)) vals

-- | Extracts captures from osc message address and appends args to it.
getOscPathArgs :: String -> Osc -> Maybe (Array OscValue)
getOscPathArgs template msg = do
  pathArgs <- getAddressPathArgs template msg.address
  pure (pathArgs <> msg.args)

-- | Matches the address of OSC message against template.
-- Template can contain plain strings or special characters to
-- capture arguments. The special charcacters are $oscTypeChar.
-- For example use $s to cature strings, or $d to capture doubles (numbers).
-- Note that due to underlying library restrictions all numeric types
-- including int are returned as OscDouble case of the OscValue.
--
-- For example let's capture string and boolean:
--
-- > getOscPathArgs "/foo/$s/$b" "/foo/mary/true == Just [OscString "mary", OscBoolean true]"
getAddressPathArgs :: String -> String -> Maybe (Array OscValue)
getAddressPathArgs template address =
  map Array.reverse $ matchParts [] (getParts template) (getParts address)
  where
    getParts = String.split (String.Pattern "/")

    matchParts result as bs =
      case Array.uncons as of
        Nothing ->
          case Array.uncons bs of
            Nothing -> Just result
            _ -> Nothing
        Just headTailA ->
          case Array.uncons bs of
            Nothing -> Nothing
            Just headTailB -> do
              val <- matchValue headTailA.head headTailB.head
              matchParts (val <> result) headTailA.tail headTailB.tail

    matchValue :: String -> String -> Maybe (Array OscValue)
    matchValue expect actual
      | expect == actual = Just []
      | isVal expect = map pure $ getValue expect actual
      | otherwise = Nothing

    isVal :: String -> Boolean
    isVal str =
        case String.uncons str of
          Just headTail -> headTail.head == String.codePointFromChar '$'
          Nothing -> false

    -- note that all numeric values are placed into OscDouble
    -- as input messages always contain only OscDouble for numbers
    getValue :: String -> String -> Maybe OscValue
    getValue templ val =
      case templ of
        "$f" -> OscDouble <$> readBy J.toNumber val
        "$d" -> OscDouble <$> readBy J.toNumber val
        "$i" -> OscDouble <$> readBy J.toNumber val
        "$s" -> Just (OscString val)
        "$b" -> OscBoolean <$> readBy J.toBoolean val
        _ -> Nothing

readBy :: forall a . (JSON -> Maybe a) -> String -> Maybe a
readBy extract str = extract =<< (hush $ J.parse str)

foreign import data PortType :: Type

foreign import _oscNewWebSocketPort :: String -> Effect PortType
foreign import _oscPortOpen :: PortType -> Effect Unit
foreign import _oscPortSend :: PortType -> JSON -> Effect Unit
foreign import _oscPortOnMessage :: PortType -> (JSON -> Effect Unit) -> Effect Unit
