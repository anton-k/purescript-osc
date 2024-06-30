module Network.Osc
  ( Port
  , Osc (..)
  , OscValue (..)
  , newWebsocketPort
  , oscToJson
  , oscFromJson
  ) where

import Prelude
import Effect (Effect)
import JSON (JSON)
import JSON as J
import JSON.Object as JO
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe (..))
import Data.Traversable (traverse, traverse_)
import Data.Show.Generic (genericShow)
import Data.Generic.Rep (class Generic)

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
  obj <- J.toJObject json
  ty <- J.toString =<< JO.lookup "type" obj
  val <- JO.lookup "value" obj
  case ty of
    "f" -> OscFloat <$> J.toNumber val
    "d" -> OscDouble <$> J.toNumber val
    "i" -> OscInt <$> J.toInt val
    "b" -> OscBoolean <$> J.toBoolean val
    _ -> Nothing

oscArgJson :: String -> JSON -> JSON
oscArgJson ty val = J.fromJObject $ JO.fromEntries
  [ Tuple "type" (J.fromString ty)
  , Tuple "value" val
  ]

foreign import data PortType :: Type

foreign import _oscNewWebSocketPort :: String -> Effect PortType
foreign import _oscPortOpen :: PortType -> Effect Unit
foreign import _oscPortSend :: PortType -> JSON -> Effect Unit
foreign import _oscPortOnMessage :: PortType -> (JSON -> Effect Unit) -> Effect Unit
