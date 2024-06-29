module Network.Osc
  ( Port
  , Osc (..)
  , OscValue (..)
  , newWebsocketPort
  , oscToJson
  ) where

import Prelude
import Effect (Effect)
import JSON
import JSON.Object as JO
import JSON.Array as JA
import Data.Tuple (Tuple(..))

type Port =
  { send :: Osc -> Effect Unit
  }

type Osc =
  { address :: String
  , args :: Array OscValue
  }

data OscValue
  = OscFloat Number
  | OscInt Int
  | OscBoolean Boolean
  | OscString String

newWebsocketPort :: String -> Effect Port
newWebsocketPort url = do
  port <- _oscNewWebSocketPort url
  _oscPortOpen port
  pure
    { send: \msg -> _oscPortSend port (oscToJson msg)
    }

oscToJson :: Osc -> JSON
oscToJson osc =
  fromJObject $ JO.fromEntries
    [ Tuple "address" (fromString osc.address)
    , Tuple "args" (fromArray $ map oscValueToJson osc.args)
    ]

oscValueToJson :: OscValue -> JSON
oscValueToJson = case _ of
  OscFloat val -> oscArgJson "f" (fromNumber val)
  OscInt val -> oscArgJson "i" (fromInt val)
  OscBoolean val -> oscArgJson "b" (fromBoolean val)
  OscString val -> oscArgJson "s" (fromString val)

oscArgJson :: String -> JSON -> JSON
oscArgJson ty val = fromJObject $ JO.fromEntries
  [ Tuple "type" (fromString ty)
  , Tuple "value" val
  ]

foreign import data PortType :: Type

foreign import _oscNewWebSocketPort :: String -> Effect PortType
foreign import _oscPortOpen :: PortType -> Effect Unit
foreign import _oscPortSend :: PortType -> JSON -> Effect Unit
