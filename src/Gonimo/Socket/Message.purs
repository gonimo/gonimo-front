module Gonimo.Socket.Message where

import Prelude
import Data.Argonaut.Generic.Aeson as Aeson
import Gonimo.Util as Util
import Data.Generic (class Generic)
import Data.Maybe (Maybe)
import Gonimo.WebRTC (SessionDescription, IceCandidate(..))

data Message =
-- https://github.com/purescript/purescript/issues/1443 :
--    SessionDescription SessionDescription
    SessionDescription { sdp :: String, "type" :: String }
  | IceCandidate IceCandidate
-- Indicate the other party that we want to close the connection.
-- After sending this message we will close the connection, the other party may do
-- the same. In any case, after receiving this message the other party knows that the connection
-- is lost on purpose.
  | CloseConnection

derive instance genericMessage :: Generic Message

encodeToString :: Message -> String
encodeToString = show <<< Aeson.encodeJson

decodeFromString :: String -> Maybe Message
decodeFromString = Util.fromString
