module Gonimo.UI.Socket.Message where

import Prelude
import Data.Argonaut.Generic.Aeson as Aeson
import Gonimo.Util as Util
import Data.Generic (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import WebRTC.RTC (RTCIceCandidateInit)

data Message =
    StartStreaming -- Ask a baby station to start streaming (Will be ignored by parent stations).
-- https://github.com/purescript/purescript/issues/1443 :
--    SessionDescription SessionDescription
  | SessionDescriptionOffer { sdp :: String, "type" :: String }
  | SessionDescriptionAnswer { sdp :: String, "type" :: String }
  | IceCandidate MaybeIceCandidate
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

-- Necessary for Generic deriving ...
data MaybeIceCandidate = NoIceCandidate
                       | JustIceCandidate { sdpMLineIndex :: Maybe Int
                                          , sdpMid :: Maybe String
                                          , candidate :: String
                                          }

derive instance genericMaybeIceCandidate :: Generic MaybeIceCandidate

runMaybeIceCandidate :: MaybeIceCandidate -> Maybe RTCIceCandidateInit
runMaybeIceCandidate NoIceCandidate = Nothing
runMaybeIceCandidate (JustIceCandidate candidate) = Just candidate
