module Gonimo.WebRTC where

import Prelude
import Data.Generic (class Generic)
import Data.Maybe (Maybe)
import Data.Nullable (toMaybe, toNullable, Nullable)
import WebRTC.RTC (RTCIceCandidate)

newtype IceCandidate = IceCandidate { sdpMLineIndex :: Maybe Int
                                    , sdpMid :: Maybe String
                                    , candidate :: String
                                    }

derive instance genericIceCandidate :: Generic IceCandidate

toJS :: IceCandidate -> RTCIceCandidate
toJS (IceCandidate icecandidate)= {
    candidate : icecandidate.candidate
  , sdpMid  : toNullable icecandidate.sdpMid
  , sdpMLineIndex : toNullable icecandidate.sdpMLineIndex
  }

fromJS :: RTCIceCandidate -> IceCandidate
fromJS icecandidate = IceCandidate
  { candidate : icecandidate.candidate
  , sdpMid  : toMaybe icecandidate.sdpMid
  , sdpMLineIndex : toMaybe icecandidate.sdpMLineIndex
  }

type SessionDescription = { sdp :: String, "type" :: String }

-- data Signal =
--     ICECandidate String
--   | Description String

-- data Error =
--   ParseError String -- A signaling message could not be parsed

-- type Config = {
--     settings :: Settings
--   , ourId :: DeviceId
--   , sendSignaling :: SendSignaling
--   , sendErrors :: ErrorReceiver
--   , iceServers :: Array { url :: String }
--   }

-- testICEServer = "stun:stun.l.google.com:19302"

-- type SendSignaling = String -> IO Unit
-- type ErrorReceiver = Error -> IO Unit

-- type Connection =
--   { config :: Config
--   , rtcConnection :: RTCPeerConnection
--   }


-- make :: Config -> IO Connection
-- make c = do
--   conn <- newRTCPeerConnection { iceServers : c.iceServers }
--   pure
--     { config : c
--     , rtcConnection : conn
--     }

-- addStream :: MediaStream -> Connection -> IO Unit
-- addStream stream conn = addStream conn.rtcConnection stream

-- receiveSignaling :: String -> Connection -> IO Unit
-- receiveSignal = unsafeCrashWith "Not yet implemented"


-- parseSignal :: String -> Either Error Signal


