module Gonimo.WebRTC where

import Prelude
import Data.Generic (class Generic)
import Data.Maybe (Maybe)
import Data.Nullable (toMaybe, toNullable, Nullable)


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


