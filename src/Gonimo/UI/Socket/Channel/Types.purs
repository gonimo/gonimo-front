module Gonimo.UI.Socket.Channel.Types where

import Data.Maybe (Maybe(Just, Nothing))
import Gonimo.Client.Types (Settings)
import Gonimo.Server.DbEntities (Family(Family), Device(Device))
import Gonimo.Types (Secret(Secret), Key(Key))
import Gonimo.UI.Socket.Message (Message)
import Partial.Unsafe (unsafeCrashWith)
import WebRTC.MediaStream (MediaStream, MediaStreamConstraints(MediaStreamConstraints))
import WebRTC.RTC (RTCPeerConnection)


type State =
  { mediaStream :: Maybe MediaStream
  , rtcConnection :: RTCPeerConnection
  , isBabyStation :: Boolean
  }

type Props ps =
  { settings :: Settings
  , theirId :: Key Device
  , cSecret :: Secret
  , ourId :: Key Device
  , familyId :: Key Family
  }

data Action = AcceptMessage Message
            | StartStreaming MediaStreamConstraints
            | StopStreaming
            | SetMediaStream MediaStream
            | CloseConnection -- Say bye and tear down all connections!
            | Nop

unsafeMakeFamilyId :: Maybe (Key Family) -> Key Family
unsafeMakeFamilyId Nothing = unsafeCrashWith "We have to have a valid family id when using a channel, anything else is a but - therefore I am crashing now. Bye!"
unsafeMakeFamilyId (Just familyId') = familyId'
