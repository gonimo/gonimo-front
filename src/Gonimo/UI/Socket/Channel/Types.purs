module Gonimo.UI.Socket.Channel.Types where

import Control.Monad.Eff (Eff)
import Control.Monad.IO (IO)
import Data.Lens (lens, lens', LensP)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple))
import Gonimo.Client.Types (class ReportErrorAction, GonimoError, Settings)
import Gonimo.Server.DbEntities (Family(Family), Device(Device))
import Gonimo.Server.Types (DeviceType)
import Gonimo.Types (Secret(Secret), Key(Key))
import Gonimo.UI.Socket.Message (Message)
import Partial.Unsafe (unsafeCrashWith)
import Prelude (Unit)
import Signal.Channel (Channel)
import WebRTC.MediaStream (MediaStream, MediaStreamConstraints(MediaStreamConstraints))
import WebRTC.RTC (MediaStreamEvent, IceEvent, RTCPeerConnection)


type State =
  { mediaStream :: Maybe MediaStream
  , remoteStream :: Maybe RemoteStream
  , rtcConnection :: RTCPeerConnection
  , isBabyStation :: Boolean
  }

type Props ps =
  { settings :: Settings
  , theirId :: Key Device
  , cSecret :: Secret
  , ourId :: Key Device
  , familyId :: Key Family
  , sendAction :: Action -> Eff () Unit
  , onlineDevices :: Array (Tuple (Key Device) DeviceType)
  }

data Action = InitConnection
            | AcceptMessage Message
            | StartStreaming MediaStreamConstraints
            | StopStreaming
            | SetMediaStream MediaStream
            | CloseConnection -- Say bye and tear down all connections!
            | ReportError GonimoError
            | OnIceCandidate IceEvent
            | OnAddStream MediaStreamEvent
            | SetRemoteStream RemoteStream
            | Nop

instance reportErrorAction :: ReportErrorAction Action where
  reportError = ReportError

type RemoteStream = { stream :: MediaStream
                    , objectURL :: String
                    }

unsafeMakeFamilyId :: Maybe (Key Family) -> Key Family
unsafeMakeFamilyId Nothing = unsafeCrashWith "We have to have a valid family id when using a channel, anything else is a but - therefore I am crashing now. Bye!"
unsafeMakeFamilyId (Just familyId') = familyId'

remoteStream :: LensP State (Maybe RemoteStream)
remoteStream = lens _.remoteStream (_ { remoteStream = _})
