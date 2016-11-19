module Gonimo.UI.Socket.Channel.Types where

import Control.Monad.Eff (Eff)
import Control.Monad.IO (IO)
import Data.Lens (lens, lens', LensP, Lens)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple))
import Gonimo.Client.Types (class ReportErrorAction, GonimoError, Settings)
import Gonimo.Server.Db.Entities (Family(Family), Device(Device))
import Gonimo.Server.State.Types (MessageNumber(MessageNumber))
import Gonimo.Server.Types (DeviceType)
import Gonimo.Types (Secret(Secret), Key(Key))
import Gonimo.UI.Socket.Message (Message)
import Partial.Unsafe (unsafeCrashWith)
import Prelude (Unit)
import Signal.Channel (Channel)
import WebRTC.MediaStream (MediaStream, MediaStreamConstraints(MediaStreamConstraints))
import WebRTC.RTC (MediaStreamEvent, IceEvent, RTCPeerConnection)
import Data.CatQueue (CatQueue)
import Data.List (List)

maxMessagesInFlight :: Int
maxMessagesInFlight = 2 -- Offers some performance advantage and hopefull is low enough to not cause dead locks on regular use.

type State =
  { mediaStream :: Maybe MediaStream
  , remoteStream :: Maybe RemoteStream
  , rtcConnection :: RTCPeerConnection
  , isBabyStation :: Boolean
  , messageQueue  :: List String
  , messagesInFlight :: Int
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
            | AcceptMessages MessageNumber (Array String)
            | StartStreaming MediaStreamConstraints
            | EnqueueMessage Message
            | MessageSent Action -- Notification that the ajax call for sending a message returned.
            | StartNegotiation
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

messagesInFlight :: forall a b r. Lens { "messagesInFlight" :: a | r } { "messagesInFlight" :: b | r } a b
messagesInFlight = lens _."messagesInFlight" (_ { "messagesInFlight" = _ })

messageQueue :: forall a b r. Lens { "messageQueue" :: a | r } { "messageQueue" :: b | r } a b
messageQueue = lens _."messageQueue" (_ { "messageQueue" = _ })
