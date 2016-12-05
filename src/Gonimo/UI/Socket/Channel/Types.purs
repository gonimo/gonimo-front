module Gonimo.UI.Socket.Channel.Types where

import Control.Monad.Eff (Eff)
import Control.Monad.IO (IO)
import DOM.Event.Types (Event)
import Data.CatQueue (CatQueue)
import Data.Lens (lens, lens', LensP, Lens)
import Data.List (List)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple))
import Gonimo.Client.Types (class ReportErrorAction, GonimoError, Settings)
import Gonimo.Client.Vibrations (Vibrator)
import Gonimo.Server.Db.Entities (Family(Family), Device(Device))
import Gonimo.Server.State.Types (MessageNumber(MessageNumber))
import Gonimo.Server.Types (DeviceType)
import Gonimo.Types (Secret(Secret), Key(Key))
import Gonimo.UI.Socket.Message (Message)
import Gonimo.Util (Audio)
import Partial.Unsafe (unsafeCrashWith)
import Prelude (Unit)
import Signal.Channel (Channel)
import WebRTC.MediaStream (MediaStream, MediaStreamConstraints(MediaStreamConstraints))
import WebRTC.RTC (MediaStreamEvent, IceEvent, RTCPeerConnection)

maxMessagesInFlight :: Int
maxMessagesInFlight = 2 -- Offers some performance advantage and hopefull is low enough to not cause dead locks on regular use.

type State =
  { mediaStream :: Maybe MediaStream
  , remoteStream :: Maybe RemoteStream
  , rtcConnection :: RTCPeerConnection
  , isBabyStation :: Boolean
  , messageQueue  :: List String
  , messagesInFlight :: Int
  , audioStats    :: StreamConnectionStats
  , videoStats    :: StreamConnectionStats
  , vibrator      :: Maybe Vibrator
  , alarm         :: Audio
  , alarmIsOn     :: Boolean
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
            | RegisterMediaTracks (Array TrackKind) -- We received a stream containing an audio/video track
            | OnAudioConnectionDrop (Maybe Int)
            | OnVideoConnectionDrop (Maybe Int)
            | SetRemoteStream RemoteStream
            | SetVibrator (Maybe Vibrator)
            | TurnOnAlarm Boolean
            | ConnectionClosed -- Message to parent, when channel gets closed
            | Nop

instance reportErrorAction :: ReportErrorAction Action where
  reportError = ReportError

type RemoteStream = { stream :: MediaStream
                    , objectURL :: String
                    }

data StreamConnectionState = ConnectionNotAvailable -- not available or stopped by user.
                           | ConnectionUnknown -- It got registered but is not yet known to be really alive.
                           | ConnectionReliable
                           | ConnectionDied

type StreamConnectionStats = { packetsReceived :: Int
                             , connectionState :: StreamConnectionState
                             }

data TrackKind = AudioTrack | VideoTrack
unsafeMakeFamilyId :: Maybe (Key Family) -> Key Family
unsafeMakeFamilyId Nothing = unsafeCrashWith "We have to have a valid family id when using a channel, anything else is a bug - therefore I am crashing now. Bye!"
unsafeMakeFamilyId (Just familyId') = familyId'

remoteStream :: LensP State (Maybe RemoteStream)
remoteStream = lens _.remoteStream (_ { remoteStream = _})

messagesInFlight :: forall a b r. Lens { "messagesInFlight" :: a | r } { "messagesInFlight" :: b | r } a b
messagesInFlight = lens _."messagesInFlight" (_ { "messagesInFlight" = _ })

messageQueue :: forall a b r. Lens { "messageQueue" :: a | r } { "messageQueue" :: b | r } a b
messageQueue = lens _."messageQueue" (_ { "messageQueue" = _ })

audioStats :: forall a b r. Lens { "audioStats" :: a | r } { "audioStats" :: b | r } a b
audioStats = lens _."audioStats" (_ { "audioStats" = _ })

videoStats :: forall a b r. Lens { "videoStats" :: a | r } { "videoStats" :: b | r } a b
videoStats = lens _."videoStats" (_ { "videoStats" = _ })

packetsReceived :: forall a b r. Lens { "packetsReceived" :: a | r } { "packetsReceived" :: b | r } a b
packetsReceived = lens _."packetsReceived" (_ { "packetsReceived" = _ })

connectionState :: forall a b r. Lens { "connectionState" :: a | r } { "connectionState" :: b | r } a b
connectionState = lens _."connectionState" (_ { "connectionState" = _ })

vibrator :: forall a b r. Lens { "vibrator" :: a | r } { "vibrator" :: b | r } a b
vibrator = lens _."vibrator" (_ { "vibrator" = _ })
