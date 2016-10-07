module Gonimo.Socket.Types where


import Prelude
import Data.Map as Map
import Gonimo.Socket.Connection as ConnectionC
import Control.Monad.Reader (runReader)
import Control.Monad.Reader.Class (class MonadReader)
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Maybe (maybe, fromMaybe, Maybe)
import Data.Monoid (mempty)
import Data.Tuple (uncurry, snd, Tuple(Tuple))
import Gonimo.Client.Types (Settings)
import Gonimo.Pux (noEffects)
import Gonimo.Server.DbEntities (Family(Family), Device(Device))
import Gonimo.Socket.Message (Message)
import Gonimo.Types (Secret(Secret), Key(Key))
import Gonimo.WebAPI.Subscriber (receiveSocketByFamilyIdByToDevice, receiveSocketByFamilyIdByFromDeviceByToDeviceByChannelId)
import Servant.Subscriber (Subscriptions)
import WebRTC.MediaStream (MediaStreamConstraints(MediaStreamConstraints), MediaStream)
import WebRTC.RTC (newRTCPeerConnection, RTCPeerConnection)

type State =
  { connections :: Map Secret Connection
  }


type Connection = { theirId :: Key Device
                  , connState :: ConnectionC.State
                  }

type Props ps = { settings :: Settings
                , acceptConnections :: Boolean
                , deviceId :: Key Device
                , familyId :: Maybe (Key Family)
                }

data Action = AcceptConnection (Key Device) Secret
            | AddConnection ConnectionC.State
              -- Connection actions for a given sender and channel id
            | ConnectionA (Key Device) Secret ConnectionC.Action
            | Nop

