module Gonimo.Socket where

import Prelude
import Control.Monad.Reader (runReader)
import Control.Monad.Reader.Class (class MonadReader)
import Data.Foldable (foldl)
import Data.Maybe (maybe, fromMaybe, Maybe)
import Data.Monoid (mempty)
import Data.Tuple (uncurry, snd, Tuple(Tuple))
import Gonimo.Client.Types (Settings)
import Gonimo.Pux (noEffects, EffModel(EffModel))
import Gonimo.Server.DbEntities (Family(Family), Device(Device))
import Gonimo.Types (Secret(Secret), Key(Key))
import Gonimo.WebAPI.Subscriber (receiveSocketByFamilyIdByToDevice, receiveSocketByFamilyIdByFromDeviceByToDeviceByChannelId)
import Servant.Subscriber (Subscriptions)
import WebRTC.MediaStream (MediaStream)
import WebRTC.RTC (RTCPeerConnection)

type State =
  {
    connections :: Array (Tuple (Key Device) Connection)
  }

type Props ps = { settings :: Settings
                , acceptConnections :: Boolean
                , deviceId :: Key Device
                , familyId :: Maybe (Key Family)
                }

type Connection =
  { ourId :: Key Device
  , theirId :: Key Device
  , channel :: Secret
  , mediaStream :: MediaStream
  , rtcConnection :: RTCPeerConnection
  }


data Action = AcceptConnection (Key Device) Secret
            | AcceptMessage (Key Device) String
            | Nop


update :: forall eff ps. Props ps -> Action -> State -> EffModel eff State Action
update props action = case action of
  AcceptConnection theirId channelId -> noEffects
  AcceptMessage theirId message      -> noEffects
  Nop                                -> noEffects

getSubscriptions :: forall ps. Props ps -> State -> Subscriptions Action
getSubscriptions props state = fromMaybe mempty $ getSubscriptions' props state <$> props.familyId

getSubscriptions' :: forall ps. Props ps -> State -> (Key Family) -> Subscriptions Action
getSubscriptions' props state familyId =
  let
    receiveMessage = receiveSocketByFamilyIdByFromDeviceByToDeviceByChannelId
    receiveChannel = receiveSocketByFamilyIdByToDevice

    connectionToSubscriptions :: forall m. (MonadReader Settings m) => Connection -> m (Subscriptions Action)
    connectionToSubscriptions conn =
      receiveMessage (maybe Nop (AcceptMessage conn.theirId)) familyId conn.theirId conn.ourId conn.channel

    subArray :: Array (Subscriptions Action)
    subArray = map (flip runReader props.settings)
               $ map (connectionToSubscriptions <<< snd) state.connections
               <> [ receiveChannel (maybe Nop (uncurry AcceptConnection)) familyId props.deviceId ]
  in
     foldl append mempty subArray
