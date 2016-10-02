module Gonimo.Socket where

import Prelude
import Gonimo.Socket.Types
import Gonimo.Socket.Connection as ConnectionC
import Control.Monad.Reader (runReader)
import Control.Monad.Reader.Class (class MonadReader)
import Data.Foldable (foldl)
import Data.Maybe (maybe, fromMaybe, Maybe)
import Data.Monoid (mempty)
import Data.Tuple (uncurry, snd, Tuple(Tuple))
import Gonimo.Client.Types (Settings)
import Gonimo.Pux (justEffect, noEffects, EffModel(EffModel))
import Gonimo.Server.DbEntities (Family(Family), Device(Device))
import Gonimo.Socket.Message (decodeFromString)
import Gonimo.Types (Secret(Secret), Key(Key))
import Gonimo.WebAPI.Subscriber (receiveSocketByFamilyIdByToDevice, receiveSocketByFamilyIdByFromDeviceByToDeviceByChannelId)
import Servant.Subscriber (Subscriptions)
import WebRTC.MediaStream (getUserMedia, MediaStream)
import WebRTC.RTC (RTCPeerConnection)
import Data.Map as Map
import Data.Map (Map)
import Data.Array as Arr

update :: forall eff ps. Props ps -> Action -> State -> EffModel eff State Action
update props action = case action of
  AcceptConnection theirId channelId -> handleAcceptConnection props theirId channelId
  ConnectionA fromId channel action' -> noEffects
  AddConnection conn                 -> noEffects
  Nop                                -> noEffects


handleAcceptConnection :: forall eff ps. Props ps -> Key Device -> Secret -> State -> EffModel eff State Action
handleAcceptConnection props theirId channel = justEffect $ do
  let config = { ourId : props.deviceId
               , theirId : theirId
               , channel : channel
               }
  AddConnection <$> ConnectionC.init


getSubscriptions :: forall ps. Props ps -> State -> Subscriptions Action
getSubscriptions props state = fromMaybe mempty $ getSubscriptions' props state <$> props.familyId

getSubscriptions' :: forall ps. Props ps -> State -> (Key Family) -> Subscriptions Action
getSubscriptions' props state familyId =
  let
    receiveChannel = receiveSocketByFamilyIdByToDevice

    getConnectionSubscriptions :: forall m. (MonadReader Settings m)
                                  => Tuple Secret Connection -> m (Subscriptions Action)
    getConnectionSubscriptions (Tuple channel conn)=
      map (ConnectionA conn.theirId channel)
      <$> ConnectionC.getSubscriptions (mkConnectionProps props conn.theirId channel familyId)

    getConnectionsSubscriptions :: forall m. (MonadReader Settings m)
                                   => Map Secret Connection -> Array (m (Subscriptions Action))
    getConnectionsSubscriptions = Arr.fromFoldable <<< map getConnectionSubscriptions <<< Map.toList

    subArray :: Array (Subscriptions Action)
    subArray = map (flip runReader props.settings)
                $ getConnectionsSubscriptions state.connections
                <> [ receiveChannel (maybe Nop (uncurry AcceptConnection)) familyId props.deviceId ]
  in
     foldl append mempty subArray

-- mkConnectionProps :: forall ps. Props ps -> Key Device -> Secret -> ConnectionC.Props ()
mkConnectionProps props theirId channel familyId = { ourId : props.deviceId
                                                   , theirId : theirId
                                                   , channel : channel
                                                   , familyId : familyId
                                                   }
