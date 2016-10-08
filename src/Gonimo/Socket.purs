module Gonimo.Socket where

import Prelude
import Data.Array as Arr
import Data.Map as Map
import Gonimo.Socket.Channel as ChannelC
import Control.Monad.IO (IO)
import Control.Monad.Reader (ask, runReader)
import Control.Monad.Reader.Class (class MonadReader, ask)
import Control.Monad.State.Class (put, get, class MonadState)
import Data.Foldable (foldl)
import Data.Lens (_Just, (.=))
import Data.Map (Map)
import Data.Maybe (maybe, fromMaybe, Maybe(Just))
import Data.Monoid (mempty)
import Data.Tuple (uncurry, snd, Tuple(Tuple))
import Gonimo.Client.Types (Settings)
import Gonimo.Pux (Component, liftChild, toParent, ComponentType, makeChildData, ToChild, onlyModify, Update, noEffects, class MonadComponent)
import Gonimo.Server.DbEntities (Family(Family), Device(Device))
import Gonimo.Socket.Message (decodeFromString)
import Gonimo.Socket.Types (toCSecret, toTheirId, makeChannelId, ChannelId(ChannelId), channel, Props, State, Action(..))
import Gonimo.Types (Secret(Secret), Key(Key))
import Gonimo.WebAPI.Subscriber (receiveSocketByFamilyIdByToDevice, receiveSocketByFamilyIdByFromDeviceByToDeviceByChannelId)
import Servant.Subscriber (Subscriptions)
import WebRTC.MediaStream (getUserMedia, MediaStream)
import WebRTC.RTC (RTCPeerConnection)


init :: forall ps. Props ps -> State
init props = { ourId : props.deviceId
             , ourFamilyId : props.ourFamilyId
             , channels : Map.empty
             }


update :: forall ps. Update (Props ps) State Action
update action = do
    props <- ask
    state <- (get :: Component (Props ps) State State)
    if state.ourId == props.deviceId && state.ourFamilyId == props.ourFamilyId
      then update' action
      else -- Ok family switch/ new device id - let's start over ...
      if Map.isEmpty state.channels
      then do
        put $ init props
        update' action
      else -- Cleanup first ...
        pure
          $ Arr.fromFoldable (pure <<< CloseChannel <$> Map.keys state.channels) -- Close old channels first.
          <> [ pure action ] -- Then try again.

update' :: forall ps. Update (Props ps) State Action
update' action = case action of
  AcceptConnection channelId'           -> handleAcceptConnection channelId'
  AddChannel channelId' cState          -> channel channelId' .= Just cState
                                           *> pure []
  ChannelA channelId' action'           -> updateChannel channelId' action'
  CloseChannel channelId                -> noEffects -- do CloseConnection, then remove from map
  Nop                                   -> noEffects


toChannel :: forall ps. ChannelId -> ToChild (Props ps) State (ChannelC.Props {}) ChannelC.State
toChannel channelId' = do
  state <- get
  let
    props :: ChannelC.Props {}
    props = mkChannelProps state channelId'
  pure $ makeChildData (channel channelId' <<< _Just) props

handleAcceptConnection :: forall m. ( Applicative m )
                          => ChannelId
                          -> m (Array (IO Action))
handleAcceptConnection channelId' = do
  pure [ AddChannel channelId' <$> ChannelC.init ]

updateChannel :: forall ps. ChannelId -> ChannelC.Action -> ComponentType (Props ps) State Action
updateChannel channelId' = toParent [] (ChannelA channelId')
                           <<< liftChild (toChannel channelId')
                           <<< ChannelC.update

getSubscriptions :: forall ps. Props ps -> State -> Subscriptions Action
getSubscriptions props state =
  let
    receiveAChannel = receiveSocketByFamilyIdByToDevice

    getChannelSubscriptions :: forall m. (MonadReader Settings m)
                                  => ChannelId -> m (Subscriptions Action)
    getChannelSubscriptions channelId' =
      let
        cProps = mkChannelProps state channelId'
      in
      map (ChannelA channelId') <$> ChannelC.getSubscriptions cProps

    getChannelsSubscriptions :: forall m. (MonadReader Settings m)
                                => Map ChannelId ChannelC.State
                                -> Array (m (Subscriptions Action))
    getChannelsSubscriptions = Arr.fromFoldable <<< map getChannelSubscriptions <<< Map.keys

    subArray :: Array (Subscriptions Action)
    subArray = map (flip runReader props.settings)
                $ getChannelsSubscriptions state.channels
                <> [ receiveAChannel
                      (maybe Nop (AcceptConnection <<< uncurry makeChannelId ))
                      props.ourFamilyId props.deviceId -- Use props here - not state!
                   ]
  in
     foldl append mempty subArray

mkChannelProps :: State -> ChannelId -> ChannelC.Props {}
mkChannelProps state channelId = { ourId : state.ourId
                                 , theirId : toTheirId channelId
                                 , cSecret : toCSecret channelId
                                 , ourFamilyId : state.ourFamilyId
                                 }
