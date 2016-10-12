module Gonimo.UI.Socket.Channel where

import Prelude
import Gonimo.UI.Socket.Message as Message
import WebRTC.RTC as RTC
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.IO (IO)
import Control.Monad.Reader.Class (class MonadReader)
import Data.Maybe (isJust, maybe, Maybe(Nothing, Just))
import Gonimo.Client.Types (Settings, GonimoEff)
import Gonimo.Pux (noEffects, Update, onlyModify, ComponentType)
import Gonimo.Server.DbEntities (Family(Family), Device(Device))
import Gonimo.Types (Secret(Secret), Key(Key))
import Gonimo.UI.Socket.Channel.Types (Action(Nop, AcceptMessage, CloseConnection, StopStreaming, SetMediaStream, StartStreaming), State, Props)
import Gonimo.UI.Socket.Message (decodeFromString, Message)
import Gonimo.UI.Socket.Types (BabyStation)
import Gonimo.Util (coerceEffects)
import Gonimo.WebAPI.Subscriber (receiveSocketByFamilyIdByFromDeviceByToDeviceByChannelId)
import Servant.Subscriber (Subscriptions)
import WebRTC.MediaStream (getUserMedia, MediaStreamConstraints(MediaStreamConstraints), MediaStream)
import WebRTC.RTC (RTCIceCandidateInit, RTCPeerConnection, Ice, newRTCPeerConnection)



init :: Maybe BabyStation -> IO State
init babyStation = do
  rtcConnection <- liftEff (newRTCPeerConnection { iceServers : [ {url : "stun:stun.l.google.com:19302"} ] })
  pure $  { mediaStream : _.mediaStream <$> babyStation
          , rtcConnection : rtcConnection
          , isBabyStation : isJust babyStation
          }



update :: forall ps. Update (Props ps) State Action
update action = case action of
  StartStreaming constraints         -> handleStartStreaming constraints
  SetMediaStream stream              -> onlyModify $ _ { mediaStream = Just stream }
  StopStreaming                      -> noEffects
  CloseConnection                    -> noEffects
  AcceptMessage message              -> noEffects
  Nop                                -> noEffects

handleStartStreaming :: forall ps.
                        MediaStreamConstraints
                        -> ComponentType (Props ps) State Action
handleStartStreaming constraints =
  pure [ liftAff $ SetMediaStream <$> coerceEffects (getUserMedia constraints) ]


getSubscriptions :: forall m ps. (MonadReader Settings m) => Props ps -> m (Subscriptions Action)
getSubscriptions props =
  let
    receiveMessage = receiveSocketByFamilyIdByFromDeviceByToDeviceByChannelId
                      ((maybe Nop AcceptMessage) <<< (decodeFromString =<< _))
  in
    receiveMessage props.familyId props.theirId props.ourId props.cSecret
