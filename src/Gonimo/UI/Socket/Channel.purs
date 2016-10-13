module Gonimo.UI.Socket.Channel where

import Prelude
import Gonimo.UI.Socket.Message as Message
import WebRTC.RTC as RTC
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.IO (IO)
import Control.Monad.Reader.Class (ask, class MonadReader)
import Data.Maybe (isJust, maybe, Maybe(Nothing, Just))
import Gonimo.Client.Types (Settings)
import Gonimo.Pux (noEffects, runGonimo, Component, Update, onlyModify, ComponentType)
import Gonimo.Server.DbEntities (Family(Family), Device(Device))
import Gonimo.Types (Secret(Secret), Key(Key))
import Gonimo.UI.Socket.Channel.Types (Action(..), State, Props)
import Gonimo.UI.Socket.Message (encodeToString, decodeFromString, Message)
import Gonimo.UI.Socket.Types (BabyStation)
import Gonimo.Util (coerceEffects)
import Gonimo.WebAPI (putSocketByFamilyIdByFromDeviceByToDeviceByChannelId)
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
  ReportError _                      -> noEffects
  Nop                                -> noEffects

handleStartStreaming :: forall ps.
                        MediaStreamConstraints
                        -> ComponentType (Props ps) State Action
handleStartStreaming constraints =
  pure [ liftAff $ SetMediaStream <$> coerceEffects (getUserMedia constraints) ]

sendMessage :: forall ps. Message -> ComponentType (Props ps) State Action
sendMessage msg = do
  let sendString = putSocketByFamilyIdByFromDeviceByToDeviceByChannelId
  let strMsg = encodeToString msg
  props <- ask
  let familyId' = props.familyId
  let fromDevice' = props.ourId
  let toDevice' = props.theirId
  let channelId' = props.cSecret
  runGonimo $ do
    sendString strMsg familyId' fromDevice' toDevice' channelId'
    pure Nop

getSubscriptions :: forall m ps. (MonadReader Settings m) => Props ps -> m (Subscriptions Action)
getSubscriptions props =
  let
    receiveMessage = receiveSocketByFamilyIdByFromDeviceByToDeviceByChannelId
                      ((maybe Nop AcceptMessage) <<< (decodeFromString =<< _))
  in
    receiveMessage props.familyId props.theirId props.ourId props.cSecret
