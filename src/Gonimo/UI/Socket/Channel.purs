module Gonimo.UI.Socket.Channel where

import Prelude
import Data.Array as Arr
import Gonimo.UI.Socket.Message as Message
import WebRTC.MediaStream as MediaStream
import WebRTC.RTC as RTC
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.IO (IO)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Reader.Class (ask, class MonadReader)
import Control.Monad.State.Class (get)
import Data.Lens ((.=))
import Data.Maybe (fromMaybe, isJust, maybe, Maybe(Nothing, Just))
import Gonimo.Client.Types (toIO, Settings)
import Gonimo.Pux (noEffects, runGonimo, Component, Update, onlyModify, ComponentType)
import Gonimo.Server.DbEntities (Family(Family), Device(Device))
import Gonimo.Types (Secret(Secret), Key(Key))
import Gonimo.UI.Socket.Channel.Types (Action, Action(OnAddStream, OnIceCandidate), Action(..), State, Props)
import Gonimo.UI.Socket.Lenses (mediaStream)
import Gonimo.UI.Socket.Message (runMaybeIceCandidate, encodeToString, decodeFromString, Message)
import Gonimo.UI.Socket.Types (BabyStation)
import Gonimo.Util (coerceEffects)
import Gonimo.WebAPI (putSocketByFamilyIdByFromDeviceByToDeviceByChannelId)
import Gonimo.WebAPI.Subscriber (receiveSocketByFamilyIdByFromDeviceByToDeviceByChannelId)
import Servant.Subscriber (Subscriptions)
import WebRTC.MediaStream (stopStream, getUserMedia, MediaStreamConstraints(MediaStreamConstraints), MediaStream)
import WebRTC.RTC (iceEventCandidate, IceEvent, onicecandidate, onaddstream, addIceCandidate, setRemoteDescription, fromRTCSessionDescription, createOffer, addStream, RTCIceCandidateInit, RTCPeerConnection, Ice, newRTCPeerConnection)



init :: (Action -> Eff () Unit) -> Maybe BabyStation -> IO State
init sendAction' babyStation = do
  rtcConnection <- liftEff (newRTCPeerConnection { iceServers : [ {url : "stun:stun.l.google.com:19302"} ] })
  liftEff $ onaddstream (coerceEffects <<< sendAction' <<< OnAddStream ) rtcConnection
  liftEff $ onicecandidate (coerceEffects <<< sendAction' <<< OnIceCandidate ) rtcConnection
  let sockStream = _.mediaStream <$> babyStation
  ourStream <- runMaybeT $ do
    origStream <- MaybeT <<< pure $ sockStream
    liftEff $ MediaStream.clone origStream
  pure $  { mediaStream : ourStream
          , rtcConnection : rtcConnection
          , isBabyStation : isJust babyStation
          }



update :: forall ps. Update (Props ps) State Action
update action = case action of
  StartStreaming constraints         -> handleStartStreaming constraints
  SetMediaStream stream              -> handleSetMediaStream stream
  StopStreaming                      -> noEffects
  CloseConnection                    -> handleCloseConnection
  AcceptMessage message              -> handleAcceptMessage message
  OnIceCandidate iceEvent            -> handleOnIceCandidate iceEvent
  OnAddStream streamEvent            -> noEffects -- handleOnAddStream iceEvent
  ReportError _                      -> noEffects
  Nop                                -> noEffects

handleStartStreaming :: forall ps.
                        MediaStreamConstraints
                        -> ComponentType (Props ps) State Action
handleStartStreaming constraints =
  pure [ liftAff $ SetMediaStream <$> coerceEffects (getUserMedia constraints) ]

handleSetMediaStream :: forall ps. MediaStream -> ComponentType (Props ps) State Action
handleSetMediaStream stream = do
  mediaStream .= Just stream
  startStreaming stream

startStreaming :: forall ps. MediaStream -> ComponentType (Props ps) State Action
startStreaming stream = do
  state <- get
  sendMessage <- getSendMessage
  pure [ do
            liftEff $ addStream stream state.rtcConnection
            offer <- liftAff $ createOffer state.rtcConnection
            sendMessage $ Message.SessionDescription offer
            pure Nop
       ]

handleOnIceCandidate :: forall ps. IceEvent -> ComponentType (Props ps) State Action
handleOnIceCandidate event = do
  let candidate = iceEventCandidate event
  state <- get
  pure [ liftEff $ do
           addIceCandidate candidate state.rtcConnection
           pure Nop
       ]

getSendMessage :: forall ps. Component (Props ps) State (Message -> IO Action)
getSendMessage = do
  let sendString = putSocketByFamilyIdByFromDeviceByToDeviceByChannelId
  props <- ask :: Component (Props ps) State (Props ps)
  let familyId' = props.familyId
  let fromDevice' = props.ourId
  let toDevice' = props.theirId
  let channelId' = props.cSecret
  pure $ toIO props.settings <<< ( \msg ->  do
    let strMsg = encodeToString msg
    sendString strMsg familyId' fromDevice' toDevice' channelId'
    pure Nop)

handleAcceptMessage :: forall ps. Message -> ComponentType (Props ps) State Action
handleAcceptMessage msg = do
  state <- get
  sendMessage <- getSendMessage
  case msg of
    Message.StartStreaming -> fromMaybe (pure []) $ startStreaming <$> state.mediaStream
    Message.SessionDescription description ->
      pure [ do
              liftAff $ setRemoteDescription description state.rtcConnection
              pure Nop
           ]
    Message.IceCandidate candidate ->
      pure [ do
               liftEff $ addIceCandidate (runMaybeIceCandidate candidate) state.rtcConnection
               pure Nop
           ]
    Message.CloseConnection -> closeStream

closeStream :: forall ps. ComponentType (Props ps) State Action
closeStream = do
  state <- get
  pure $ fromMaybe [] $ do
    stream <- state.mediaStream
    pure [ liftEff $ do
              stopStream stream
              pure Nop
         ]


handleCloseConnection :: forall ps. ComponentType (Props ps) State Action
handleCloseConnection = do
  sendMessage <- getSendMessage
  closeActions <- closeStream
  pure $ closeActions <> [ sendMessage Message.CloseConnection ]

getSubscriptions :: forall m ps. (MonadReader Settings m) => Props ps -> m (Subscriptions Action)
getSubscriptions props =
  let
    receiveMessage = receiveSocketByFamilyIdByFromDeviceByToDeviceByChannelId
                      ((maybe Nop AcceptMessage) <<< (decodeFromString =<< _))
  in
    receiveMessage props.familyId props.theirId props.ourId props.cSecret
