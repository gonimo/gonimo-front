module Gonimo.UI.Socket.Channel where

import Prelude
import Data.Array as Arr
import Data.Tuple as Tuple
import Gonimo.UI.Socket.Message as Message
import Pux.Html.Attributes as A
import Pux.Html.Elements as H
import Pux.Html.Events as E
import WebRTC.MediaStream as MediaStream
import WebRTC.RTC as RTC
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.IO (IO)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Reader.Class (ask, class MonadReader)
import Control.Monad.State.Class (get)
import Data.Identity (runIdentity)
import Data.Lens ((^?), _Just, lens, Lens, (.=))
import Data.Maybe (fromMaybe, isJust, maybe, Maybe(Nothing, Just))
import Gonimo.Client.Types (toIO, Settings)
import Gonimo.Pux (noEffects, runGonimo, Component, Update, onlyModify, ComponentType)
import Gonimo.Server.DbEntities (Family(Family), Device(Device))
import Gonimo.Server.Types.Lenses (_Baby)
import Gonimo.Types (Secret(Secret), Key(Key))
import Gonimo.UI.Socket.Channel.Types (Action, Action(..), State, Props)
import Gonimo.UI.Socket.Lenses (mediaStream)
import Gonimo.UI.Socket.Message (runMaybeIceCandidate, MaybeIceCandidate(JustIceCandidate, NoIceCandidate), encodeToString, decodeFromString, Message)
import Gonimo.Util (coerceEffects)
import Gonimo.WebAPI (putSocketByFamilyIdByFromDeviceByToDeviceByChannelId)
import Gonimo.WebAPI.Subscriber (receiveSocketByFamilyIdByFromDeviceByToDeviceByChannelId)
import Pux.Html (text, Html)
import Pux.Html.Attributes (offset)
import Servant.Subscriber (Subscriptions)
import Unsafe.Coerce (unsafeCoerce)
import WebRTC.MediaStream (createObjectURL, mediaStreamToBlob, stopStream, getUserMedia, MediaStreamConstraints(MediaStreamConstraints), MediaStream)
import WebRTC.RTC (MediaStreamEvent, setLocalDescription, createAnswer, iceEventCandidate, IceEvent, onicecandidate, onaddstream, addIceCandidate, setRemoteDescription, fromRTCSessionDescription, createOffer, addStream, RTCIceCandidateInit, RTCPeerConnection, Ice, newRTCPeerConnection)



init :: Maybe MediaStream -> IO State
init stream = do
  rtcConnection <- liftEff (newRTCPeerConnection { iceServers : [ {url : "stun:stun.l.google.com:19302"} ] })
  ourStream <- runMaybeT $ do
    origStream <- MaybeT <<< pure $ stream
    liftEff $ MediaStream.clone origStream
  pure $  { mediaStream : ourStream
          , remoteStream : Nothing
          , rtcConnection : rtcConnection
          , isBabyStation : isJust stream
          }


remoteStream :: forall a b r. Lens { "remoteStream" :: a | r } { "remoteStream" :: b | r } a b
remoteStream = lens _."remoteStream" (_ { "remoteStream" = _ })

update :: forall ps. Update (Props ps) State Action
update action = case action of
  InitConnection                     -> handleInit -- Things we can't do in init already.
  StartStreaming constraints         -> handleStartStreaming constraints
  SetMediaStream stream              -> handleSetMediaStream stream
  StopStreaming                      -> closeStream
  CloseConnection                    -> handleCloseConnection
  AcceptMessage message              -> handleAcceptMessage message
  OnIceCandidate iceEvent            -> handleOnIceCandidate iceEvent
  OnAddStream streamEvent            -> handleOnAddStream streamEvent
  SetRemoteStream stream             -> remoteStream .= Just stream *> pure []
  ReportError _                      -> noEffects
  Nop                                -> noEffects

handleInit :: forall ps. ComponentType (Props ps) State Action
handleInit = do
  props <- ask :: Component (Props ps) State (Props ps)
  state <- get :: Component (Props ps) State State
  sendMessage <- getSendMessage
  pure
    [ do
         liftEff $ onaddstream (coerceEffects <<< props.sendAction <<< OnAddStream ) state.rtcConnection
         liftEff $ onicecandidate (coerceEffects <<< props.sendAction <<< OnIceCandidate ) state.rtcConnection
         if state.isBabyStation
           then pure Nop
           else sendMessage Message.StartStreaming
    ]

handleStartStreaming :: forall ps.
                        MediaStreamConstraints
                        -> ComponentType (Props ps) State Action
handleStartStreaming constraints =
  pure [ liftAff $ SetMediaStream <$> coerceEffects (getUserMedia constraints) ]

handleSendStartStreaming :: forall ps. ComponentType (Props ps) State Action
handleSendStartStreaming = do
  sendMessage <- getSendMessage
  pure [ sendMessage Message.StartStreaming ]

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
            liftAff $ setLocalDescription offer state.rtcConnection
            sendMessage $ Message.SessionDescriptionOffer offer
            pure Nop
       ]

handleOnIceCandidate :: forall ps. IceEvent -> ComponentType (Props ps) State Action
handleOnIceCandidate event = do
  let candidate = iceEventCandidate event
  let mCandidate = maybe NoIceCandidate JustIceCandidate candidate
  sendMessage <- getSendMessage
  pure [ sendMessage $ Message.IceCandidate mCandidate ]

handleOnAddStream :: forall ps. MediaStreamEvent  -> ComponentType (Props ps) State Action
handleOnAddStream event = do
  pure [ do
            url <- liftEff <<< createObjectURL <<< mediaStreamToBlob $ event.stream
            pure $ SetRemoteStream $ { stream : event.stream, objectURL : url }
       ]

getSendMessage :: forall ps. Component (Props ps) State (Message -> IO Action)
getSendMessage = do
  let sendString = putSocketByFamilyIdByFromDeviceByToDeviceByChannelId
  props <- ask
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
    Message.SessionDescriptionOffer description ->
      pure [ do
              liftAff $ setRemoteDescription description state.rtcConnection
              answer <- liftAff $ createAnswer state.rtcConnection
              liftAff $ setLocalDescription answer state.rtcConnection
              sendMessage $ Message.SessionDescriptionAnswer answer
           ]
    Message.SessionDescriptionAnswer description ->
      pure [ do
              liftAff $ setRemoteDescription description state.rtcConnection
              pure Nop
           ]
    Message.IceCandidate candidate ->
      pure [ do
               let test = runMaybeIceCandidate candidate
               case test of
                 Nothing -> pure unit -- let's see whether this helps
                 Just c -> liftAff $ addIceCandidate (runMaybeIceCandidate candidate) state.rtcConnection
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


view :: forall ps. Props ps -> State -> Html Action
view props state =
  let
    mBabyDevice = Tuple.lookup props.theirId props.onlineDevices
    babyName = fromMaybe "unknown baby" $ mBabyDevice ^? _Just <<< _Baby
  in
    H.div [ A.className "panel panel-default" ]
            [ H.div [ A.className "panel-heading" ]
              [
                H.text babyName
              ]
            , viewVideo state
            ]

viewVideo :: State -> Html Action
viewVideo state =
   case state.remoteStream of
       Nothing -> H.text $ "Sorry - no video there yet, please check that your camera/microphone "
                  <> "are enabled on the baby station and your browser allows us to access it."
       Just stream ->
         H.video [ A.src stream.objectURL
                 , A.autoPlay "true"
                 , A.controls true
                 , A.width "100%"
                 ] []



getSubscriptions :: forall m ps. (MonadReader Settings m) => Props ps -> m (Subscriptions Action)
getSubscriptions props =
  let
    receiveMessage = receiveSocketByFamilyIdByFromDeviceByToDeviceByChannelId
                      doDecode
    doDecode :: Maybe (Maybe String) -> Action
    doDecode = maybe Nop AcceptMessage <<< (\mv -> decodeFromString =<< join mv)
  in
    receiveMessage props.familyId props.theirId props.ourId props.cSecret
