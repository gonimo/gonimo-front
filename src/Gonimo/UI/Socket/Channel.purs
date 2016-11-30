module Gonimo.UI.Socket.Channel where

import Prelude
import Data.Array as Arr
import Data.CatQueue as Q
import Data.List as List
import Data.Tuple as Tuple
import Gonimo.Client.Effects as Gonimo
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
import Data.Lens ((+=), (-=), (^?), _Just, lens, Lens, (.=), use, (%=), to)
import Data.List (List(Cons, Nil))
import Data.Maybe (fromMaybe, isJust, maybe, Maybe(Nothing, Just))
import Data.Traversable (traverse)
import Data.Tuple (uncurry, curry, Tuple(Tuple))
import Gonimo.Client.Types (toIO, Settings)
import Gonimo.Pux (noEffects, runGonimo, Component, Update, onlyModify, ComponentType)
import Gonimo.Server.Db.Entities (Family(Family), Device(Device))
import Gonimo.Server.State.Types (MessageNumber(MessageNumber))
import Gonimo.Server.Types.Lenses (_Baby)
import Gonimo.Types (Secret(Secret), Key(Key))
import Gonimo.UI.Socket.Channel.Types (maxMessagesInFlight, messagesInFlight, messageQueue, Action(EnqueueMessage), Action(..), State, Props)
import Gonimo.UI.Socket.Lenses (mediaStream)
import Gonimo.UI.Socket.Message (runMaybeIceCandidate, MaybeIceCandidate(JustIceCandidate, NoIceCandidate), encodeToString, decodeFromString, Message)
import Gonimo.Util (coerceEffects)
import Gonimo.WebAPI (deleteSocketByFamilyIdByFromDeviceByToDeviceByChannelIdMessagesByMessageNumber, putSocketByFamilyIdByFromDeviceByToDeviceByChannelId)
import Gonimo.WebAPI.Subscriber (getSocketByFamilyIdByFromDeviceByToDeviceByChannelId)
import Pux.Html (text, Html)
import Pux.Html.Attributes (offset)
import Servant.Subscriber (Subscriptions)
import Unsafe.Coerce (unsafeCoerce)
import WebRTC.MediaStream (createObjectURL, mediaStreamToBlob, stopStream, getUserMedia, MediaStreamConstraints(MediaStreamConstraints), MediaStream)
import WebRTC.RTC (iceConnectionState, oniceconnectionstatechange, connectionState, onconnectionstatechange, onnegotiationneeded, closeRTCPeerConnection, MediaStreamEvent, setLocalDescription, createAnswer, iceEventCandidate, IceEvent, onicecandidate, onaddstream, addIceCandidate, setRemoteDescription, fromRTCSessionDescription, createOffer, addStream, RTCIceCandidateInit, RTCPeerConnection, Ice, newRTCPeerConnection)
import WebRTC.Util (onConnectionDrop)


init :: Maybe MediaStream -> IO State
init stream = do
  rtcConnection <- liftEff (newRTCPeerConnection { iceServers : [ { urls : [ "turn:gonimo.com:3478" ]
                                                                  , username : "gonimo"
                                                                  , credential : "Aeloh5chai2eil1"
                                                                  , credentialType : "password"
                                                                  }
                                                                ]
                                                 }
                           )
  ourStream <- runMaybeT $ do
    origStream <- MaybeT <<< pure $ stream
    liftEff $ MediaStream.clone origStream
  connState <- liftEff $ iceConnectionState rtcConnection
  pure $  { mediaStream : ourStream
          , remoteStream : Nothing
          , rtcConnection : rtcConnection
          , isBabyStation : isJust stream
          , messageQueue  : Nil
          , messagesInFlight : 0
          , prevConnectionState : connState
          }


remoteStream :: forall a b r. Lens { "remoteStream" :: a | r } { "remoteStream" :: b | r } a b
remoteStream = lens _."remoteStream" (_ { "remoteStream" = _ })

update :: forall ps. Update (Props ps) State Action
update action = case action of
  InitConnection                     -> handleInit -- Things we can't do in init already.
  StartStreaming constraints         -> handleStartStreaming constraints
  StartNegotiation                   -> handleStartNegotiation
  SetMediaStream stream              -> handleSetMediaStream stream
  StopStreaming                      -> closeConnection
  CloseConnection                    -> handleCloseConnection
  AcceptMessages num messages        -> handleAcceptMessages num messages
  OnIceCandidate iceEvent            -> handleOnIceCandidate iceEvent
  OnAddStream streamEvent            -> handleOnAddStream streamEvent
  OnConnectionDrop                -> handleOnConnectionDrop
  SetRemoteStream stream             -> remoteStream .= Just stream *> pure []
  ReportError _                      -> noEffects
  EnqueueMessage message             -> handleEnqueueMessage message
  MessageSent nAction                -> handleMessageSent nAction
  Nop                                -> noEffects

handleInit :: forall ps. ComponentType (Props ps) State Action
handleInit = do
  props <- ask :: Component (Props ps) State (Props ps)
  state <- get :: Component (Props ps) State State
  pure
    [ do
         liftEff $ onaddstream (coerceEffects <<< props.sendAction <<< OnAddStream ) state.rtcConnection
         liftEff $ onicecandidate (coerceEffects <<< props.sendAction <<< OnIceCandidate ) state.rtcConnection
         liftEff $ onnegotiationneeded (coerceEffects <<< props.sendAction $ StartNegotiation) state.rtcConnection
         if state.isBabyStation
           then pure Nop
           else do
             liftEff $ onConnectionDrop (coerceEffects <<< props.sendAction $ OnConnectionDrop) Nothing state.rtcConnection
             pure $ EnqueueMessage Message.StartStreaming
    ]

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
  pure [ do
            liftEff $ addStream stream state.rtcConnection
            pure Nop
       ]

handleStartNegotiation :: forall ps. ComponentType (Props ps) State Action
handleStartNegotiation = do
  state <- get
  pure [ do
            offer <- liftAff $ createOffer state.rtcConnection
            liftAff $ setLocalDescription offer state.rtcConnection
            pure <<< EnqueueMessage $ Message.SessionDescriptionOffer offer
       ]

handleOnIceCandidate :: forall ps. IceEvent -> ComponentType (Props ps) State Action
handleOnIceCandidate event = do
  let candidate = iceEventCandidate event
  let mCandidate = maybe NoIceCandidate JustIceCandidate candidate
  pure [ pure <<< EnqueueMessage $ Message.IceCandidate mCandidate ]

handleOnAddStream :: forall ps. MediaStreamEvent  -> ComponentType (Props ps) State Action
handleOnAddStream event = do
  pure [ do
            url <- liftEff <<< createObjectURL <<< mediaStreamToBlob $ event.stream
            pure $ SetRemoteStream $ { stream : event.stream, objectURL : url }
       ]

handleOnConnectionDrop :: forall ps. ComponentType (Props ps) State Action
handleOnConnectionDrop = do
  state <- get
  pure [ do
            Gonimo.log $ "Old connection state: " <> state.prevConnectionState
            connState <- liftEff $ iceConnectionState state.rtcConnection
            Gonimo.log $ "New connection state: " <> connState
            pure Nop
            -- if state.prevConnectionState == "connected" && 
       ]
getSendMessages :: forall ps. Component (Props ps) State (Array String -> IO Action)
getSendMessages = do
  let sendMessages = putSocketByFamilyIdByFromDeviceByToDeviceByChannelId
  props <- ask
  let familyId' = props.familyId
  let fromDevice' = props.ourId
  let toDevice' = props.theirId
  let channelId' = props.cSecret
  let sendMessage = map MessageSent
                    <<< toIO props.settings
                    <<< ( \msgs ->  do
                             sendMessages msgs familyId' fromDevice' toDevice' channelId'
                             pure Nop
                        )
  pure sendMessage


handleAcceptMessages :: forall ps. MessageNumber -> Array String -> ComponentType (Props ps) State Action
handleAcceptMessages msgNumber msgs = do
  let decoded = Arr.mapMaybe decodeFromString msgs
  props <- ask
  let deleteMessage = deleteSocketByFamilyIdByFromDeviceByToDeviceByChannelIdMessagesByMessageNumber
  deleteActions <- runGonimo $ do
    deleteMessage props.familyId props.theirId props.ourId props.cSecret msgNumber
    pure Nop
  messageActions <- Arr.concat <$> traverse handleMessage decoded
  pure $ deleteActions <> messageActions


handleMessage :: forall ps. Message -> ComponentType (Props ps) State Action
handleMessage msg = do
  state <- get
  case msg of
    Message.StartStreaming -> fromMaybe (pure []) $ startStreaming <$> state.mediaStream
    Message.SessionDescriptionOffer description ->
      pure [ do
              liftAff $ setRemoteDescription description state.rtcConnection
              answer <- liftAff $ createAnswer state.rtcConnection
              liftAff $ setLocalDescription answer state.rtcConnection
              pure <<< EnqueueMessage $ Message.SessionDescriptionAnswer answer
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
    Message.CloseConnection -> closeConnection

closeConnection :: forall ps. ComponentType (Props ps) State Action
closeConnection = do
  state <- get
  let conn = state.rtcConnection
  pure $ fromMaybe [] $ do
    stream <- state.mediaStream
    pure [ liftEff $ do
              stopStream stream
              closeRTCPeerConnection conn
              pure Nop
         ]


handleCloseConnection :: forall ps. ComponentType (Props ps) State Action
handleCloseConnection = do
  closeActions <- closeConnection
  pure $ closeActions <> [ pure $ EnqueueMessage Message.CloseConnection ]

handleEnqueueMessage :: forall ps. Message -> ComponentType (Props ps) State Action
handleEnqueueMessage message = do
  let strMsg = encodeToString message
  allClear <- use $ messagesInFlight <<< to (_ < maxMessagesInFlight)
  sendMessages <- getSendMessages
  if allClear
    then do
      messagesInFlight += 1
      pure [ sendMessages $ Arr.singleton strMsg ] -- Ready to take off!
    else do
      messageQueue %= Cons strMsg
      pure []

handleMessageSent :: forall ps. Action -> ComponentType (Props ps) State Action
handleMessageSent nAction = do
  messagesInFlight -= 1
  newActions <- update nAction
  queue :: List String <- use messageQueue
  sendMessages <- getSendMessages
  let messages = Arr.fromFoldable $ List.reverse queue
  ourActions <-
        if Arr.null messages
        then pure []
        else do
          messageQueue .= Nil :: (List String)
          messagesInFlight += 1
          pure [ sendMessages messages ]
  pure $ newActions <> ourActions

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
    receiveMessage = getSocketByFamilyIdByFromDeviceByToDeviceByChannelId toAction
    toAction :: Maybe (Maybe (Tuple MessageNumber (Array String))) -> Action
    toAction (Just (Just (Tuple num messages))) = AcceptMessages num messages
    toAction (Just Nothing) = Nop
    toAction Nothing = Nop
  in
    receiveMessage props.familyId props.theirId props.ourId props.cSecret

