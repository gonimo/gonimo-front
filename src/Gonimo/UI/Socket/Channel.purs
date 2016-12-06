module Gonimo.UI.Socket.Channel where

import Prelude
import Control.Monad.Eff.Console as Console
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
import WebRTC.MediaStream.Track as Track
import WebRTC.RTC as RTC
import WebRTC.RTCRtpSender as Sender
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (catchException)
import Control.Monad.IO (IO)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Reader.Class (ask, class MonadReader)
import Control.Monad.State.Class (modify, get)
import Data.Array (zip)
import Data.Foldable (traverse_)
import Data.Identity (runIdentity)
import Data.Lens ((+=), (-=), (^?), _Just, lens, Lens, (.=), use, (%=), to, LensP)
import Data.List (List(Cons, Nil))
import Data.Maybe (fromMaybe, isJust, maybe, Maybe(Nothing, Just))
import Data.Traversable (traverse)
import Data.Tuple (snd, fst, uncurry, curry, Tuple(Tuple))
import Gonimo.Client.Types (toIO, Settings)
import Gonimo.Client.Vibrations (stopVibration, startVibration, Vibrator)
import Gonimo.Pux (noEffects, runGonimo, Component, Update, onlyModify, ComponentType)
import Gonimo.Server.Db.Entities (Family(Family), Device(Device))
import Gonimo.Server.State.Types (MessageNumber(MessageNumber))
import Gonimo.Server.Types.Lenses (_Baby)
import Gonimo.Types (Secret(Secret), Key(Key))
import Gonimo.UI.Socket.Channel.Types (audioStats, maxMessagesInFlight, messagesInFlight, messageQueue, Action(EnqueueMessage), Action(..), State, Props, StreamConnectionState(..), StreamConnectionStats, videoStats, connectionState, packetsReceived, TrackKind(..), vibrator)
import Gonimo.UI.Socket.Lenses (mediaStream)
import Gonimo.UI.Socket.Message (runMaybeIceCandidate, MaybeIceCandidate(JustIceCandidate, NoIceCandidate), encodeToString, decodeFromString, Message)
import Gonimo.Util (stopSound, playSound, loadSound, boostVolumeMediaStream, coerceEffects)
import Gonimo.WebAPI (deleteSocketByFamilyIdByFromDeviceByToDeviceByChannelIdMessagesByMessageNumber, putSocketByFamilyIdByFromDeviceByToDeviceByChannelId)
import Gonimo.WebAPI.Subscriber (getSocketByFamilyIdByFromDeviceByToDeviceByChannelId)
import Pux.Html (text, Html)
import Pux.Html.Attributes (offset)
import Servant.PureScript.Affjax (unsafeToString)
import Servant.Subscriber (Subscriptions)
import Unsafe.Coerce (unsafeCoerce)
import WebRTC.MediaStream (createObjectURL, mediaStreamToBlob, stopStream, getUserMedia, MediaStreamConstraints(MediaStreamConstraints), MediaStream)
import WebRTC.MediaStream.Track (addEventListener, MediaStreamTrack)
import WebRTC.RTC (closeRTCPeerConnection, RTCPeerConnection, iceConnectionState, oniceconnectionstatechange, onconnectionstatechange, onnegotiationneeded, MediaStreamEvent, setLocalDescription, createAnswer, iceEventCandidate, IceEvent, onicecandidate, onaddstream, addIceCandidate, setRemoteDescription, fromRTCSessionDescription, createOffer, addStream, RTCIceCandidateInit, Ice, newRTCPeerConnection)
import WebRTC.Util (onConnectionDrop)


init :: Maybe MediaStream -> IO State
init stream = do
  rtcConnection <- liftEff (newRTCPeerConnection { iceServers : [ { urls : [ "turn:gonimo.com:3478" ]
                                                                  , username : "gonimo"
                                                                  , credential : "Aeloh5chai2eil1" -- FIXME: For production this obviously should not be hardcoded.
                                                                  , credentialType : "password"
                                                                  }
                                                                ]
                                                 }
                           )
  ourStream <- runMaybeT $ do
    origStream <- MaybeT <<< pure $ stream
    -- Close rtc connection once a track ends - so parent will notice local problems:
    tracks <- liftEff $ MediaStream.getTracks origStream
    let addListener event = liftEff <<< addEventListener event (const (closeRTCPeerConnection rtcConnection))
    traverse_ (addListener "ended") tracks
    traverse_ (addListener "mute") tracks
    liftEff $ boostVolumeMediaStream origStream

  connState <- liftEff $ iceConnectionState rtcConnection
  let isBabyStation = isJust stream
  alarm' <- if isBabyStation -- We only need alarm at the parent side
            then pure Nothing
            else map Just <<< liftAff $ loadSound "/sounds/pup_alert.mp3"
  pure $  { mediaStream : ourStream
          , remoteStream : Nothing
          , rtcConnection : rtcConnection
          , isBabyStation : isBabyStation
          , messageQueue  : Nil
          , messagesInFlight : 0
          , audioStats : initStreamConnectionStats
          , videoStats : initStreamConnectionStats
          , vibrator   : Nothing
          , alarm : alarm'
          , alarmIsOn : false
          }

initStreamConnectionStats :: StreamConnectionStats
initStreamConnectionStats = { packetsReceived : 0
                            , connectionState : ConnectionNotAvailable
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
  OnAudioConnectionDrop mPackets     -> handleOnConnectionDrop audioStats mPackets
  OnVideoConnectionDrop mPackets     -> handleOnConnectionDrop videoStats mPackets
  RegisterMediaTracks tracks         -> traverse_ handleRegisterMediaTrack tracks *> pure []
  SetRemoteStream stream             -> remoteStream .= Just stream *> pure []
  ReportError _                      -> noEffects
  EnqueueMessage message             -> handleEnqueueMessage message
  MessageSent nAction                -> handleMessageSent nAction
  SetVibrator mVibrator              -> handleSetVibrator mVibrator
  TurnOnAlarm onOff                  -> handleTurnOnAlarm onOff
  ConnectionClosed                      -> noEffects
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
           else pure $ EnqueueMessage Message.StartStreaming
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
    state <- get
    props <- ask
    let registerConnDrop = if state.isBabyStation
                           then []
                           else [ registerOnConnectionDrop props state ]
    pure $ registerConnDrop
         <> [ do
              url <- liftEff <<< createObjectURL <<< mediaStreamToBlob $ event.stream
              pure $ SetRemoteStream $ { stream : event.stream, objectURL : url }
            ]
  where
    registerOnConnectionDrop :: Props ps -> State -> IO Action
    registerOnConnectionDrop props state = do
      mVideoTrack <- map Arr.head <<< liftEff $ MediaStream.getVideoTracks event.stream
      mAudioTrack <- map Arr.head <<< liftEff $ MediaStream.getAudioTracks event.stream
      let kindArray kind = Arr.fromFoldable <<< map (const kind)
      maybeRegister props OnAudioConnectionDrop mAudioTrack state.rtcConnection
      maybeRegister props OnVideoConnectionDrop mVideoTrack state.rtcConnection
      -- pure $ RegisterMediaTracks (kindArray audioStats mAudioTrack <> kindArray videoStats mVideoTrack)
      pure $ RegisterMediaTracks (kindArray AudioTrack mAudioTrack <> kindArray VideoTrack mVideoTrack)

    maybeRegister :: Props ps -> (Maybe Int -> Action) -> Maybe MediaStreamTrack -> RTCPeerConnection -> IO Unit
    maybeRegister _ _ Nothing _ = pure unit
    maybeRegister props mkAction mTrack@(Just track) pc = do
      let eventHandler = coerceEffects <<< props.sendAction <<< mkAction
      liftEff $ onConnectionDrop eventHandler mTrack pc
      liftEff $ addEventListener "ended" (const (eventHandler Nothing)) track
      liftEff $ addEventListener "ended" (const (Console.log "Track ended")) track

handleOnConnectionDrop :: forall ps. LensP State StreamConnectionStats
                          -> Maybe Int -> ComponentType (Props ps) State Action
handleOnConnectionDrop stats mPackets= do
    case mPackets of
      Nothing -> do
        stats <<< connectionState %= updateState ConnectionDied
        status <- use (stats <<< connectionState)
        case status of
          ConnectionDied -> do
            let vibratorActions = [ do
                                       vibrator' <- liftEff $ startVibration [200, 100, 250, 100, 300]
                                       pure $ (SetVibrator <<< Just) vibrator'
                                  ]
            alarmActions <- handleTurnOnAlarm true
            pure $ alarmActions <> vibratorActions
          _ -> pure []
      Just packets -> do
        stats <<< packetsReceived .= packets
        stats <<< connectionState %= updateState ConnectionReliable
        pure []
  where
    -- updateState new old
    updateState :: StreamConnectionState -> StreamConnectionState -> StreamConnectionState
    updateState ConnectionNotAvailable ConnectionDied = ConnectionNotAvailable -- User can stop a dead connection.
    updateState _ ConnectionDied = ConnectionDied -- In all other cases it remains dead.
    updateState ConnectionUnknown ConnectionNotAvailable = ConnectionUnknown
    updateState _ ConnectionNotAvailable = ConnectionNotAvailable
    updateState new _ = new

handleSetVibrator :: forall ps. Maybe Vibrator -> ComponentType (Props ps) State Action
handleSetVibrator mVibrator = do
  state <- get :: Component (Props ps) State State
  let oldVibrator = state.vibrator
  vibrator .= mVibrator
  pure [ liftEff $ do
            traverse_ stopVibration oldVibrator
            pure Nop
       ]

handleTurnOnAlarm :: forall ps. Boolean -> ComponentType (Props ps) State Action
handleTurnOnAlarm onOff = do
  state <- get :: Component (Props ps) State State
  let old = state.alarmIsOn
  modify $ _ { alarmIsOn = onOff }
  let actions =
        if onOff
        then [ do
                  liftEff $ traverse_ playSound state.alarm
                  pure Nop
            ]
        else [ do
                  liftEff $ traverse_ stopSound state.alarm
                  pure Nop
            ]
  if old /= onOff
    then pure actions
    else pure []


handleRegisterMediaTrack :: forall ps. TrackKind -> Component (Props ps) State Unit
handleRegisterMediaTrack AudioTrack = audioStats <<< connectionState .= ConnectionUnknown
handleRegisterMediaTrack VideoTrack = videoStats <<< connectionState .= ConnectionUnknown

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
  state <- get :: Component (Props ps) State State
  let conn = state.rtcConnection
  audioStats <<< connectionState .= ConnectionNotAvailable
  videoStats <<< connectionState .= ConnectionNotAvailable
  alarmA <- handleTurnOnAlarm false
  pure $ [ liftEff $ do
            catchException (\_ -> Console.log "Stopping vibrations failed!")
              $ traverse_ stopVibration state.vibrator
            pure $ SetVibrator Nothing
        , liftEff $ do
            catchException (\_ -> Console.log "Closing RTC connection failed!")
              $ closeRTCPeerConnection conn
            pure ConnectionClosed
        ]
    <> alarmA


handleCloseConnection :: forall ps. ComponentType (Props ps) State Action
handleCloseConnection = do
  state <- get :: Component (Props ps) State State
  closeActions <- closeConnection
  pure $ [ pure $ EnqueueMessage Message.CloseConnection ] <> closeActions

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
            , H.div [ A.style [Tuple "position" "relative"]] -- Needed so absolute positioning of error works.
              [ viewVideo state
              , viewError state
              ]
            , viewStreamStatus state
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

viewError :: State -> Html Action
viewError state =
  let
    isBroken connState = case connState of
      ConnectionDied -> true
      _              -> false
    audioBroken = isBroken state.audioStats.connectionState
    videoBroken = isBroken state.videoStats.connectionState
    brokenText = if audioBroken && not videoBroken
                 then "Audio connection broken!"
                 else if not audioBroken && videoBroken
                      then "Video connection broken!"
                      else "Connection broken!"

    errorView = H.div [ A.className "brokenConnectionOverlay" ]
                [ H.h2 [] [ H.text brokenText ]
                , H.p []
                  [
                    H.text "Please reconnect or check on your baby!"
                  ]
                ]
  in
   if audioBroken || videoBroken
   then errorView
   else H.div [] []

viewStreamStatus :: State -> Html Action
viewStreamStatus state =
  let
     getStatus connState = case connState of
       ConnectionNotAvailable -> H.text "x"
       ConnectionUnknown  -> H.text "Might break unnoticed"
       ConnectionReliable -> H.text "Reliable"
       ConnectionDied     -> H.text "Dead"
     videoStatus = getStatus state.videoStats.connectionState
     audioStatus = getStatus state.audioStats.connectionState
  in
   H.div []
   [ H.text "Audio: " , audioStatus
   , H.text ", "
   , H.text "Video: ", videoStatus
   ]


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

