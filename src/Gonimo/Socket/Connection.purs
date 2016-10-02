module Gonimo.Socket.Connection where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader.Class (class MonadReader)
import Data.Maybe (maybe, Maybe(Nothing, Just))
import Gonimo.Client.Types (Settings, GonimoEff)
import Gonimo.Pux (justEffect, noEffects, EffModel(EffModel))
import Gonimo.Server.DbEntities (Family(Family), Device(Device))
import Gonimo.Socket.Message (decodeFromString, Message)
import Gonimo.Types (Secret(Secret), Key(Key))
import Gonimo.Util (coerceEffects)
import Gonimo.WebAPI.Subscriber (receiveSocketByFamilyIdByFromDeviceByToDeviceByChannelId)
import Servant.Subscriber (Subscriptions)
import WebRTC.MediaStream (getUserMedia, MediaStreamConstraints(MediaStreamConstraints), MediaStream)
import WebRTC.RTC (RTCPeerConnection, Ice, newRTCPeerConnection)

type State =
  { mediaStream :: Maybe MediaStream
  , rtcConnection :: RTCPeerConnection
  }

type Props ps =
  { ourId :: Key Device
  , theirId :: Key Device
  , channel :: Secret
  , familyId :: Key Family
  }

data Action = AcceptMessage Message
            | StartStreaming MediaStreamConstraints
            | StopStreaming
            | SetMediaStream MediaStream
            | Nop


init :: forall eff. Aff (GonimoEff eff) State
init = do
  let
    myNewRTCPeerConnection :: Ice -> Aff () RTCPeerConnection
    myNewRTCPeerConnection ice = liftEff  ( newRTCPeerConnection ice ::  Eff () RTCPeerConnection )

  rtcConnection <- coerceEffects (myNewRTCPeerConnection { iceServers : [ {url : "stun:stun.l.google.com:19302"} ] })
  pure $  { mediaStream : Nothing
          , rtcConnection : rtcConnection
          }



update :: forall eff ps. Props ps -> Action -> State -> EffModel eff State Action
update props action = case action of
  StartStreaming constraints         -> handleStartStreaming constraints
  AcceptMessage  message             -> noEffects
  SetMediaStream stream              -> noEffects <<< (\s -> s { mediaStream = Just stream })
  StopStreaming                      -> noEffects
  Nop                                -> noEffects

handleStartStreaming :: forall eff. MediaStreamConstraints -> State -> EffModel eff State Action
handleStartStreaming constraints = justEffect $ SetMediaStream <$> coerceEffects (getUserMedia constraints)

getSubscriptions :: forall m ps. (MonadReader Settings m) => Props ps -> m (Subscriptions Action)
getSubscriptions props =
  let
    receiveMessage = receiveSocketByFamilyIdByFromDeviceByToDeviceByChannelId
  in
    receiveMessage ((maybe Nop AcceptMessage) <<< (decodeFromString =<< _))
                   props.familyId props.theirId props.ourId props.channel
