module Gonimo.Socket.Channel where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.IO (IO)
import Control.Monad.Reader.Class (class MonadReader)
import Data.Maybe (maybe, Maybe(Nothing, Just))
import Gonimo.Client.Types (Settings, GonimoEff)
import Gonimo.Pux (noEffects, Update, onlyModify, ComponentType)
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
  , cSecret :: Secret
  , ourFamilyId :: Key Family
  }

data Action = AcceptMessage Message
            | StartStreaming MediaStreamConstraints
            | StopStreaming
            | SetMediaStream MediaStream
            | CloseConnection
            | Nop


init :: IO State
init = do
  rtcConnection <- liftEff (newRTCPeerConnection { iceServers : [ {url : "stun:stun.l.google.com:19302"} ] })
  pure $  { mediaStream : Nothing
          , rtcConnection : rtcConnection
          }



update :: forall ps. Update (Props ps) State Action
update action = case action of
  StartStreaming constraints         -> handleStartStreaming constraints
  AcceptMessage  message             -> noEffects
  SetMediaStream stream              -> onlyModify $ _ { mediaStream = Just stream }
  StopStreaming                      -> noEffects
  CloseConnection                    -> noEffects
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
  in
    receiveMessage ((maybe Nop AcceptMessage) <<< (decodeFromString =<< _))
                   props.ourFamilyId props.theirId props.ourId props.cSecret
