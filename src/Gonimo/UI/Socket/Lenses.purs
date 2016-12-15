module Gonimo.UI.Socket.Lenses where

import Gonimo.UI.Socket.Types
import Data.Either as Either
import Data.Lens as Lens
import Prelude as Prelude
import Data.Either (Either(Left, Right))
import WebRTC.MediaStream (MediaStreamConstraints(MediaStreamConstraints))


settings :: forall a b r. Lens.Lens { "settings" :: a | r } { "settings" :: b | r } a b
settings = Lens.lens _."settings" (_ { "settings" = _ })

authData :: forall a b r. Lens.Lens { "authData" :: a | r } { "authData" :: b | r } a b
authData = Lens.lens _."authData" (_ { "authData" = _ })

sessionId :: forall a b r. Lens.Lens { "sessionId" :: a | r } { "sessionId" :: b | r } a b
sessionId = Lens.lens _."sessionId" (_ { "sessionId" = _ })

currentFamily :: forall a b r. Lens.Lens { "currentFamily" :: a | r } { "currentFamily" :: b | r } a b
currentFamily = Lens.lens _."currentFamily" (_ { "currentFamily" = _ })

channels :: forall a b r. Lens.Lens { "channels" :: a | r } { "channels" :: b | r } a b
channels = Lens.lens _."channels" (_ { "channels" = _ })

isAvailable :: forall a b r. Lens.Lens { "isAvailable" :: a | r } { "isAvailable" :: b | r } a b
isAvailable = Lens.lens _."isAvailable" (_ { "isAvailable" = _ })

babyName :: forall a b r. Lens.Lens { "babyName" :: a | r } { "babyName" :: b | r } a b
babyName = Lens.lens _."babyName" (_ { "babyName" = _ })

newBabyName :: forall a b r. Lens.Lens { "newBabyName" :: a | r } { "newBabyName" :: b | r } a b
newBabyName = Lens.lens _."newBabyName" (_ { "newBabyName" = _ })

mediaStream :: forall a b r. Lens.Lens { "mediaStream" :: a | r } { "mediaStream" :: b | r } a b
mediaStream = Lens.lens _."mediaStream" (_ { "mediaStream" = _ })

previewEnabled :: forall a b r. Lens.Lens { "previewEnabled" :: a | r } { "previewEnabled" :: b | r } a b
previewEnabled = Lens.lens _."previewEnabled" (_ { "previewEnabled" = _ })

localStream :: forall a b r. Lens.Lens { "localStream" :: a | r } { "localStream" :: b | r } a b
localStream = Lens.lens _."localStream" (_ { "localStream" = _ })

streamURL :: forall a b r. Lens.Lens { "streamURL" :: a | r } { "streamURL" :: b | r } a b
streamURL = Lens.lens _."streamURL" (_ { "streamURL" = _ })

constraints :: forall a b r. Lens.Lens { "constraints" :: a | r } { "constraints" :: b | r } a b
constraints = Lens.lens _."constraints" (_ { "constraints" = _ })

video :: forall a b r. Lens.Lens { "video" :: a | r } { "video" :: b | r } a b
video = Lens.lens _."video" (_ { "video" = _ })

gotStreamAction :: forall a b r. Lens.Lens { "gotStreamAction" :: a | r } { "gotStreamAction" :: b | r } a b
gotStreamAction = Lens.lens _."gotStreamAction" (_ { "gotStreamAction" = _ })

_AcceptConnection :: Lens.PrismP Action ChannelId
_AcceptConnection = Lens.prism AcceptConnection unwrap
  where
    unwrap (AcceptConnection x) = Either.Right x
    unwrap y = Either.Left y

_CloseChannel :: Lens.PrismP Action ChannelId
_CloseChannel = Lens.prism CloseChannel unwrap
  where
    unwrap (CloseChannel x) = Either.Right x
    unwrap y = Either.Left y

_Nop :: Lens.PrismP Action Prelude.Unit
_Nop = Lens.prism (Prelude.const Nop) unwrap
  where
    unwrap Nop = Either.Right Prelude.unit
    unwrap y = Either.Left y

_MediaStreamConstraints :: Lens.PrismP MediaStreamConstraints { video :: Boolean
                         , audio :: Boolean
                         }
_MediaStreamConstraints = Lens.prism MediaStreamConstraints unwrap
  where
    unwrap (MediaStreamConstraints x) = Right x
    unwrap y = Left y
