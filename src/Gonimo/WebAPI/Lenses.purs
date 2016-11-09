module Gonimo.WebAPI.Lenses where

import Gonimo.WebAPI.Types
import Data.Either as Either
import Data.Lens as Lens
import Prelude as Prelude
import Data.Maybe (Maybe)
import Gonimo.Server.Db.Entities (Device(Device), Account(Account))
import Gonimo.Server.Types (AuthToken)
import Gonimo.Types (Date, Key(Key))


accountId :: forall a b r. Lens.Lens { "accountId" :: a | r } { "accountId" :: b | r } a b
accountId = Lens.lens _."accountId" (_ { "accountId" = _ })

deviceId :: forall a b r. Lens.Lens { "deviceId" :: a | r } { "deviceId" :: b | r } a b
deviceId = Lens.lens _."deviceId" (_ { "deviceId" = _ })

authToken :: forall a b r. Lens.Lens { "authToken" :: a | r } { "authToken" :: b | r } a b
authToken = Lens.lens _."authToken" (_ { "authToken" = _ })

_AuthData :: Lens.PrismP AuthData
               { accountId :: Key Account
               , deviceId :: Key Device
               , authToken :: AuthToken
               }
_AuthData = Lens.prism AuthData unwrap
  where
    unwrap (AuthData x) = Either.Right x
    unwrap y = Either.Left y

invitationInfoFamily :: forall a b r. Lens.Lens { "invitationInfoFamily" :: a | r } { "invitationInfoFamily" :: b | r } a b
invitationInfoFamily = Lens.lens _."invitationInfoFamily" (_ { "invitationInfoFamily" = _ })

invitationInfoSendingDevice :: forall a b r. Lens.Lens { "invitationInfoSendingDevice" :: a | r } { "invitationInfoSendingDevice" :: b | r } a b
invitationInfoSendingDevice = Lens.lens _."invitationInfoSendingDevice" (_ { "invitationInfoSendingDevice" = _ })

invitationInfoSendingUser :: forall a b r. Lens.Lens { "invitationInfoSendingUser" :: a | r } { "invitationInfoSendingUser" :: b | r } a b
invitationInfoSendingUser = Lens.lens _."invitationInfoSendingUser" (_ { "invitationInfoSendingUser" = _ })

_InvitationInfo :: Lens.PrismP InvitationInfo
                     { invitationInfoFamily :: String
                     , invitationInfoSendingDevice :: String
                     , invitationInfoSendingUser :: Maybe String
                     }
_InvitationInfo = Lens.prism InvitationInfo unwrap
  where
    unwrap (InvitationInfo x) = Either.Right x
    unwrap y = Either.Left y

_InvitationAccept :: Lens.PrismP InvitationReply Prelude.Unit
_InvitationAccept = Lens.prism (Prelude.const InvitationAccept) unwrap
  where
    unwrap InvitationAccept = Either.Right Prelude.unit
    unwrap y = Either.Left y

_InvitationReject :: Lens.PrismP InvitationReply Prelude.Unit
_InvitationReject = Lens.prism (Prelude.const InvitationReject) unwrap
  where
    unwrap InvitationReject = Either.Right Prelude.unit
    unwrap y = Either.Left y

deviceInfoName :: forall a b r. Lens.Lens { "deviceInfoName" :: a | r } { "deviceInfoName" :: b | r } a b
deviceInfoName = Lens.lens _."deviceInfoName" (_ { "deviceInfoName" = _ })

deviceInfoAccountId :: forall a b r. Lens.Lens { "deviceInfoAccountId" :: a | r } { "deviceInfoAccountId" :: b | r } a b
deviceInfoAccountId = Lens.lens _."deviceInfoAccountId" (_ { "deviceInfoAccountId" = _ })

deviceInfoLastAccessed :: forall a b r. Lens.Lens { "deviceInfoLastAccessed" :: a | r } { "deviceInfoLastAccessed" :: b | r } a b
deviceInfoLastAccessed = Lens.lens _."deviceInfoLastAccessed" (_ { "deviceInfoLastAccessed" = _ })

deviceInfoUserAgent :: forall a b r. Lens.Lens { "deviceInfoUserAgent" :: a | r } { "deviceInfoUserAgent" :: b | r } a b
deviceInfoUserAgent = Lens.lens _."deviceInfoUserAgent" (_ { "deviceInfoUserAgent" = _ })

_DeviceInfo :: Lens.PrismP DeviceInfo
                 { deviceInfoName :: String
                 , deviceInfoAccountId :: Key Account
                 , deviceInfoLastAccessed :: Date
                 , deviceInfoUserAgent :: String
                 }
_DeviceInfo = Lens.prism DeviceInfo unwrap
  where
    unwrap (DeviceInfo x) = Either.Right x
    unwrap y = Either.Left y
