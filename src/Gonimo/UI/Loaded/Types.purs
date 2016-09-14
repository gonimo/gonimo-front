module Gonimo.UI.Loaded.Types where

import Prelude
import Gonimo.UI.AcceptInvitation as AcceptC
import Gonimo.UI.Invite as InviteC
import Data.Generic (class Generic)
import Data.Map (Map)
import Data.Tuple (Tuple(Tuple))
import Gonimo.Client.Types (class ReportErrorAction, GonimoError)
import Gonimo.Server.DbEntities (Device(Device), Family(Family))
import Gonimo.Server.Types (DeviceType)
import Gonimo.Types (Secret(Secret), Key(Key))
import Gonimo.WebAPI.Types (DeviceInfo(DeviceInfo), AuthData(AuthData))
import Servant.Subscriber.Connection (Notification)

type State = { authData :: AuthData
             , subscriberUrl :: String
             , inviteS  :: InviteC.State
             , acceptS  :: AcceptC.State
             , central  :: Central
             , families :: Array (Tuple (Key Family) Family)
             , onlineDevices :: Map (Key Device) DeviceType
             , deviceInfos :: Map (Key Device) DeviceInfo
             , userError :: UserError
             }

data Action = ReportError GonimoError
            | SetState State
            | SetAuthData AuthData
            | InviteA InviteC.Action
            | AcceptA AcceptC.Action
            | HandleInvite Secret
            | SetFamilies (Array (Tuple (Key Family) Family))
            | SetOnlineDevices (Array (Tuple (Key Device) DeviceType))
            | SetDeviceInfos (Array (Tuple (Key Device) DeviceInfo))
            | HandleSubscriber Notification
            | ResetDevice -- Reinitialize basically everything.
            | Nop

data Central = CentralInvite
             | CentralAccept

derive instance genericCentral :: Generic Central

instance reportErrorActionAction :: ReportErrorAction Action where
  reportError = ReportError

setCentral :: Central -> State -> State
setCentral central = _ { central = central }

-- | Errors which are of concern for the user and should be displayed.
data UserError = NoError
               | DeviceInvalid -- Server does not accept our device secret.
               | Crashed -- An unexpected error occurred - we can not continue. - The user obviously should never see this in production.
