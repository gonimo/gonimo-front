module Gonimo.UI.Loaded.Types where

import Prelude
import Gonimo.UI.AcceptInvitation as AcceptC
import Gonimo.UI.Invite as InviteC
import Data.Generic (class Generic)
import Data.Lens (lens, LensP)
import Data.Map (Map)
import Data.Tuple (Tuple(Tuple))
import Gonimo.Client.Types (Settings, class ReportErrorAction, GonimoError)
import Gonimo.Server.DbEntities (Device(Device), Family(Family))
import Gonimo.Server.Types (DeviceType)
import Gonimo.Types (Secret(Secret), Key(Key))
import Gonimo.UI.Error (class ErrorAction, UserError)
import Gonimo.WebAPI.Types (DeviceInfo(DeviceInfo), AuthData(AuthData))
import Servant.Subscriber.Connection (Notification)

type State = { authData :: AuthData
             , subscriberUrl :: String
             , _inviteS  :: InviteC.State
             , _acceptS  :: AcceptC.State
             , central  :: Central
             , families :: Array (Tuple (Key Family) Family)
             , onlineDevices :: Map (Key Device) DeviceType
             , deviceInfos :: Map (Key Device) DeviceInfo
             , userError :: UserError
             }

type Props = { settings :: Settings }

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

instance errorActionAction :: ErrorAction Action where
  resetDevice = ResetDevice
  nop = Nop
  
inviteS :: LensP State InviteC.State
inviteS = lens _._inviteS (_ { _inviteS = _ })

acceptS :: LensP State AcceptC.State
acceptS = lens _._acceptS (_ { _acceptS = _ })

setCentral :: Central -> State -> State
setCentral central = _ { central = central }

