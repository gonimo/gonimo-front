module Gonimo.UI.Loaded.Types where

import Prelude
import Gonimo.UI.AcceptInvitation as AcceptC
import Gonimo.UI.Home as HomeC
import Gonimo.UI.Invite as InviteC
import Data.Generic (class Generic)
import Data.Lens (lens, LensP)
import Data.Map (Map)
import Data.Maybe (Maybe)
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
             , _homeS    :: HomeC.State
             , _central  :: Central
             , familyIds :: Array (Key Family)
             , families  :: Map (Key Family) Family
             , currentFamily :: Maybe (Key Family)
             , onlineDevices :: Array (Tuple (Key Device) DeviceType)
             , deviceInfos :: Array (Tuple (Key Device) DeviceInfo)
             , userError :: UserError
             , url :: String
             , onlineStatus :: DeviceType
             }

type Props = { settings :: Settings
             , deviceId :: Key Device
             , familyId :: Maybe (Key Family)
             , family :: Maybe Family
             , onlineStatus :: DeviceType
             , onlineDevices :: Array (Tuple (Key Device) DeviceType)
             , deviceInfos :: Array (Tuple (Key Device) DeviceInfo)
             }

data Action = ReportError GonimoError
            | SetState State
            | SetAuthData AuthData
            | InviteA InviteC.Action
            | AcceptA AcceptC.Action
            | HomeA HomeC.Action
            | SetFamilyIds (Array (Key Family))
            | UpdateFamily (Key Family) Family
            | SetCentral Central
            | ServerFamilyGoOffline (Key Family) -- | A bit of a hack - for reliably switching families
            | SetOnlineDevices (Array (Tuple (Key Device) DeviceType))
            | SetDeviceInfos (Array (Tuple (Key Device) DeviceInfo))
            | SwitchFamily (Key Family)
            | SetURL String
            | HandleSubscriber Notification
            | ResetDevice -- Reinitialize basically everything.
            | ClearError
            | Nop

data Central = CentralInvite
             | CentralAccept
             | CentralHome

centralHome :: Central
centralHome = CentralHome

derive instance genericCentral :: Generic Central

instance reportErrorActionAction :: ReportErrorAction Action where
  reportError = ReportError

instance errorActionAction :: ErrorAction Action where
  resetDevice = ResetDevice
  clearError  = ClearError
  nop = Nop

inviteS :: LensP State InviteC.State
inviteS = lens _._inviteS (_ { _inviteS = _ })

acceptS :: LensP State AcceptC.State
acceptS = lens _._acceptS (_ { _acceptS = _ })

homeS :: LensP State HomeC.State
homeS = lens _._homeS (_ { _homeS = _ })

central :: LensP State Central
central = lens _._central (_ { _central = _})

