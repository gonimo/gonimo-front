module Gonimo.UI.Loaded.Types where

import Prelude
import Gonimo.UI.AcceptInvitation as AcceptC
import Gonimo.UI.Home as HomeC
import Gonimo.UI.Invite as InviteC
import Gonimo.UI.Socket.Lenses as SocketC
import Gonimo.UI.Socket.Types as SocketC
import Data.Generic (class Generic)
import Data.Lens (to, _Just, TraversalP, lens, LensP)
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

type State = { subscriberUrl :: String
             , inviteS  :: InviteC.State
             , acceptS  :: AcceptC.State
             , homeS    :: HomeC.State
             , socketS   :: SocketC.State
             , central  :: Central
             , familyIds :: Array (Key Family)
             , families  :: Map (Key Family) Family
             , onlineDevices :: Array (Tuple (Key Device) DeviceType)
             , deviceInfos :: Array (Tuple (Key Device) DeviceInfo)
             , userError :: UserError
             , url :: String
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
            | InviteA InviteC.Action
            | AcceptA AcceptC.Action
            | HomeA HomeC.Action
            | SocketA SocketC.Action
            | SetFamilyIds (Array (Key Family))
            | UpdateFamily (Key Family) Family
            | SetCentral Central
            | SetOnlineDevices (Array (Tuple (Key Device) DeviceType))
            | SetDeviceInfos (Array (Tuple (Key Device) DeviceInfo))
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
inviteS = lens _.inviteS (_ { inviteS = _ })

acceptS :: LensP State AcceptC.State
acceptS = lens _.acceptS (_ { acceptS = _ })

homeS :: LensP State HomeC.State
homeS = lens _.homeS (_ { homeS = _ })

central :: LensP State Central
central = lens _.central (_ { central = _})

socketS :: LensP State SocketC.State
socketS = lens _.socketS (_ { socketS = _ })

authData :: LensP State AuthData
authData = socketS <<< SocketC.authData

currentFamily :: TraversalP State (Key Family)
currentFamily = socketS <<< SocketC.currentFamily <<< _Just

familyIds :: LensP State (Array (Key Family))
familyIds = lens _.familyIds (_ { familyIds = _ })
