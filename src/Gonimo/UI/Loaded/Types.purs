module Gonimo.UI.Loaded.Types where

import Prelude
import Data.Map as Map
import Gonimo.UI.AcceptInvitation as AcceptC
import Gonimo.UI.Invite as InviteC
import Gonimo.UI.Overview as OverviewC
import Gonimo.UI.Socket.Lenses as SocketC
import Gonimo.UI.Socket.Types as SocketC
import Control.Monad.Eff (Eff)
import Control.Monad.IO (IO)
import Data.Either (Either(Left, Right))
import Data.Generic (class Generic)
import Data.Lens ((^?), (^.), prism, PrismP, to, _Just, TraversalP, lens, LensP)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Profunctor (lmap)
import Data.Tuple (Tuple(Tuple))
import Gonimo.Client.Types (Settings, class ReportErrorAction, GonimoError)
import Gonimo.Server.Db.Entities (Device(Device), Family(Family))
import Gonimo.Server.Types (DeviceType)
import Gonimo.Types (Secret(Secret), Key(Key))
import Gonimo.UI.Error (class ErrorAction, UserError)
import Gonimo.Util (class UserShow)
import Gonimo.WebAPI (SPParams_(SPParams_))
import Gonimo.WebAPI.Types (DeviceInfo(DeviceInfo), AuthData(AuthData))
import Gonimo.WebAPI.Types.Helpers (runAuthData)
import Servant.PureScript.Settings (defaultSettings)
import Servant.Subscriber.Connection (Notification)

type State = { subscriberUrl :: String
             , overviewS    :: OverviewC.State
             , socketS   :: SocketC.State
             , central  :: Central
             , familyIds :: Array (Key Family)
             , families  :: Map (Key Family) Family
             , onlineDevices :: Array (Tuple (Key Device) DeviceType)
             , deviceInfos :: Array (Tuple (Key Device) DeviceInfo)
             , userError :: UserError
             , url :: String
             , sendAction :: Action -> Eff () Unit
             }

type Props = { settings :: Settings
             , deviceId :: Key Device
             , familyId :: Maybe (Key Family)
             , family :: Maybe Family
             , onlineStatus :: DeviceType
             , onlineDevices :: Array (Tuple (Key Device) DeviceType)
             , deviceInfos :: Array (Tuple (Key Device) DeviceInfo)
             , sendActionSocket :: SocketC.Action -> Eff () Unit
             , socketS   :: SocketC.State
             }

type InviteProps = InviteC.Props ()

data Action = ReportError GonimoError
            | SetState State
            | InviteA (Maybe InviteProps) InviteC.Action
            | AcceptA AcceptC.Action
            | OverviewA OverviewC.Action
            | SocketA SocketC.Action
            | SetFamilyIds (Array (Key Family))
            | UpdateFamily (Key Family) Family
            | SetCentral Central
            | RequestCentral CentralReq
            | SetOnlineDevices (Array (Tuple (Key Device) DeviceType))
            | SetDeviceInfos (Array (Tuple (Key Device) DeviceInfo))
            | SetURL String
            | HandleSubscriber Notification
            | ResetDevice -- Reinitialize basically everything.
            | ClearError
            | Nop

data Central = CentralInvite InviteC.State
             | CentralAccept AcceptC.State
             | CentralBaby
             | CentralOverview


data CentralReq = ReqCentralInvite
                | ReqCentralOverview
                | ReqCentralBaby

mkProps :: State -> Props
mkProps state = { settings : mkSettings $ state^.authData
                , deviceId : (runAuthData $ state^.authData).deviceId
                , familyId : state^?currentFamily
                , onlineStatus  : state^.socketS <<< to SocketC.toDeviceType
                , family : flip Map.lookup state.families =<< state^?currentFamily
                , onlineDevices : state.onlineDevices
                , deviceInfos : state.deviceInfos
                , sendActionSocket : lmap SocketA state.sendAction
                , socketS : state.socketS
                }

mkInviteProps :: State -> Maybe InviteProps
mkInviteProps state = do
  familyId' <- state^?currentFamily
  family' <- Map.lookup familyId' state.families
  pure $ mkInviteProps' familyId' family' state

mkInviteProps' :: Key Family -> Family -> State -> InviteProps
mkInviteProps' familyId' family' state =
  { settings : mkSettings $ state^.authData
  , rFamilyId : familyId'
  , rFamily : family'
  , baseURL : state.url
  }

mkSettings :: AuthData -> Settings
mkSettings (AuthData auth) = defaultSettings $ SPParams_ {
      authorization : auth.authToken
    , baseURL       : "http://localhost:8081/"
    }

instance reportErrorActionAction :: ReportErrorAction Action where
  reportError = ReportError

instance errorActionAction :: ErrorAction Action where
  resetDevice = ResetDevice
  clearError  = ClearError
  nop = Nop

_CentralInvite :: PrismP Central InviteC.State
_CentralInvite = prism CentralInvite unwrap
  where
    unwrap (CentralInvite s) = Right s
    unwrap y = Left y

_CentralAccept :: PrismP Central AcceptC.State
_CentralAccept = prism CentralAccept unwrap
  where
    unwrap (CentralAccept s) = Right s
    unwrap y = Left y

inviteS :: TraversalP State InviteC.State
inviteS = central <<< _CentralInvite

acceptS :: TraversalP State AcceptC.State
acceptS = central <<< _CentralAccept

overviewS :: LensP State OverviewC.State
overviewS = lens _.overviewS (_ { overviewS = _ })

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

instance eqCentralReq :: Eq CentralReq where
  eq ReqCentralInvite ReqCentralInvite = true
  eq ReqCentralOverview ReqCentralOverview = true
  eq ReqCentralBaby ReqCentralBaby = true
  eq _ _ = false

instance userShowCentralReq :: UserShow CentralReq where
  userShow req = case req of
    ReqCentralInvite -> "Add Device"
    ReqCentralOverview   -> "Overview"
    ReqCentralBaby   -> "Baby Station"
