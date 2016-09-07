-- | Loaded application ui logic
module Gonimo.UI.Loaded where

import Gonimo.UI.Html
import Data.List as List
import Data.Map as Map
import Gonimo.Client.Effects as Gonimo
import Gonimo.Client.LocalStorage as Key
import Gonimo.Client.Types as Gonimo
import Gonimo.UI.AcceptInvitation as AcceptC
import Gonimo.UI.Invite as InviteC
import Gonimo.WebAPI.MakeRequests as Reqs
import Gonimo.WebAPI.Subscriber as Sub
import Pux.Html.Attributes as A
import Pux.Html.Events as E
import Servant.Subscriber as Sub
import Servant.Subscriber.Connection as Sub
import Browser.LocalStorage (STORAGE, localStorage)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Reader (runReader)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Array (fromFoldable, concat, catMaybes, head)
import Data.Bifunctor (bimap)
import Data.Either (Either(Right, Left))
import Data.Foldable (foldl)
import Data.Generic (gShow)
import Data.Map (Map)
import Data.Maybe (maybe, Maybe(..))
import Data.Monoid (mempty)
import Data.Semigroup (append)
import Data.Traversable (traverse)
import Data.Tuple (fst, Tuple(Tuple))
import Debug.Trace (trace)
import Gonimo.Client.Effects (handleError)
import Gonimo.Client.Types (GonimoEff, runGonimoT, Settings)
import Gonimo.Pux (justEffect, noEffects, onlyEffects, EffModel(EffModel))
import Gonimo.Server.DbEntities (Device(Device), Family(Family))
import Gonimo.Server.DbEntities.Helpers (runFamily)
import Gonimo.Server.Types (DeviceType(NoBaby), AuthToken, AuthToken(GonimoSecret))
import Gonimo.Types (dateToString, Key(Key), Secret(Secret))
import Gonimo.WebAPI (SPParams_(SPParams_), postAccounts)
import Gonimo.WebAPI.Types (DeviceInfo(DeviceInfo), AuthData(AuthData))
import Gonimo.WebAPI.Types.Helpers (runAuthData)
import Partial.Unsafe (unsafeCrashWith)
import Pux (renderToDOM, fromSimple, start)
import Pux.Html (h3, h2, td, tbody, th, tr, thead, table, ul, p, button, input, h1, text, span, Html, img, div)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (defaultSettings, SPSettings_(SPSettings_))
import Servant.Subscriber (Subscriber)
import Servant.Subscriber.Connection (Notification(HttpRequestFailed))
import Servant.Subscriber.Internal (coerceEffects)
import Servant.Subscriber.Request (HttpRequest(HttpRequest))
import Servant.Subscriber.Subscriptions (Subscriptions)
import Signal (constant, Signal)
import Prelude hiding (div)

type State = { authData :: AuthData
             , settings :: Settings
             , subscriberUrl :: String
             , inviteS  :: InviteC.State
             , acceptS  :: AcceptC.State
             , central  :: Central
             , families :: Array (Tuple (Key Family) Family)
             , onlineDevices :: Map (Key Device) DeviceType
             , deviceInfos :: Map (Key Device) DeviceInfo
             }

data Action = ReportError Gonimo.Error
            | SetState State
            | InviteA InviteC.Action
            | AcceptA AcceptC.Action
            | HandleInvite Secret
            | SetFamilies (Array (Tuple (Key Family) Family))
            | SetOnlineDevices (Array (Tuple (Key Device) DeviceType))
            | SetDeviceInfos (Array (Tuple (Key Device) DeviceInfo))
            | HandleSubscriber Notification
            | Nop

data Central = CentralInvite
             | CentralAccept

setCentral :: Central -> State -> State
setCentral central = _ { central = central }

getInviteState :: State -> InviteC.State
getInviteState state = case head state.families of
  Nothing -> state.inviteS
  Just (Tuple key family) -> state.inviteS { familyId = Just key
                                           , familyName = (runFamily family).familyName
                                           }
--------------------------------------------------------------------------------

update :: forall eff. Action -> State -> EffModel eff State Action
update (SetState state)           = const $ noEffects state
update (ReportError err)          = justEffect $ handleError Nop err
update (InviteA action)           = updateInvite action
update (HandleInvite secret)      = setCentral CentralAccept
                                    >>> justEffect (inviteEffect secret)
update (AcceptA action)           = updateAccept action
update (SetFamilies families')    = \state -> noEffects (state { families = families'})
update (SetOnlineDevices devices) = \state -> noEffects (state {onlineDevices = Map.fromFoldable devices})
update (SetDeviceInfos devices)   = \state -> noEffects (state {deviceInfos = Map.fromFoldable devices})
update (HandleSubscriber msg)     = justEffect (do
                                            Gonimo.log <<< gShow $ msg
                                            pure Nop)
update Nop                        = noEffects


updateInvite :: forall eff. InviteC.Action -> State -> EffModel eff State Action
updateInvite action state = bimap (state {inviteS = _}) InviteA
                            $ InviteC.update state.settings action (getInviteState state)

updateAccept :: forall eff. AcceptC.Action -> State -> EffModel eff State Action
updateAccept action state = bimap (state {acceptS = _}) AcceptA
                            $ AcceptC.update state.settings action state.acceptS

inviteEffect :: forall m. Monad m => Secret -> m Action
inviteEffect = pure <<< AcceptA <<< AcceptC.LoadInvitation

--------------------------------------------------------------------------------

view :: State -> Html Action
view state =
  let
    numDevices = Map.size state.onlineDevices
  in
    div []
    [ viewCentral state
    , div []
      [ h3 [] [ text $ show numDevices <> " Device(s) currently online:" ]
      , div [] [ viewOnlineDevices state ]
      ]
    ]

viewCentral :: State -> Html Action
viewCentral state = case state.central of
  CentralInvite -> map InviteA $ InviteC.view (getInviteState state)
  CentralAccept -> map AcceptA $ AcceptC.view state.acceptS

viewOnlineDevices :: State -> Html Action
viewOnlineDevices state = table [ A.className "table", A.className "table-striped"]
                          [ head
                          , body
                          ]
  where
    head = thead []
           [ tr []
             [ th [] [ text "Name" ]
             , th [] [ text "Last Accessed"]
             ]
           ]
    body = tbody []
           <<< fromFoldable
           <<< List.mapMaybe viewOnlineDevice
           $ Map.keys state.onlineDevices
    viewOnlineDevice deviceId = do
        (DeviceInfo info) <- Map.lookup deviceId state.deviceInfos
        let name = info.deviceInfoName
        let lastAccessed = dateToString info.deviceInfoLastAccessed
        pure $ tr []
               [ td [] [ text name ]
               , td [] [ text lastAccessed ]
               ]

--------------------------------------------------------------------------------

getSubscriptions :: State -> Subscriptions Action
getSubscriptions state =
  let
    familyId = fst <$> head state.families -- Currently we just pick the first family
    subArray :: Array (Subscriptions Action)
    subArray = map (flip runReader state.settings) <<< concat $
      [ [ Sub.getFamiliesByAccountId (maybe Nop SetFamilies)
                                          (runAuthData state.authData).accountId
        ]
      , fromFoldable $
        Sub.getOnlineStatusByFamilyId (maybe Nop SetOnlineDevices) <$> familyId
      , fromFoldable $
        Sub.getDeviceInfosByFamilyId (maybe Nop SetDeviceInfos) <$> familyId
      ]
  in
   foldl append mempty subArray

getPongRequest :: State -> Maybe HttpRequest
getPongRequest state =
  let
    deviceId = (runAuthData state.authData).deviceId
    deviceData = Tuple deviceId NoBaby
    familyId = fst <$> head state.families -- Currently we just pick the first family
  in
    flip runReader state.settings <<< Reqs.postOnlineStatusByFamilyId deviceData <$> familyId

getCloseRequest :: State -> Maybe HttpRequest
getCloseRequest state =
  let
    deviceId = (runAuthData state.authData).deviceId
    familyId = fst <$> head state.families -- Currently we just pick the first family
  in
    flip runReader state.settings <<< flip Reqs.deleteOnlineStatusByFamilyIdByDeviceId deviceId <$> familyId
