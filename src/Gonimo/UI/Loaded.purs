-- | Loaded application ui logic
module Gonimo.UI.Loaded where

import Gonimo.UI.Html
import Data.List as List
import Data.Generic (class Generic)
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
import Data.Argonaut.Generic.Aeson (decodeJson)
import Data.Array (fromFoldable, concat, catMaybes, head)
import Data.Bifunctor (bimap)
import Data.Either (either, Either(Right, Left))
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
import Gonimo.Pux (onlyEffects, onlyEffect, justEffect, noEffects, EffModel(EffModel))
import Gonimo.Server.DbEntities (Device(Device), Family(Family))
import Gonimo.Server.DbEntities.Helpers (runFamily)
import Gonimo.Server.Error (ServerError(InvalidAuthToken))
import Gonimo.Server.Types (DeviceType(NoBaby), AuthToken, AuthToken(GonimoSecret))
import Gonimo.Types (dateToString, Key(Key), Secret(Secret))
import Gonimo.WebAPI (SPParams_(SPParams_), postAccounts)
import Gonimo.WebAPI.Types (DeviceInfo(DeviceInfo), AuthData(AuthData))
import Gonimo.WebAPI.Types.Helpers (runAuthData)
import Partial.Unsafe (unsafeCrashWith)
import Pux (renderToDOM, fromSimple, start)
import Pux.Html (h3, h2, td, tbody, th, tr, thead, table, ul, p, button, input, h1, text, span, Html, img, div)
import Pux.Html.Attributes (offset)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (defaultSettings, SPSettings_(SPSettings_))
import Servant.Subscriber (Subscriber)
import Servant.Subscriber.Connection (Notification(HttpRequestFailed, ParseError, WebSocketClosed, WebSocketError))
import Servant.Subscriber.Internal (doDecode, coerceEffects)
import Servant.Subscriber.Request (HttpRequest(HttpRequest))
import Servant.Subscriber.Response (HttpResponse(HttpResponse))
import Servant.Subscriber.Subscriptions (Subscriptions)
import Servant.Subscriber.Util (toUserType)
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
             , userError :: UserError
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
            | ResetDevice -- Reinitialize basically everything.
            | Nop

data Central = CentralInvite
             | CentralAccept

derive instance genericCentral :: Generic Central

setCentral :: Central -> State -> State
setCentral central = _ { central = central }

-- | Errors which are of concern for the user and should be displayed.
data UserError = NoError
               | DeviceInvalid -- Server does not accept our device secret.
               | Crashed -- An unexpected error occurred - we can not continue. - The user obviously should never see this in production.

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
update (HandleSubscriber msg)     = handleSubscriber msg
update ResetDevice                = noEffects
update Nop                        = noEffects


updateInvite :: forall eff. InviteC.Action -> State -> EffModel eff State Action
updateInvite action state = bimap (state {inviteS = _}) InviteA
                            $ InviteC.update state.settings action (getInviteState state)

updateAccept :: forall eff. AcceptC.Action -> State -> EffModel eff State Action
updateAccept action state = bimap (state {acceptS = _}) AcceptA
                            $ AcceptC.update state.settings action state.acceptS

inviteEffect :: forall m. Monad m => Secret -> m Action
inviteEffect = pure <<< AcceptA <<< AcceptC.LoadInvitation

handleSubscriber :: forall eff. Notification -> State -> EffModel eff State Action
handleSubscriber notification state =
  case notification of
    WebSocketError msg           -> onlyEffect state $ do
      Gonimo.log $ "A websocket error occurred: " <> msg
      pure Nop
    WebSocketClosed msg          -> onlyEffect state $ do
      Gonimo.log $ "The websocket connection got closed: " <> msg
      pure Nop
    ParseError msg               -> onlyEffect state $ do
      Gonimo.log $ "A parsing error occurred: " <> msg
      pure Nop
    HttpRequestFailed req' resp' -> handleFailedHttpRequest state req' resp'

handleFailedHttpRequest :: forall eff. State -> HttpRequest -> HttpResponse -> EffModel eff State Action
handleFailedHttpRequest state req' fResp@(HttpResponse resp') = onlyEffects newState $ case decodingResult of
    Right _ -> []
    Left err -> [ do
                     Gonimo.log $ "Decoding error while handling failed http request: " <> err
                     Gonimo.log $ "For request: " <> gShow req'
                     Gonimo.log $ "Response was: " <> gShow fResp
                     pure Nop
                ]
  where
    newState = case error of
        NoError -> state
        _       -> state { userError = error }
    fromServer :: Maybe ServerError -> UserError
    fromServer mErr = case mErr of
        Nothing -> NoError
        Just InvalidAuthToken -> DeviceInvalid
        Just _ -> Crashed -- TODO: Check other errors as well.
    decodingResult = doDecode (toUserType fromServer) resp'.httpBody
    error = either (const Crashed) id decodingResult
--------------------------------------------------------------------------------

view :: State -> Html Action
view state =
  let
    numDevices = Map.size state.onlineDevices
  in
    case state.userError of
      NoError ->
        div []
        [ viewCentral state
        , div []
          [ h3 [] [ text $ show numDevices <> " Device(s) currently online:" ]
          , div [] [ viewOnlineDevices state ]
          ]
        ]
      err -> viewError state

viewCentral :: State -> Html Action
viewCentral state = case state.central of
  CentralInvite -> map InviteA $ InviteC.view (getInviteState state)
  CentralAccept -> map AcceptA $ AcceptC.view state.acceptS

viewOnlineDevices :: State -> Html Action
viewOnlineDevices state = table [ A.className "table table-stripped"]
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

viewError :: State -> Html Action
viewError state = case state.userError of
  NoError -> text "No error occurred - everything is fine!"
  DeviceInvalid ->
    div [ A.className "alert alert-danger", A.role "alert"]
    [ h1 [] [ text "Your account is no (longer) known by gonimo server!"]
    , p []
      [ text $ "We are sorry, but your account is no longer known by our server."
            <> "To proceed I would create a new account for you. You will have to rejoin any"
            <> "families you were in already. I am sorry for any inconveniences!"
      ]
    , button [ E.onClick $ const ResetDevice ]
      [ text "Proceed"]
    ]
  Crashed ->
    div [ A.className "alert alert-danger", A.role "alert"]
    [ h1 [] [ text "Your gonimo has crashed!"]
    , p []
      [ text $ "Something unexpected happened - and I don't know what to do!"
            <> "Please try again later - hopefully we have fixed the problem then!"
      ]
    ]
--------------------------------------------------------------------------------

getSubscriptions :: State -> Subscriptions Action
getSubscriptions state =
  let
    familyId = fst <$> head state.families -- Currently we just pick the first family
    subArray :: Array (Subscriptions Action)
    subArray = map (flip runReader state.settings) <<< concat $
      [ [ Sub.getAccountsByAccountIdFamilies (maybe Nop SetFamilies)
                                          (runAuthData state.authData).accountId
        ]
      , fromFoldable $
        Sub.getOnlineStatusByFamilyId (maybe Nop SetOnlineDevices) <$> familyId
      , fromFoldable $
        Sub.getDeviceInfosByFamilyId (maybe Nop SetDeviceInfos) <$> familyId
      ]
  in
   case state.userError of
     NoError -> foldl append mempty subArray
     _       -> mempty

getPongRequest :: State -> Maybe HttpRequest
getPongRequest state =
  let
    deviceId = (runAuthData state.authData).deviceId
    deviceData = Tuple deviceId NoBaby
    familyId = fst <$> head state.families -- Currently we just pick the first family
  in
   case state.userError of
     NoError -> flip runReader state.settings <<< Reqs.postOnlineStatusByFamilyId deviceData <$> familyId
     _ -> Nothing

getCloseRequest :: State -> Maybe HttpRequest
getCloseRequest state =
  let
    deviceId = (runAuthData state.authData).deviceId
    familyId = fst <$> head state.families -- Currently we just pick the first family
  in
   case state.userError of
     NoError -> flip runReader state.settings <<< flip Reqs.deleteOnlineStatusByFamilyIdByDeviceId deviceId <$> familyId
     _ -> Nothing

