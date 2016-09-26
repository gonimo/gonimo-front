-- | Loaded application ui logic
module Gonimo.UI.Loaded where

import Gonimo.UI.Html
import Data.List as List
import Data.Map as Map
import Gonimo.Client.Effects as Gonimo
import Gonimo.Client.LocalStorage as Key
import Gonimo.Client.LocalStorage as Key
import Gonimo.Client.Router as Router
import Gonimo.Client.Types as Gonimo
import Gonimo.UI.AcceptInvitation as AcceptC
import Gonimo.UI.Error as Error
import Gonimo.UI.Invite as InviteC
import Gonimo.UI.Home as HomeC
import Gonimo.WebAPI.MakeRequests as Reqs
import Gonimo.WebAPI.Subscriber as Sub
import Pux.Html.Attributes as A
import Pux.Html.Attributes.Bootstrap as A
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
import Data.Generic (class Generic)
import Data.Generic (gShow)
import Data.Map (Map)
import Data.Maybe (maybe, Maybe(..))
import Data.Monoid (mempty)
import Data.Semigroup (append)
import Data.String (takeWhile)
import Data.Traversable (traverse)
import Data.Tuple (fst, Tuple(Tuple))
import Debug.Trace (trace)
import Gonimo.Client.Types (Settings, GonimoError, class ReportErrorAction, Gonimo, GonimoEff, runGonimoT)
import Gonimo.Pux (updateChild, onlyGonimo, onlyEffects, onlyEffect, justEffect, noEffects, EffModel(EffModel))
import Gonimo.Server.DbEntities (Device(Device), Family(Family))
import Gonimo.Server.DbEntities.Helpers (runFamily)
import Gonimo.Server.Error (ServerError(InvalidAuthToken))
import Gonimo.Server.Types (DeviceType(NoBaby), AuthToken, AuthToken(GonimoSecret))
import Gonimo.Types (dateToString, Key(Key), Secret(Secret))
import Gonimo.UI.Error (viewError, class ErrorAction, UserError(NoError, DeviceInvalid), handleSubscriber, handleError)
import Gonimo.UI.Loaded.Types (centralHome, Props, acceptS, inviteS, State, Action(..), Central(..))
import Gonimo.WebAPI (SPParams_(SPParams_), postAccounts)
import Gonimo.WebAPI.Types (DeviceInfo(DeviceInfo), AuthData(AuthData))
import Gonimo.WebAPI.Types.Helpers (runAuthData)
import Partial.Unsafe (unsafeCrashWith)
import Pux (renderToDOM, fromSimple, start)
import Pux.Html (text, small, script, li, a, nav, h3, h2, td, tbody, th, tr, thead, table, ul, p, button, input, h1, span, Html, img, div)
import Pux.Html.Attributes (offset)
import Pux.Router (navigateTo)
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


update :: forall eff. Unit -> Action -> State -> EffModel eff State Action
update _ (SetState state)                    = const $ noEffects state
update _ (ReportError err)                   = handleError err
update _ (InviteA (InviteC.ReportError err)) = handleError err
update _ (InviteA action)                    = updateInvite action
update _ (AcceptA (AcceptC.ReportError err)) = handleError err
update _ (AcceptA action)                    = updateAccept action
update _ (SetFamilies families')             = \state -> noEffects (state { families = families'})
update _ (SetCentral c)                      = \state -> noEffects (state { _central = c })
update _ (SetOnlineDevices devices)          = \state -> noEffects (state {onlineDevices = Map.fromFoldable devices})
update _ (SetDeviceInfos devices)            = \state -> noEffects (state {deviceInfos = Map.fromFoldable devices})
update _ (SetURL url)                        = handleSetURL url
update _ (HandleSubscriber msg)              = handleSubscriber msg
update _ ResetDevice                         = handleResetDevice
update _ ClearError                          = handleClearError
update _ (SetAuthData auth)                  = handleSetAuthData auth
update _ StartBabyStation                    = handleStartBabyStation
update _ Nop                                 = noEffects
update _ _                                   = noEffects


updateInvite :: forall eff. InviteC.Action -> State -> EffModel eff State Action
updateInvite action state = updateChild inviteS InviteA InviteC.update (mkProps state) action state

updateAccept :: forall eff. AcceptC.Action -> State -> EffModel eff State Action
updateAccept action state = updateChild acceptS AcceptA AcceptC.update (mkProps state) action state

inviteEffect :: forall m. Monad m => Secret -> m Action
inviteEffect = pure <<< AcceptA <<< AcceptC.LoadInvitation

handleResetDevice :: forall eff. State -> EffModel eff State Action
handleResetDevice state = onlyGonimo (mkProps state) state $ do
  liftEff $ localStorage.removeItem Key.authData
  SetAuthData <$> getAuthData

handleSetAuthData :: forall eff. AuthData -> State -> EffModel eff State Action
handleSetAuthData auth state = noEffects $ state
                                  { authData = auth
                                  , userError = case state.userError of
                                                  DeviceInvalid -> NoError
                                                  _ -> state.userError
                                  }

handleSetURL :: forall eff. String -> State -> EffModel eff State Action
handleSetURL url state = let
    route = Router.match url
    withoutQuery = takeWhile (_ /= '?') url
    navigateTo' :: String -> Eff (GonimoEff eff) Unit
    navigateTo' = coerceEffects <<< navigateTo
  in
    case route of
      Router.Home -> noEffects $ state { url = url }
      Router.AcceptInvitation s ->
        EffModel
          { state : state { url = withoutQuery
                          , _central = CentralAccept
                          }
          , effects : [ do
                          liftEff $ navigateTo' withoutQuery
                          inviteEffect s
                      ]
          }

handleClearError :: forall eff. State -> EffModel eff State Action
handleClearError state = let
  newCentral = case state._central of
    CentralAccept -> case state._acceptS of
      Nothing -> centralHome
      Just _ -> state._central
    _ -> state._central
  in
   noEffects (state { userError = NoError
                    , _central = newCentral
                    })

handleStartBabyStation :: forall eff. State -> EffModel eff State Action
handleStartBabyStation state = noEffects $ state { isBabyStation = true }
--------------------------------------------------------------------------------

view :: State -> Html Action
view state =
  let
    numDevices = Map.size state.onlineDevices
  in
    case state.userError of
      NoError ->
        div []
        [ viewHeader state
        , viewNavbar state
        , viewCentral state
        , div []
          [ h3 [] [ text $ show numDevices <> " Device(s) currently online:" ]
          , div [] [ viewOnlineDevices state ]
          ]
        ]
      err -> viewError state

viewHeader :: State -> Html Action
viewHeader state =
      div [ A.className "page-header" ]
      [ h1 []
        [ text "gonimo.com "
        , small [] [ text "Good Night Monitor!" ]
        ]
      ]
viewNavbar :: State -> Html Action
viewNavbar state =
       nav [ A.className ".navbar .navbar-default" ]
        [ div [ A.className "container-fluid" ]
          [ -- Brand and toggle get grouped for better mobile display
            div [ A.className "navbar-header" ]
            [ button [ A.type_ "button"
                     , A.className "navbar-toggle collapsed"
                     , A.dataToggle "collapse"
                     , A.dataTarget "navbar-collapse-1"
                     ]
              [ span [ A.className "sr-only" ] [ text "Toggle navigation" ]
              , span [ A.className "icon-bar" ] []
              , span [ A.className "icon-bar" ] []
              , span [ A.className "icon-bar" ] []
              ]
            , a [ A.className "navbar-brand"
                , A.role "button"
                , E.onClick $ const $ SetCentral centralHome
                ]
              [ text "Home" ]
            ]
          , -- Collect the nav links, forms, and other content for toggling
            div [ A.className "collapse navbar-collapse", A.id_ "navbar-collapse-1" ]
            [
              ul [ A.className "nav navbar-nav" ]
              [ li []
                [ a [ A.role "button", E.onClick $ const $ SetCentral centralHome ]
                  [ text "Home again ;-)" ]
                ]
              ]
            , ul [ A.className "nav navbar-nav navbar-right" ]
              [ li [ A.className "dropdown" ]
                [ a [ A.className "dropdown-toggle"
                         , A.dataToggle "dropdown"
                         , A.role "button"
                         ]
                  [ text "Account"
                  , span [ A.className "carret" ] []
                  ]
                  , ul [ A.className "dropdown-menu" ]
                    [ li []
                      [ a [ A.href "#" ] [ text "Configure User Details" ] ]
                    , li []
                      [ a [ A.href "#" ] [ text "change password" ] ]
                    , li [ A.role "separator",  A.className "divider" ] []
                    , li []
                      [ a [ A.href "#" ] [ text "Log out" ] ]
                    ]
                  ]
                ]
              ]
            ]
          ]

viewCentral :: State -> Html Action
viewCentral state = case state._central of
  CentralInvite -> map InviteA $ InviteC.view state._inviteS
  CentralAccept -> map AcceptA $ AcceptC.view state._acceptS
  CentralHome   -> div [] [] ---map HomeA   $ HomeC.view   state._homeS

viewHome :: State -> Html Action
viewHome _ =
  div [ A.className "jumbotron" ]
  [ div [ A.className "container" ]
    [ button [ A.className "btn btn-block btn-info"
             , A.style [Tuple "margin-left" "0px"]
             , A.type_ "button"
             , E.onClick $ const $ StartBabyStation
             ]
             [ text "Start Baby Station"
             , span [A.className "glyphicon glyphicon-transfer"] []
             ]
    ]
  ]

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
--------------------------------------------------------------------------------
getSubscriptions :: State -> Subscriptions Action
getSubscriptions state =
  let
    familyId = fst <$> head state.families -- Currently we just pick the first family
    subArray :: Array (Subscriptions Action)
    subArray = map (flip runReader (mkSettings state.authData)) <<< concat $
      [ [ Sub.getAccountsByAccountIdFamilies (maybe Nop SetFamilies)
                                          (runAuthData state.authData).accountId
        ]
      , fromFoldable $
        Sub.getOnlineStatusByFamilyId (maybe Nop SetOnlineDevices) <$> familyId
      , fromFoldable $
        Sub.getFamiliesByFamilyIdDeviceInfos (maybe Nop SetDeviceInfos) <$> familyId
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
     NoError -> flip runReader (mkSettings state.authData) <<< Reqs.postOnlineStatusByFamilyId deviceData <$> familyId
     _ -> Nothing

getCloseRequest :: State -> Maybe HttpRequest
getCloseRequest state =
  let
    deviceId = (runAuthData state.authData).deviceId
    familyId = fst <$> head state.families -- Currently we just pick the first family
  in
   case state.userError of
     NoError -> flip runReader (mkSettings state.authData) <<< flip Reqs.deleteOnlineStatusByFamilyIdByDeviceId deviceId <$> familyId
     _ -> Nothing


-- | Retrieve AuthData from local storage or if not present get new one from server
getAuthData :: forall eff. Gonimo eff AuthData
getAuthData = do
  md <- liftEff $ localStorage.getItem Key.authData
  Gonimo.log $ "Got authdata from local storage: " <> gShow md
  case md of
    Nothing -> do
      auth <- postAccounts
      Gonimo.log $ "Got Nothing - called postAccounts and got: " <> gShow auth
      Gonimo.log $ "Calling setItem with : " <> gShow Key.authData
      liftEff $ localStorage.setItem Key.authData auth
      pure auth
    Just d  -> pure d

mkProps :: State -> Props
mkProps state = { settings : mkSettings state.authData }

mkSettings :: AuthData -> Settings
mkSettings (AuthData auth) = defaultSettings $ SPParams_ {
      authorization : auth.authToken
    , baseURL       : "http://localhost:8081/"
    }
