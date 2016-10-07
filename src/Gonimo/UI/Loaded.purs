-- | Loaded application ui logic
module Gonimo.UI.Loaded where

import Gonimo.UI.Html
import Data.Array as Arr
import Data.List as List
import Data.Map as Map
import Data.Tuple as Tuple
import Gonimo.Client.Effects as Gonimo
import Gonimo.Client.LocalStorage as Key
import Gonimo.Client.LocalStorage as Key
import Gonimo.Client.Router as Router
import Gonimo.Client.Types as Gonimo
import Gonimo.UI.AcceptInvitation as AcceptC
import Gonimo.UI.Error as Error
import Gonimo.UI.Home as HomeC
import Gonimo.UI.Invite as InviteC
import Gonimo.WebAPI.MakeRequests as Reqs
import Gonimo.WebAPI.Subscriber as Sub
import Pux.Html as H
import Pux.Html.Attributes as A
import Pux.Html.Attributes.Bootstrap as A
import Pux.Html.Events as E
import Servant.Subscriber as Sub
import Servant.Subscriber.Connection as Sub
import Browser.LocalStorage (STORAGE, localStorage)
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Reader (runReader)
import Control.Monad.Reader.Class (class MonadReader)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.State.Class (get, modify, put)
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
import Data.Tuple (uncurry, fst, Tuple(Tuple))
import Debug.Trace (trace)
import Gonimo.Client.Types (toIO, Settings, GonimoError, class ReportErrorAction, Gonimo, GonimoEff, runGonimoT)
import Gonimo.Pux (Component(Component), toParent, runGonimo, liftChild, ComponentType, makeChildData, ToChild, noEffects, onlyModify, Update, wrapAction)
import Gonimo.Server.DbEntities (Device(Device), Family(Family))
import Gonimo.Server.DbEntities.Helpers (runFamily)
import Gonimo.Server.Error (ServerError(InvalidAuthToken))
import Gonimo.Server.Types (DeviceType(Baby, NoBaby), AuthToken, AuthToken(GonimoSecret))
import Gonimo.Types (dateToString, Key(Key), Secret(Secret))
import Gonimo.UI.Error (viewError, class ErrorAction, UserError(NoError, DeviceInvalid), handleSubscriber, handleError)
import Gonimo.UI.Loaded.Types (homeS, centralHome, Props, acceptS, inviteS, State, Action(..), Central(..))
import Gonimo.Util (toString, fromString)
import Gonimo.WebAPI (deleteOnlineStatusByFamilyIdByDeviceId, SPParams_(SPParams_), postAccounts)
import Gonimo.WebAPI.Types (DeviceInfo(DeviceInfo), AuthData(AuthData))
import Gonimo.WebAPI.Types.Helpers (runAuthData)
import Partial.Unsafe (unsafeCrashWith)
import Pux (renderToDOM, fromSimple, start)
import Pux.Html (text, small, script, li, a, nav, h3, h2, td, tbody, th, tr, thead, table, ul, p, button, input, h1, span, Html, img, div)
import Pux.Html.Attributes (letterSpacing, offset)
import Pux.Html.Events (FormEvent, FocusEvent)
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


update :: Update Unit State Action
update (SetState state)                    = put state *> pure []
update (ReportError err)                   = handleError err
update (InviteA (InviteC.ReportError err)) = handleError err
update (InviteA action)                    = updateInvite action
update (AcceptA (AcceptC.ReportError err)) = handleError err
update (AcceptA action)                    = updateAccept action
update (HomeA action)                      = updateHome action
update (SetFamilyIds ids)                  = onlyModify
                                             $ \state -> state { familyIds = ids
                                                               , currentFamily = state.currentFamily <|> head ids
                                                               }
update (ServerFamilyGoOffline familyId)    = handleServerFamilyGoOffline familyId
update (UpdateFamily familyId' family')    = onlyModify $ \state ->  state { families = Map.insert familyId' family' state.families }
update (SwitchFamily familyId')            = do
  oldFamily <- _.currentFamily <$> (get :: Component Unit State State)
  modify $ _ { currentFamily = Just familyId' }
  pure
    $ fromFoldable
    $ map (pure <<< ServerFamilyGoOffline) oldFamily
update (SetCentral c)                      = onlyModify $ _ { central = c }
update (SetOnlineDevices devices)          = onlyModify $ _ { onlineDevices = devices }
update (SetDeviceInfos devices)            = onlyModify $ _ { deviceInfos = devices }
update (SetURL url)                        = handleSetURL url
update (HandleSubscriber msg)              = handleSubscriber msg
update ResetDevice                         = handleResetDevice
update ClearError                          = handleClearError
update (SetAuthData auth)                  = handleSetAuthData auth
update Nop                                 = noEffects
update _                                   = noEffects


toInvite :: ToChild Unit State Props InviteC.State
toInvite = do
  props <- mkProps <$> get
  pure $ makeChildData inviteS props

toAccept :: ToChild Unit State Props AcceptC.State
toAccept = do
  props <- mkProps <$> get
  pure $ makeChildData acceptS props

toHome :: ToChild Unit State Props HomeC.State
toHome = do
  props <- mkProps <$> get
  pure $ makeChildData homeS props

updateInvite :: InviteC.Action -> ComponentType Unit State Action
updateInvite = toParent Nop InviteA <<< liftChild toInvite <<< InviteC.update

updateAccept :: AcceptC.Action -> ComponentType Unit State Action
updateAccept = toParent Nop AcceptA <<< liftChild toAccept <<< AcceptC.update

updateHome :: HomeC.Action -> ComponentType Unit State Action
updateHome action = case action of
    HomeC.StartBabyStation baby -> handleStartBabyStation baby
    HomeC.StopBabyStation       -> handleStopBabyStation
    HomeC.ConnectToBaby baby    -> unsafeCrashWith "ConnectToBaby: Not yet implemented!"
    _                           -> toParent Nop HomeA <<< liftChild toHome $ HomeC.update action
  where
    handleStartBabyStation :: String -> ComponentType Unit State Action
    handleStartBabyStation name = onlyModify $ _ { onlineStatus = Baby name }

    handleStopBabyStation :: ComponentType Unit State Action
    handleStopBabyStation = onlyModify $ _ { onlineStatus = NoBaby }

inviteEffect :: forall m. Monad m => Secret -> m Action
inviteEffect = pure <<< AcceptA <<< AcceptC.LoadInvitation

handleResetDevice :: ComponentType Unit State Action
handleResetDevice = do
  props <- mkProps <$> get
  pure $ [ toIO props.settings $ do
              liftEff $ localStorage.removeItem Key.authData
              SetAuthData <$> getAuthData
         ]

handleSetAuthData :: AuthData -> ComponentType Unit State Action
handleSetAuthData auth = onlyModify $ \state -> state
                                  { authData = auth
                                  , userError = case state.userError of
                                                  DeviceInvalid -> NoError
                                                  _ -> state.userError
                                  }

handleSetURL :: String -> ComponentType Unit State Action
handleSetURL url = do
  let
    route = Router.match url
    withoutQuery = takeWhile (_ /= '?') url
  case route of
    Router.Home -> onlyModify $ _ { url = url }
    Router.AcceptInvitation s -> do
      modify $ _ { url = withoutQuery
                 , central = CentralAccept
                 }
      pure [ do
              liftEff $ navigateTo withoutQuery
              inviteEffect s
            ]

handleClearError :: ComponentType Unit State Action
handleClearError = do
  state <- (get :: Component Unit State State)
  let
    newCentral = case state.central of
      CentralAccept -> case state.acceptS of
        Nothing  -> centralHome
        Just _ -> state.central
      _ -> state.central

  onlyModify $ _ { userError = NoError
                 , central = newCentral
                 }

handleServerFamilyGoOffline :: Key Family -> ComponentType Unit State Action
handleServerFamilyGoOffline familyId = do
    state <- get
    let
      deviceId = (runAuthData state.authData).deviceId
      props = mkProps state
    pure $ [ Gonimo.toIO props.settings $ do
                deleteOnlineStatusByFamilyIdByDeviceId familyId deviceId
                pure Nop
           ]

--------------------------------------------------------------------------------

view :: State -> Html Action
view state =
  let
    numDevices = Arr.length state.onlineDevices
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
                [ viewFamilyChooser state
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
viewCentral state =
  let
    props = mkProps state
  in
   case state.central of
    CentralInvite -> map InviteA $ InviteC.view props state.inviteS
    CentralAccept -> map AcceptA $ AcceptC.view state.acceptS
    CentralHome   -> map HomeA   $ HomeC.view props state.homeS


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
           <<< Arr.mapMaybe viewOnlineDevice
           $ map fst state.onlineDevices
    viewOnlineDevice deviceId = do
        (DeviceInfo info) <- Tuple.lookup deviceId state.deviceInfos
        let name = info.deviceInfoName
        let lastAccessed = dateToString info.deviceInfoLastAccessed
        pure $ tr []
               [ td [] [ text name ]
               , td [] [ text lastAccessed ]
               ]

viewFamilyChooser :: State -> Html Action
viewFamilyChooser state = H.div []
                          [ H.label [ A.htmlFor "familySelect" ] [ text "Family: " ]
                          , H.select [ A.id_ "familySelect"
                                     , A.className "form-control"
                                     , E.onInput doSwitchFamily
                                     ]
                            $ fromFoldable
                            <<< map (uncurry makeOption)
                            <<< Map.toList $ state.families
                          ]
  where
    doSwitchFamily :: FormEvent -> Action
    doSwitchFamily ev = maybe Nop SwitchFamily $ fromString ev.target.value

    makeOption :: Key Family -> Family -> Html Action
    makeOption familyId (Family family) = H.option [ A.value (toString familyId) ]
                                          [ text family.familyName ]
--------------------------------------------------------------------------------
getSubscriptions :: State -> Subscriptions Action
getSubscriptions state =
  let
    familyId = state.currentFamily

    --subscribeGetFamily :: forall m. MonadReader Settings m => Key Family -> m (Subscriptions Action)
    subscribeGetFamily familyId' =
      Sub.getFamiliesByFamilyId (maybe Nop (UpdateFamily familyId')) familyId'

    subArray :: Array (Subscriptions Action)
    subArray = map (flip runReader (mkSettings state.authData)) <<< concat $
      [ [ Sub.getAccountsByAccountIdFamilies (maybe Nop SetFamilyIds)
                                          (runAuthData state.authData).accountId
        ]
      , fromFoldable $
        Sub.getOnlineStatusByFamilyId (maybe Nop SetOnlineDevices) <$> familyId
      , fromFoldable $
        Sub.getFamiliesByFamilyIdDeviceInfos (maybe Nop SetDeviceInfos) <$> familyId
      , map subscribeGetFamily state.familyIds
      ]
  in
   case state.userError of
     NoError -> foldl append mempty subArray
     _       -> mempty

getPongRequest :: State -> Maybe HttpRequest
getPongRequest state =
  let
    deviceId = (runAuthData state.authData).deviceId
    deviceData = Tuple deviceId state.onlineStatus
    familyId = state.currentFamily
  in
   case state.userError of
     NoError -> flip runReader (mkSettings state.authData) <<< Reqs.postOnlineStatusByFamilyId deviceData <$> familyId
     _ -> Nothing

getCloseRequest :: State -> Maybe HttpRequest
getCloseRequest state =
  let
    deviceId = (runAuthData state.authData).deviceId
    familyId = state.currentFamily
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
mkProps state = { settings : mkSettings state.authData
                , deviceId : (runAuthData state.authData).deviceId
                , familyId : state.currentFamily
                , onlineStatus  : state.onlineStatus
                , family : flip Map.lookup state.families =<< state.currentFamily
                , onlineDevices : state.onlineDevices
                , deviceInfos : state.deviceInfos
                }

mkSettings :: AuthData -> Settings
mkSettings (AuthData auth) = defaultSettings $ SPParams_ {
      authorization : auth.authToken
    , baseURL       : "http://localhost:8081/"
    }
