module Main where

import Control.Monad.Eff.Console as Console
import Data.Map as Map
import Gonimo.Client.Effects as Gonimo
import Gonimo.Client.Effects as Gonimo
import Gonimo.Client.LocalStorage as Key
import Gonimo.Client.Types as Gonimo
import Gonimo.UI.AcceptInvitation as AcceptC
import Gonimo.UI.Home as HomeC
import Gonimo.UI.Invite as InviteC
import Gonimo.UI.Loaded as LoadedC
import Gonimo.UI.Loaded.Types as LoadedC
import Gonimo.WebAPI.MakeRequests as Reqs
import Pux.Html.Attributes as A
import Servant.Subscriber as Sub
import Servant.Subscriber.Connection as Sub
import Servant.Subscriber.Subscriptions as Sub
import Browser.LocalStorage (STORAGE, localStorage)
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (newRef, writeRef, readRef, REF, Ref)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.IO (IO)
import Control.Monad.Reader (runReader)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.State.Class (modify, put, get)
import Data.Array (head)
import Data.Bifunctor (lmap, bimap)
import Data.Either (Either(Right, Left))
import Data.Foldable (traverse_)
import Data.Generic (gShow)
import Data.Lens (prism', PrismP, (^?))
import Data.List (toUnfoldable, reverse, List(Nil, Cons))
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (fst, Tuple(Tuple))
import Debug.Trace (trace)
import Gonimo.Client.Router (match, Route(AcceptInvitation, Home))
import Gonimo.Client.Types (toIO, GonimoError, GonimoEff, Gonimo, class ReportErrorAction)
import Gonimo.Client.Types (runGonimoT, Settings)
import Gonimo.Pux (toPux, noEffects, wrapAction, onlyModify, Update, ComponentType, makeChildData, ToChild, liftChild, Component)
import Gonimo.Server.Types (DeviceType(NoBaby), AuthToken, AuthToken(GonimoSecret))
import Gonimo.Types (Secret(Secret))
import Gonimo.UI.Error (viewError, handleError, class ErrorAction, UserError(NoError))
import Gonimo.UI.Html (viewLogo)
import Gonimo.Util (fromMaybeM, coerceEffects)
import Gonimo.WebAPI (postFunnyName, SPParams_(SPParams_), postAccounts)
import Gonimo.WebAPI.Types (AuthData(AuthData))
import Gonimo.WebAPI.Types.Helpers (runAuthData)
import Partial.Unsafe (unsafeCrashWith)
import Pux (App, renderToDOM, fromSimple, start)
import Pux.Html (text, span, Html, img, div)
import Pux.Router (sampleUrl)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (defaultSettings, SPSettings_(SPSettings_))
import Servant.Subscriber (Subscriber, SubscriberEff)
import Servant.Subscriber.Connection (setPongRequest)
import Signal (foldp, map2, runSignal, constant, Signal)
import Signal.Channel (CHANNEL, Channel, send, subscribe, channel)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (div)

data State = LoadingS LoadingS'
           | LoadedS LoadedC.State


_LoadingS :: PrismP State LoadingS'
_LoadingS = prism' LoadingS (\s -> case s of
                                LoadingS x -> Just x
                                _ -> Nothing)

_LoadedS :: PrismP State LoadedC.State
_LoadedS = prism' LoadedS (\s -> case s of
                                LoadedS x -> Just x
                                _ -> Nothing)

type LoadingS' = { actionQueue :: List LoadedC.Action -- | We queue actions until we are loaded.
                 , userError :: UserError
                 }

type LoadedState = {
               authData :: AuthData
             , settings :: Settings
             }

init :: State
init = LoadingS { actionQueue : Nil
                , userError : NoError
                }

data Action = Start
            | Init LoadedC.State
            | ReportError GonimoError
            | LoadedA LoadedC.Action
            | ResetDevice
            | ClearError
            | Nop

instance reportErrorActionAction :: ReportErrorAction Action where
  reportError = ReportError

instance errorActionAction :: ErrorAction Action where
  resetDevice = ResetDevice
  nop = Nop
  clearError = ClearError

type MySubscriber = Subscriber () LoadedC.Action
--------------------------------------------------------------------------------


update :: Update Unit State Action
update action = do
  state <- ( get :: Component Unit State State)
  case action of
    Nop -> noEffects
    Init ls -> fromMaybe [] <$> handleInit ls
    _ -> fromMaybeM (fromMaybe [] <$> updateLoaded action) (updateLoading action)


toLoading :: ToChild Unit State Unit LoadingS'
toLoading = pure $ makeChildData _LoadingS unit

toLoaded :: ToChild Unit State Unit LoadedC.State
toLoaded = pure $ makeChildData _LoadedS unit

updateLoading' ::  Update Unit LoadingS' Action
updateLoading' action = case action of
  Start           -> pure [ load ]
  LoadedA action  -> onlyModify $ \state -> state {
                         actionQueue = Cons action state.actionQueue
                       }
  Init ls         -> pure [ do
                                 Gonimo.log "Init command should be handled in toplevel update!"
                                 pure Nop
                            ]
  ReportError err -> handleError err
  ResetDevice     -> handleResetDevice
  ClearError      -> do
    modify $ _ { userError = NoError }
    pure $ wrapAction Start
  Nop             -> noEffects

handleResetDevice :: ComponentType Unit LoadingS' Action
handleResetDevice = pure $ [ do
  liftEff $ localStorage.removeItem Key.authData
  pure Start
 ]
updateLoading :: Action -> Component Unit State (Maybe (Array (IO Action)))
updateLoading = liftChild toLoading <<< updateLoading'

updateLoaded :: Action -> Component Unit State (Maybe (Array (IO Action)))
updateLoaded action = case action of
  LoadedA act -> updateLoaded' act
  (ReportError err) -> updateLoaded' (LoadedC.ReportError err)
  _ -> do
       state <- get
       case state of
        LoadedS _ -> pure <<< Just $
                     [ do Gonimo.log "Only LoadedA actions are supported when loaded"
                          pure Nop
                     ]
        _ -> pure Nothing

updateLoaded' :: LoadedC.Action -> Component Unit State (Maybe (Array (IO Action)))
updateLoaded' = map (map (map (map LoadedA))) <<< liftChild toLoaded <<< LoadedC.update


handleInit :: LoadedC.State -> Component Unit State (Maybe (Array (IO Action)))
handleInit ls = do
    state <- get
    case state ^? _LoadingS of
      Nothing -> pure Nothing
      Just loadingS -> do
        let queueEffects = map (pure <<< LoadedA) <<< reverse $ loadingS.actionQueue
        put $ LoadedS ls
        pure <<< Just $ toUnfoldable queueEffects


--------------------------------------------------------------------------------

view :: State -> Html Action
view (LoadingS state) = viewLoading state
view (LoadedS state)  = map LoadedA  $ LoadedC.view state

viewLoading :: LoadingS' -> Html Action
viewLoading state = case state.userError of
      NoError -> viewLogo $ div []
                            [ span [] [ text "Loading your gonimo, stay tight ..."]
                            ]
      _ -> viewError state

--------------------------------------------------------------------------------


load :: IO Action
load = Gonimo.toIO initSettings $ authToAction =<< LoadedC.getAuthData
  where
    initSettings = mkSettings $ GonimoSecret (Secret "blabala")

    mkSettings :: AuthToken -> Settings
    mkSettings secret = defaultSettings $ SPParams_ {
          authorization : secret
        , baseURL       : "http://localhost:8081/"
        }

    authToAction :: forall eff. AuthData -> Gonimo eff Action
    authToAction (authData@(AuthData auth)) = do
      inviteState <- InviteC.init
      pure $ Init
            { authData      : authData
            , subscriberUrl : "ws://localhost:8081/subscriber"
            , inviteS       : inviteState
            , acceptS       : AcceptC.init
            , homeS         : HomeC.init
            , central       : LoadedC.CentralInvite
            , familyIds      : []
            , currentFamily  : Nothing
            , families      : Map.empty
            , url           : ""
            , onlineDevices : []
            , deviceInfos   : []
            , userError     : NoError
            , onlineStatus   : NoBaby
            }

makeCallback :: forall eff. Channel Action ->  (LoadedC.Action -> SubscriberEff (channel :: CHANNEL | eff) Unit)
makeCallback c = send c <<< LoadedA

makeNotify :: forall eff. Channel Action ->  (Sub.Notification -> SubscriberEff (channel :: CHANNEL | eff) Unit)
makeNotify c = send c <<< LoadedA <<< LoadedC.HandleSubscriber


deploySubscriptions :: forall eff. SubscriberEff eff (Maybe MySubscriber) -> State -> SubscriberEff eff Unit
deploySubscriptions _ (LoadingS _) = pure unit
deploySubscriptions getSubscriber (LoadedS s) = do
  let subscriptions = LoadedC.getSubscriptions s
  subscriber <- getSubscriber
  case subscriber of
    Nothing -> coerceEffects $ Console.log "Not deploying - no subscriber yet!"
    Just subscriber' -> do
      coerceEffects $ Console.log "Setting pong and close requests ..."
      let mySubscriber = coerceSubscriberEffects subscriber'
      traverse_ (flip Sub.setPongRequest  (Sub.getConnection mySubscriber)) $ LoadedC.getPongRequest s
      traverse_ (flip Sub.setCloseRequest (Sub.getConnection mySubscriber)) $ LoadedC.getCloseRequest s
      coerceEffects $ Console.log $ "Deploying " <> show (Sub.size subscriptions) <> " subscriptions!"
      Sub.deploy subscriptions mySubscriber
      coerceEffects $ Console.log $ "Deployed " <> show (Sub.size subscriptions) <> " subscriptions!"

-- | Don't use a foldp instead of Ref - you will build up an endless thunk of actions in memory.
renewSubscriber :: forall eff. Channel Action -> Ref (Maybe MySubscriber)
                   -> State -> SubscriberEff (channel :: CHANNEL | eff) (Maybe MySubscriber)
renewSubscriber controlChan subscriberRef state = do
    oldSub <- readRef subscriberRef
    let oldUrl = Sub.getUrl <<< Sub.getConnection <$> oldSub
    let newUrl = case state of
          LoadedS lState -> Just lState.subscriberUrl
          _ -> Nothing
    coerceEffects $ Console.log $ "Old url: " <> show oldUrl
    coerceEffects $ Console.log $ "New url: " <> show newUrl
    coerceEffects $ Console.log $ "Is different: " <> show (newUrl /= oldUrl)
    if (newUrl /= oldUrl)
      then do
        coerceEffects $ Console.log $ "Closing old subscriber ..."
        coerceEffects $ traverse_ (Sub.close <<< Sub.getConnection) oldSub
        newSub <- traverse makeMySubscriber newUrl
        writeRef subscriberRef newSub
        pure newSub
       else
        pure oldSub
  where
    makeMySubscriber :: String -> SubscriberEff (channel :: CHANNEL | eff) MySubscriber
    makeMySubscriber url' = do
        sub <- Sub.makeSubscriber
                  { url : url'
                  , callback : makeCallback controlChan
                  , notify : makeNotify controlChan
                  }
        pure $ coerceSubscriberEffects sub

main = do
  urlSignal <- sampleUrl
  controlChan <- channel Nop
  subscriberRef <- newRef Nothing
  let controlSig = subscribe controlChan
  let routeSignal = map (LoadedA <<< LoadedC.SetURL) urlSignal
  app <- start $
    { initialState: init
    , update: toPux update
    , view: view
    , inputs: [controlSig, routeSignal]
    }
  -- We have to send Start after the fact, because on merging const signals one gets lost!
  send controlChan Start
  let subscriberSignal = map (renewSubscriber controlChan subscriberRef) app.state
  let subscribeSignal = deploySubscriptions <$> subscriberSignal <*> app.state
  runSignal $ subscribeSignal
  runSignal $ map (\_ -> Console.log "State changed!") app.state
  runSignal $ Console.log <$> urlSignal
  runSignal $ (Console.log <<< ("Central widget: " <> _ ) <<< showCentral) <$> app.state
  renderToDOM "#app" app.html

toGonimoEffects :: forall eff a. Eff eff a -> Eff (GonimoEff eff) a
toGonimoEffects = unsafeCoerce

coerceSubscriberEffects :: forall eff1 eff2 a. Subscriber eff1 a -> Subscriber eff2 a
coerceSubscriberEffects = unsafeCoerce

showCentral :: State -> String
showCentral (LoadingS _) = ""
showCentral (LoadedS s) = gShow s.central
