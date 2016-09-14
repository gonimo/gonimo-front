module Main where

import Control.Monad.Eff.Console as Console
import Data.Map as Map
import Gonimo.Client.Effects as Gonimo
import Gonimo.Client.Effects as Gonimo
import Gonimo.Client.LocalStorage as Key
import Gonimo.Client.Types as Gonimo
import Gonimo.UI.AcceptInvitation as AcceptC
import Gonimo.UI.Invite as InviteC
import Gonimo.UI.Loaded as LoadedC
import Gonimo.WebAPI.MakeRequests as Reqs
import Pux.Html.Attributes as A
import Servant.Subscriber as Sub
import Servant.Subscriber.Connection as Sub
import Servant.Subscriber.Subscriptions as Sub
import Browser.LocalStorage (STORAGE, localStorage)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (newRef, writeRef, readRef, REF, Ref)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Reader (runReader)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Array (head)
import Data.Bifunctor (bimap)
import Data.Either (Either(Right, Left))
import Data.Foldable (traverse_)
import Data.Generic (gShow)
import Data.List (toUnfoldable, reverse, List(Nil, Cons))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (fst, Tuple(Tuple))
import Debug.Trace (trace)
import Gonimo.Client.Effects (handleError)
import Gonimo.Client.Router (match, Route(AcceptInvitation, Home))
import Gonimo.Client.Types (GonimoEff, Gonimo, class ReportErrorAction)
import Gonimo.Client.Types (GonimoError(URLRouteError), runGonimoT, Settings)
import Gonimo.Pux (onlyEffect, justEffect, onlyEffects, noEffects, EffModel(EffModel), toPux)
import Gonimo.Server.Types (DeviceType(NoBaby), AuthToken, AuthToken(GonimoSecret))
import Gonimo.Types (Secret(Secret))
import Gonimo.UI.Html (viewLogo)
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

type LoadingS' = { actionQueue :: List LoadedC.Action } -- | We queue actions until we are loaded.

type LoadedState = {
               authData :: AuthData
             , settings :: Settings
             }

init :: State
init = LoadingS {  actionQueue : Nil }

data Action = Start
            | Init LoadedC.State
            | ReportError GonimoError
            | LoadedA LoadedC.Action
            | Nop

instance reportErrorActionAction :: ReportErrorAction Action where
  reportError = ReportError


type MySubscriber = Subscriber () LoadedC.Action
--------------------------------------------------------------------------------


update :: forall eff. Action -> State -> EffModel eff State Action
update action (LoadingS state)          = updateLoading action state
update (LoadedA action) (LoadedS state) = updateLoaded action state
update (ReportError err) (LoadedS state)= updateLoaded (LoadedC.ReportError err) state
update Nop state                        = noEffects state
update _ state                          = onlyEffects state
                                            [do Gonimo.log "Only LoadedA actions are supported when loaded"
                                                pure Nop
                                            ]

updateLoading :: forall eff. Action -> LoadingS' -> EffModel eff State Action
updateLoading action state             = case action of
  Start             -> onlyEffect (LoadingS state) load
  (LoadedA action)  -> trace "Queuing action! " $ \_ -> noEffects $ LoadingS $ state {
                         actionQueue = Cons action state.actionQueue
                       }
  (Init ls)         -> handleInit ls state
  (ReportError err) -> onlyEffect (LoadingS state) $ handleError Nop err
  Nop               -> noEffects $ LoadingS state


updateLoaded :: forall eff. LoadedC.Action -> LoadedC.State -> EffModel eff State Action
updateLoaded action state  = bimap LoadedS LoadedA $ LoadedC.update action state


fromRoute :: Route -> Action
fromRoute Home                      = trace ("We are going home! ") $ \_ -> Nop
fromRoute (AcceptInvitation secret) = trace ("Translating AcceptInvitation!") $
                                      \_ -> LoadedA $ LoadedC.HandleInvite secret

handleInit :: forall eff. LoadedC.State -> LoadingS' -> EffModel eff State Action
handleInit ls state = EffModel {
                         state : LoadedS ls
                       , effects : toUnfoldable queueEffects
                       }
  where
    queueEffects = map (pure <<< LoadedA) <<< reverse $ state.actionQueue


--------------------------------------------------------------------------------

view :: State -> Html Action
view (LoadingS _)    = viewLoading
view (LoadedS state) = map LoadedA  $ LoadedC.view state

viewLoading :: Html Action
viewLoading = viewLogo $ div []
      [ span [] [ text "Loading your gonimo, stay tight ..."]
      ]

--------------------------------------------------------------------------------


load :: forall eff. Aff (GonimoEff eff) Action
load = Gonimo.toAff initSettings $ authToAction =<< getAuthData
  where
    initSettings = mkSettings $ GonimoSecret (Secret "blabala")

    mkSettings :: AuthToken -> Settings
    mkSettings secret = defaultSettings $ SPParams_ {
          authorization : secret
        , baseURL       : "http://localhost:8081/"
        }

    authToAction :: AuthData -> Gonimo eff Action
    authToAction (authData@(AuthData auth)) = do
      inviteState <- InviteC.init
      pure $ Init
            { authData      : authData
            , settings      : mkSettings auth.authToken
            , subscriberUrl : "ws://localhost:8081/subscriber"
            , inviteS       : inviteState
            , acceptS       : AcceptC.init
            , central       : LoadedC.CentralInvite
            , families      : []
            , onlineDevices : Map.empty
            , deviceInfos   : Map.empty
            , userError     : LoadedC.NoError
            }

makeCallback :: forall eff. Channel Action ->  (LoadedC.Action -> SubscriberEff (channel :: CHANNEL | eff) Unit)
makeCallback c = send c <<< LoadedA

makeNotify :: forall eff. Channel Action ->  (Sub.Notification -> SubscriberEff (channel :: CHANNEL | eff) Unit)
makeNotify c = send c <<< LoadedA <<< LoadedC.HandleSubscriber

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
  let routeSignal = map (fromRoute <<< match) urlSignal
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

coerceEffects :: forall eff1 eff2 a. Eff eff1 a -> Eff eff2 a
coerceEffects = unsafeCoerce

toGonimoEffects :: forall eff a. Eff eff a -> Eff (GonimoEff eff) a
toGonimoEffects = unsafeCoerce

coerceSubscriberEffects :: forall eff1 eff2 a. Subscriber eff1 a -> Subscriber eff2 a
coerceSubscriberEffects = unsafeCoerce

showCentral :: State -> String
showCentral (LoadingS _) = ""
showCentral (LoadedS s) = gShow s.central
