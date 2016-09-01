module Main where

import Prelude
import Gonimo.Client.Router
import Gonimo.UI.AcceptInvitation
import Gonimo.Client.Effects as Gonimo
import Gonimo.Client.Effects as Gonimo
import Gonimo.Client.LocalStorage as Key
import Gonimo.Client.Types as Gonimo
import Gonimo.UI.AcceptInvitation as AcceptC
import Gonimo.UI.Invite as InviteC
import Gonimo.UI.Loaded as LoadedC
import Pux.Html.Attributes as A
import Servant.Subscriber as Sub
import Servant.Subscriber.Connection as Sub
import Browser.LocalStorage (STORAGE, localStorage)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Bifunctor (bimap)
import Data.Either (Either(Right, Left))
import Data.Generic (gShow)
import Data.List (toUnfoldable, reverse, List(Nil, Cons))
import Data.Maybe (Maybe(..))
import Debug.Trace (trace)
import Gonimo.Client.Effects (handleError)
import Gonimo.Client.Types (GonimoEff, Gonimo, class ReportErrorAction)
import Gonimo.Client.Types (Error(URLRouteError), runGonimoT, Settings)
import Gonimo.Pux (onlyEffect, justEffect, onlyEffects, noEffects, EffModel(EffModel), toPux)
import Gonimo.Server.Types (AuthToken, AuthToken(GonimoSecret))
import Gonimo.Types (Secret(Secret))
import Gonimo.UI.Html (viewLogo)
import Gonimo.WebAPI (postFunnyName, SPParams_(SPParams_), postAccounts)
import Gonimo.WebAPI.Types (AuthData(AuthData))
import Partial.Unsafe (unsafeCrashWith)
import Pux (App, renderToDOM, fromSimple, start)
import Pux.Html (text, span, Html, img, div)
import Pux.Router (sampleUrl)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (defaultSettings, SPSettings_(SPSettings_))
import Servant.Subscriber (Subscriber, SubscriberEff)
import Signal (runSignal, constant, Signal)
import Signal.Channel (CHANNEL, Channel, send, subscribe, channel)
import Unsafe.Coerce (unsafeCoerce)

data State = LoadingS LoadingS'
           | LoadedS LoadedC.State

type LoadingS' = { subscriber :: Subscriber () LoadedC.Action
                 , actionQueue :: List LoadedC.Action } -- | We queue actions until we are loaded.

type LoadedState = {
               authData :: AuthData
             , settings :: Settings
             }

init :: Subscriber () LoadedC.Action -> State
init subscriber' = LoadingS { subscriber : subscriber' , actionQueue : Nil }

data Action = Start
            | Init LoadedC.State
            | ReportError Gonimo.Error
            | LoadedA LoadedC.Action
            | Nop

instance reportErrorActionAction :: ReportErrorAction Action where
  reportError = ReportError

--------------------------------------------------------------------------------


update :: forall eff. Action -> State -> EffModel eff State Action
update action (LoadingS state)          = updateLoading action state
update (LoadedA action) (LoadedS state) = updateLoaded action state
update (ReportError err) (LoadedS state)= updateLoaded (LoadedC.ReportError err) state
update _ state                          = onlyEffects state
                                            [do Gonimo.log "Only LoadedA actions are supported when loaded"
                                                pure Nop
                                            ]

updateLoading :: forall eff. Action -> LoadingS' -> EffModel eff State Action
updateLoading action state             = case action of
  Start             -> onlyEffect (LoadingS state) $ load state.subscriber
  (LoadedA action)  -> trace "Queuing action! " $ \_ -> noEffects $ LoadingS $ state {
                         actionQueue = Cons action state.actionQueue
                       }
  (Init ls)         -> EffModel {
                         state : LoadedS ls
                       , effects : map (pure <<< LoadedA)
                                    <<< toUnfoldable
                                    <<< reverse
                                    $ state.actionQueue
                       }
  (ReportError err) -> onlyEffect (LoadingS state) $ handleError Nop err
  Nop               -> noEffects $ LoadingS state


updateLoaded :: forall eff. LoadedC.Action -> LoadedC.State -> EffModel eff State Action
updateLoaded action state  = bimap LoadedS LoadedA $ LoadedC.update action state


fromRoute :: Route -> Action
fromRoute Home                      = Nop
fromRoute (AcceptInvitation secret) = trace ("Translating AcceptInvitation!") $
                                      \_ -> LoadedA $ LoadedC.HandleInvite secret
fromRoute (DecodingFailed err)      = ReportError $ URLRouteError err
--------------------------------------------------------------------------------

view :: State -> Html Action
view (LoadingS _)    = viewLoading
view (LoadedS state) = map LoadedA  $ LoadedC.view state

viewLoading :: Html Action
viewLoading = viewLogo $ div []
      [ span [] [ text "Loading your gonimo, stay tight ..."]
      ]

--------------------------------------------------------------------------------


load :: forall eff. Subscriber () LoadedC.Action -> Aff (GonimoEff eff) Action
load subscriber' = Gonimo.toAff initSettings $ authToAction =<< getAuthData
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
            { authData   : authData
            , settings   : mkSettings auth.authToken
            , subscriber : subscriber'
            , inviteS    : inviteState
            , acceptS    : AcceptC.init
            , central    : LoadedC.CentralInvite
            , families   : []
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


deploySubscriptions :: forall eff. State -> SubscriberEff eff Unit
deploySubscriptions (LoadingS _) = pure unit
deploySubscriptions (LoadedS s) = Sub.deploy (LoadedC.subscriptions s)
                                             (coerceSubscriberEffects s.subscriber)

main = do
  urlSignal <- sampleUrl
  controlChan <- channel Nop
  let controlSig = subscribe controlChan
  let routeSignal = map (fromRoute <<< match) urlSignal
  subscriber' <- Sub.makeSubscriber
                    { url      : "ws://localhost:8081/subscriber"
                    , callback : makeCallback controlChan
                    , notify   : makeNotify controlChan
                    }
  app <- start $
    { initialState: init (coerceSubscriberEffects subscriber')
    , update: toPux update
    , view: view
    , inputs: [controlSig, routeSignal]
    }
  -- We have to send Start after the fact, because on merging const signals one gets lost!
  send controlChan Start
  runSignal $ map deploySubscriptions app.state
  renderToDOM "#app" app.html

coerceEffects :: forall eff1 eff2 a. Eff eff1 a -> Eff eff2 a
coerceEffects = unsafeCoerce

toGonimoEffects :: forall eff a. Eff eff a -> Eff (GonimoEff eff) a
toGonimoEffects = unsafeCoerce

coerceSubscriberEffects :: forall eff1 eff2 a. Subscriber eff1 a -> Subscriber eff2 a
coerceSubscriberEffects = unsafeCoerce
