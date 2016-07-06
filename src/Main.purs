module Main where

import Prelude
import Gonimo.Client.Router
import Gonimo.Client.Effects as Gonimo
import Gonimo.Client.Effects as Gonimo
import Gonimo.Client.LocalStorage as Key
import Gonimo.Client.Types as Gonimo
import Gonimo.Client.Types (GonimoEff, Gonimo)
import Gonimo.UI.Invite as InviteC
import Gonimo.UI.Loaded as LoadedC
import Pux.Html.Attributes as A
import Browser.LocalStorage (STORAGE, localStorage)
import Control.Monad.Aff (Aff)
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
import Gonimo.Client.Types (Error(URLRouteError), runGonimoT, Settings)
import Gonimo.Pux (onlyEffect, justEffect, onlyEffects, noEffects, EffModel(EffModel), toPux)
import Gonimo.Server.Types (AuthToken, AuthToken(GonimoSecret))
import Gonimo.Types (Secret(Secret))
import Gonimo.UI.Html (viewLogo)
import Gonimo.WebAPI (SPParams_(SPParams_), postAccounts)
import Gonimo.WebAPI.Types (AuthData(AuthData))
import Partial.Unsafe (unsafeCrashWith)
import Pux (renderToDOM, fromSimple, start)
import Pux.Html (text, span, Html, img, div)
import Pux.Router (sampleUrl)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (defaultSettings, SPSettings_(SPSettings_))
import Signal (constant, Signal)
import Signal.Channel (send, subscribe, channel)

data State = LoadingS LoadingS'
           | LoadedS LoadedC.State

type LoadingS' = { actionQueue :: List LoadedC.Action } -- | We queue actions until we are loaded.

type LoadedState = {
               authData :: AuthData
             , settings :: Settings
             }


init :: State
init = LoadingS { actionQueue : Nil }

data Action = Start
            | Init LoadedC.State
            | ReportError Gonimo.Error
            | Nop
            | LoadedA LoadedC.Action

--------------------------------------------------------------------------------
update :: forall eff. Action -> State -> EffModel eff State Action
update action (LoadingS state)          = updateLoading action state
update (LoadedA action) (LoadedS state) = updateLoaded action state
update (ReportError err) (LoadedS state)= updateLoaded (LoadedC.ReportError err) state
update _ state                          =
  onlyEffects state [do Gonimo.log "Only LoadedA actions are supported when loaded"
                        pure Nop
                    ]

updateLoading :: forall eff. Action -> LoadingS' -> EffModel eff State Action
updateLoading Start state             = onlyEffect (LoadingS state) load
updateLoading (LoadedA action) state  = trace "Queuing action! " $ \_ -> noEffects $ LoadingS $ state {
    actionQueue = Cons action state.actionQueue
  }
updateLoading (Init ls) state         = EffModel {
    state : LoadedS ls
  , effects : map (pure <<< LoadedA) <<< toUnfoldable <<< reverse $ state.actionQueue
  }
updateLoading (ReportError err) state = onlyEffect (LoadingS state) $ handleError Nop err
updateLoading Nop state               = noEffects $ LoadingS state


updateLoaded :: forall eff. LoadedC.Action -> LoadedC.State -> EffModel eff State Action
updateLoaded action state  = bimap LoadedS LoadedA $ LoadedC.update action state


fromRoute :: Route -> Action
fromRoute Home = Nop
fromRoute (AcceptInvitation secret) = trace ("Translating AcceptInvitation!") $ \_ -> LoadedA $ LoadedC.HandleInvite secret
fromRoute (DecodingFailed err) = ReportError $ URLRouteError err
--------------------------------------------------------------------------------

view :: State -> Html Action
view (LoadingS _) = viewLoading
view (LoadedS state)  = map LoadedA  $ LoadedC.view state

viewLoading :: Html Action
viewLoading = viewLogo $ div []
      [ span [] [ text "Loading your gonimo, stay tight ..."]
      ]

--------------------------------------------------------------------------------


load :: forall eff. Aff (GonimoEff eff) Action
load = do
  let
    mkSettings :: AuthToken -> Settings
    mkSettings secret = defaultSettings $ SPParams_ {
          authorization : secret
        , baseURL : "http://localhost:8081/"
        }
    initSettings = mkSettings $ GonimoSecret (Secret "blabala")
  r <- runExceptT <<< flip runReaderT initSettings <<< runGonimoT $ getAuthData 
  case r of
    Left err -> pure $ ReportError err
    Right (authData@(AuthData auth)) -> pure $ Init { authData : authData
                                                    , settings : mkSettings auth.authToken
                                                    , inviteS : InviteC.init
                                                    }


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


main = do
  urlSignal <- sampleUrl
  startUp <- channel Nop
  let startUpSig = subscribe startUp
  let routeSignal = map (fromRoute <<< match) urlSignal
  app <- start $
    { initialState: init
    , update: toPux update
    , view: view
    , inputs: [startUpSig, routeSignal]
    }
  -- We have to send Start after the fact, because on merging const signals one gets lost!
  send startUp Start
  renderToDOM "#app" app.html
