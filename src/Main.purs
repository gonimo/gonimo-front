module Main where

import Prelude
import Gonimo.Client.Effects as Gonimo
import Gonimo.Client.Init as Init
import Gonimo.Client.Loaded as LoadedC
import Gonimo.Client.Loading as LoadingC
import Gonimo.Client.LocalStorage as Key
import Gonimo.Client.Types as Client
import Pux.Html.Attributes as A
import Browser.LocalStorage (STORAGE, localStorage)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(..))
import Gonimo.Client.Types (runEffectsT, Settings)
import Gonimo.Client.Types (EffModel)
import Gonimo.Server.Types (AuthToken, AuthToken(GonimoSecret))
import Gonimo.Types (Secret(Secret))
import Gonimo.WebAPI (SPParams_(SPParams_), postAccounts)
import Gonimo.WebAPI.Types (AuthData(AuthData))
import Partial.Unsafe (unsafeCrashWith)
import Pux (noEffects, onlyEffects, renderToDOM, fromSimple, start)
import Pux.Html (text, span, Html, img, div)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (defaultSettings, SPSettings_(SPSettings_))
import Signal (constant, Signal)
import Signal.Channel (send, subscribe, channel)

data State = LoadingS LoadingC.State
           | LoadedS LoadedC.State

             
type LoadedState = {
               authData :: AuthData
             , settings :: Settings
             }


data Action = LoadingA LoadingC.Action
            | LoadedA  LoadedC.Action
             


update :: forall eff. Action -> State -> EffModel State Action eff
update (LoadingA (LoadingC.Init state)) (LoadingS _) = noEffects $ LoadedS state
update (LoadingA action) (LoadingS state) = bimapEffModel LoadingS LoadingA
                                                $ LoadingC.update action state
update (LoadedA action) (LoadedS state) = bimapEffModel LoadedS LoadedA
                                                $ LoadedC.update action state
update _ state = onlyEffects state [do Gonimo.log "Invalid action state combination!"
                                       unsafeCrashWith "WTF?!"
                                   ]


bimapEffModel :: forall s1 s2 a1 a2 eff. (s1 -> s2) -> (a1 -> a2) -> EffModel s1 a1 eff -> EffModel s2 a2 eff
bimapEffModel l r m = { state : l m.state
                      , effects : map (map r) m.effects
                      }

view :: State -> Html Action
view (LoadingS state) = map LoadingA $ LoadingC.view state
view (LoadedS state)  = map LoadedA  $ LoadedC.view state


initSig :: Signal Action
initSig = constant $ LoadingA LoadingC.Start

runEffects :: forall eff. LoadedState -> Array (Client.Effects eff Action)
              -> EffModel State Action (Client.EffEffects eff)
runEffects state = onlyEffects (LoadedS state) <<< map (runEffect state.settings)

runEffect :: forall eff. Settings -> Client.Effects eff Action
             -> Aff (Client.EffEffects eff) Action
runEffect settings m = do
    er <- runExceptT <<< flip runReaderT settings <<< runEffectsT $ m
    case er of
      Left err -> pure $ LoadedA $ LoadedC.ReportError err
      Right v -> pure v

main = do
  startApp <- channel $ LoadingA LoadingC.Start
  app <- start $
    { initialState: LoadingS LoadingC.Loading
    , update: update
    , view: view
    , inputs: []
    }
  renderToDOM "#app" app.html
