module Main where

import Prelude
import Gonimo.Client.Effects as Gonimo
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
import Data.Bifunctor (bimap)
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(..))
import Gonimo.Client.Types (runEffectsT, Settings)
import Gonimo.Pux (onlyEffects, noEffects, EffModel(EffModel), toPux)
import Gonimo.Server.Types (AuthToken, AuthToken(GonimoSecret))
import Gonimo.Types (Secret(Secret))
import Gonimo.WebAPI (SPParams_(SPParams_), postAccounts)
import Gonimo.WebAPI.Types (AuthData(AuthData))
import Partial.Unsafe (unsafeCrashWith)
import Pux (renderToDOM, fromSimple, start)
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

--------------------------------------------------------------------------------

update :: forall eff. Action -> State -> EffModel eff State Action
update (LoadingA (LoadingC.Init state)) (LoadingS _) = noEffects $ LoadedS state
update (LoadingA action) (LoadingS state)            = updateLoading action state
update (LoadedA action) (LoadedS state)              = updateLoaded action state
update _ state                                       =
  onlyEffects state [do Gonimo.log "Invalid action state combination!"
                        unsafeCrashWith "WTF?!"
                    ]

updateLoading :: forall eff. LoadingC.Action -> LoadingC.State -> EffModel eff State Action
updateLoading action state = bimap LoadingS LoadingA $ LoadingC.update action state

updateLoaded :: forall eff. LoadedC.Action -> LoadedC.State -> EffModel eff State Action
updateLoaded action state  = bimap LoadedS LoadedA   $ LoadedC.update action state

--------------------------------------------------------------------------------

view :: State -> Html Action
view (LoadingS state) = map LoadingA $ LoadingC.view state
view (LoadedS state)  = map LoadedA  $ LoadedC.view state

--------------------------------------------------------------------------------

initSig :: Signal Action
initSig = constant $ LoadingA LoadingC.Start


main = do
  app <- start $
    { initialState: LoadingS LoadingC.Loading
    , update: toPux update
    , view: view
    , inputs: [initSig]
    }
  renderToDOM "#app" app.html
