module Main where

import Prelude
import Gonimo.Client.Types as Client
import Gonimo.Client.Types as Client
import Gonimo.Client.Types as Client
import Gonimo.LocalStorage as Key
import Pux.Html.Attributes as A
import Browser.LocalStorage (STORAGE, localStorage)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(..))
import Gonimo.Client.Types (runEffectsT, Settings)
import Gonimo.Server.Types (AuthToken, AuthToken(GonimoSecret))
import Gonimo.Types (Secret(Secret))
import Gonimo.WebAPI (SPParams_(SPParams_), postAccounts)
import Gonimo.WebAPI.Types (AuthData(AuthData))
import Partial.Unsafe (unsafeCrashWith)
import Pux (noEffects, onlyEffects, EffModel, renderToDOM, fromSimple, start)
import Pux.Html (text, span, Html, img, div)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (defaultSettings, SPSettings_(SPSettings_))
import Signal (constant, Signal)

data State = Loading Loading.State
           | Loaded Loaded.State

             
type LoadedState = {
               authData :: AuthData
             , settings :: Settings
             }


data Action = Loading Loading.Action
            | Loaded  Loaded.Action
             


update :: forall eff. Action -> State -> EffModel State Action (Client.Effects eff)
update (Loading Init.start) Loading = bimapEffModel Loading Loading
update _ _ = unsafeCrashWith "Fix me!"
-- update action (Loaded state) = updateLoaded action state



bimapEffModel :: forall eff. (s1 -> s2) -> (a1 -> a2) -> EffModel s1 a1 eff -> EffModel s2 a2 eff
bimapEffModel l r m = { state : l m.state
                      , effects : map (map r) m
                      }

view :: State -> Html Action
view Loading    = viewLogo Loading $ span [] [ text "Loading your gonimo, stay tight ..."]
view (Loaded ls) = viewLogo (Loaded ls)  $ span [] [ text "We did it - Ready to rumble!"]
view state      = viewLogo state   $ span [] [ text "WTF?" ]


initSig :: Signal Action
initSig = constant Start

runEffects :: forall eff. LoadedState -> Array (Client.Effects eff Action)
              -> EffModel State Action (Client.EffEffects eff)
runEffects state = onlyEffects Loading <<< map (runEffect state.settings) 

runEffect :: forall eff. Settings -> Client.Effects eff Action
             -> Aff (Client.EffEffects eff) Action
runEffect settings m = do
    er <- runExceptT <<< flip runReaderT settings <<< runEffectsT $ m
    case er of
      Left err -> pure $ ReportError err
      Right v -> pure v

main = do
  app <- start $
    { initialState: Loading
    , update: update
    , view: view
    , inputs: [initSig]
    }

  renderToDOM "#app" app.html
