-- | Loaded application ui logic
module Gonimo.Client.Loaded where

import Prelude
import Gonimo.Client.Html
import Gonimo.Client.Effects as Gonimo
import Gonimo.Client.Loading as Loading
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
import Gonimo.Client.Effects (handleError)
import Gonimo.Client.Types (runEffectsT, Settings)
import Gonimo.Pux (justEffect, noEffects, onlyEffects, EffModel(EffModel))
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


type State = Loading.LoadedState

data Action = ReportError Client.Error
            | SetState State
            | Nop


--------------------------------------------------------------------------------

update :: forall eff. Action -> State -> EffModel eff State Action 
update (SetState state)  = const $ noEffects state
update (ReportError err) = justEffect $ handleError Nop err 
update Nop               = noEffects


--------------------------------------------------------------------------------

view :: State -> Html Action
view state = viewLogo $ span [] [ text "We did it - Ready to rumble!"]

