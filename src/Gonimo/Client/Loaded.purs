module Gonimo.Client.Loaded where

import Prelude
import Pux.Html.Attributes as A
import Browser.LocalStorage (STORAGE, localStorage)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)
import Pux (noEffects, onlyEffects, EffModel, renderToDOM, fromSimple, start)
import Pux.Html (text, span, Html, img, div)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (defaultSettings, SPSettings_(SPSettings_))
import Signal (constant, Signal)


import Gonimo.Client.Types (runEffectsT, Settings)
import Gonimo.Server.Types (AuthToken, AuthToken(GonimoSecret))
import Gonimo.Types (Secret(Secret))
import Gonimo.WebAPI (SPParams_(SPParams_), postAccounts)
import Gonimo.WebAPI.Types (AuthData(AuthData))
import Gonimo.Client.Types as Client
import Gonimo.LocalStorage as Key
import Gonimo.Client.Init as Init
import Gonimo.Client.Effects as Gonimo
import Gonimo.Client.Html


type State = Init.LoadedState

data Action = ReportError Client.Error
            | Nop


update :: Action -> State -> EffModel State Action (Client.Effects eff)
update (ReportError err ) state = onlyEffects $ [Gonimo.error err >> Nop]

view :: State -> Html Action
view state = viewLogo $ span [] [ text "We did it - Ready to rumble!"]
