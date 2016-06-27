module Gonimo.Client.Loading where

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

data State = Loading


data Action = Init.Action


update :: forall eff. Action -> EffModel State Action (Client.EffEffects eff)
update Start = onlyEffects Loading [ init ]

view :: State -> Html Action
view _ = viewLogo $ span [] [ text "Loading your gonimo, stay tight ..."]
