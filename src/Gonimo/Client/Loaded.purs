module Gonimo.Client.Loaded where

import Prelude
import Gonimo.Client.Html
import Gonimo.Client.Effects as Gonimo
import Gonimo.Client.Init as Init
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


type State = Init.LoadedState

data Action = ReportError Client.Error
            | SetState State
            | Nop


update :: forall eff. Action -> State -> EffModel State Action (Client.EffEffects eff)
update (ReportError err) state = onlyEffects state $ [ do Gonimo.error err
                                                          pure Nop
                                                     ]
update (SetState state) _      = noEffects state
update Nop state               = noEffects state

view :: State -> Html Action
view state = viewLogo $ span [] [ text "We did it - Ready to rumble!"]

fromInitAction :: Init.Action -> Action
fromInitAction (Init.ReportError err) = ReportError err
fromInitAction (Init.Init state) = SetState state
fromInitAction _ = Nop
