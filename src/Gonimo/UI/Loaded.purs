-- | Loaded application ui logic
module Gonimo.UI.Loaded where

import Prelude
import Gonimo.UI.Html
import Gonimo.Client.Effects as Gonimo
import Gonimo.Client.LocalStorage as Key
import Gonimo.Client.Types as Gonimo
import Gonimo.UI.Invite as InviteC
import Gonimo.UI.AcceptInvitation as AcceptC
import Pux.Html.Attributes as A
import Pux.Html.Events as E
import Browser.LocalStorage (STORAGE, localStorage)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Bifunctor (bimap)
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(..))
import Debug.Trace (trace)
import Gonimo.Client.Effects (handleError)
import Gonimo.Client.Types (runGonimoT, Settings)
import Gonimo.Pux (justEffect, noEffects, onlyEffects, EffModel(EffModel))
import Gonimo.Server.Types (AuthToken, AuthToken(GonimoSecret))
import Gonimo.Types (Secret(Secret))
import Gonimo.WebAPI (SPParams_(SPParams_), postAccounts)
import Gonimo.WebAPI.Types (AuthData(AuthData))
import Partial.Unsafe (unsafeCrashWith)
import Pux (renderToDOM, fromSimple, start)
import Pux.Html (button, input, p, h1, text, span, Html, img, div)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (defaultSettings, SPSettings_(SPSettings_))
import Signal (constant, Signal)


type State = {
               authData :: AuthData
             , settings :: Settings
             , inviteS  :: InviteC.State
             , acceptS  :: AcceptC.State
             }

data Action = ReportError Gonimo.Error
            | SetState State
            | InviteA InviteC.Action
            | AcceptA AcceptC.Action
            | HandleInvite Secret
            | Nop


--------------------------------------------------------------------------------

update :: forall eff. Action -> State -> EffModel eff State Action
update (SetState state)      = const $ noEffects state
update (ReportError err)     = justEffect $ handleError Nop err
update (InviteA action)      = updateInvite action
update (HandleInvite secret) = justEffect $ pure (AcceptA $ AcceptC.LoadInvitation secret)
update (AcceptA action)      = updateAccept action
update Nop                   = noEffects


updateInvite :: forall eff. InviteC.Action -> State -> EffModel eff State Action
updateInvite action state = bimap (state {inviteS = _}) InviteA
                            $ InviteC.update state.settings action state.inviteS

updateAccept :: forall eff. AcceptC.Action -> State -> EffModel eff State Action
updateAccept action state = bimap (state {acceptS = _}) AcceptA
                            $ AcceptC.update state.settings action state.acceptS
--------------------------------------------------------------------------------

view :: State -> Html Action
-- view state = map InviteA $ InviteC.view state.inviteS
view state = map AcceptA $ AcceptC.view state.acceptS
