-- | Loaded application ui logic
module Gonimo.UI.Loaded where

import Prelude
import Gonimo.UI.Html
import Gonimo.Client.Effects as Gonimo
import Gonimo.Client.LocalStorage as Key
import Gonimo.Client.Types as Gonimo
import Gonimo.UI.AcceptInvitation as AcceptC
import Gonimo.UI.Invite as InviteC
import Gonimo.WebAPI.Subscriber as Sub
import Pux.Html.Attributes as A
import Pux.Html.Events as E
import Browser.LocalStorage (STORAGE, localStorage)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Reader (runReader)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Array (head)
import Data.Bifunctor (bimap)
import Data.Either (Either(Right, Left))
import Data.Foldable (foldl)
import Data.Generic (gShow)
import Data.Maybe (maybe, Maybe(..))
import Data.Monoid (mempty)
import Data.Semigroup (append)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Debug.Trace (trace)
import Gonimo.Client.Effects (handleError)
import Gonimo.Client.Types (runGonimoT, Settings)
import Gonimo.Pux (justEffect, noEffects, onlyEffects, EffModel(EffModel))
import Gonimo.Server.DbEntities (Family(Family))
import Gonimo.Server.DbEntities.Helpers (runFamily)
import Gonimo.Server.Types (AuthToken, AuthToken(GonimoSecret))
import Gonimo.Types (Key(Key), Secret(Secret))
import Gonimo.WebAPI (SPParams_(SPParams_), postAccounts)
import Gonimo.WebAPI.Types (AuthData(AuthData))
import Gonimo.WebAPI.Types.Helpers (runAuthData)
import Partial.Unsafe (unsafeCrashWith)
import Pux (renderToDOM, fromSimple, start)
import Pux.Html (button, input, p, h1, text, span, Html, img, div)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (defaultSettings, SPSettings_(SPSettings_))
import Servant.Subscriber (Subscriber)
import Servant.Subscriber.Connection (Notification)
import Servant.Subscriber.Subscriptions (Subscriptions)
import Signal (constant, Signal)

type State = { authData :: AuthData
             , settings :: Settings
             , subscriber :: Subscriber () Action -- Hack: I don't want to carry eff everywhere.
             , inviteS  :: InviteC.State
             , acceptS  :: AcceptC.State
             , central  :: Central
             , families :: Array (Tuple (Key Family) Family)
             }

data Action = ReportError Gonimo.Error
            | SetState State
            | InviteA InviteC.Action
            | AcceptA AcceptC.Action
            | HandleInvite Secret
            | SetFamilies (Array (Tuple (Key Family) Family))
            | HandleSubscriber Notification
            | Nop

data Central = CentralInvite
             | CentralAccept

setCentral :: Central -> State -> State
setCentral central = _ { central = central }

getInviteState :: State -> InviteC.State
getInviteState state = case head state.families of
  Nothing -> state.inviteS
  Just (Tuple key family) -> state.inviteS { familyId = Just key
                                           , familyName = (runFamily family).familyName
                                           }
--------------------------------------------------------------------------------

update :: forall eff. Action -> State -> EffModel eff State Action
update (SetState state)      = const $ noEffects state
update (ReportError err)     = justEffect $ handleError Nop err
update (InviteA action)      = updateInvite action
update (HandleInvite secret) = setCentral CentralAccept
                               >>> justEffect (inviteEffect secret)
update (AcceptA action)      = updateAccept action
update (SetFamilies families') = \state -> noEffects (state {families = families'})
update (HandleSubscriber msg) = justEffect (do
                                            Gonimo.log <<< gShow $ msg
                                            pure Nop)
update Nop                   = noEffects


updateInvite :: forall eff. InviteC.Action -> State -> EffModel eff State Action
updateInvite action state = bimap (state {inviteS = _}) InviteA
                            $ InviteC.update state.settings action (getInviteState state)

updateAccept :: forall eff. AcceptC.Action -> State -> EffModel eff State Action
updateAccept action state = bimap (state {acceptS = _}) AcceptA
                            $ AcceptC.update state.settings action state.acceptS

inviteEffect :: forall m. Monad m => Secret -> m Action
inviteEffect = pure <<< AcceptA <<< AcceptC.LoadInvitation
--------------------------------------------------------------------------------

view :: State -> Html Action
view = viewCentral


viewCentral :: State -> Html Action
viewCentral state = case state.central of
  CentralInvite -> map InviteA $ InviteC.view (getInviteState state)
  CentralAccept -> map AcceptA $ AcceptC.view state.acceptS

--------------------------------------------------------------------------------

subscriptions :: State -> Subscriptions Action
subscriptions state =
  let
    subArray :: Array (Subscriptions Action)
    subArray = map (flip runReader state.settings)
      [ Sub.getFamiliesByAccountId (maybe Nop SetFamilies)
                                  (runAuthData state.authData).accountId
      ]
  in
   foldl append mempty subArray
