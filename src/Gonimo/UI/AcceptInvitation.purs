module Gonimo.UI.AcceptInvitation where



import Gonimo.UI.Html
import Gonimo.Client.Effects as Gonimo
import Gonimo.Client.LocalStorage as Key
import Gonimo.Client.Types as Gonimo
import Gonimo.WebAPI.Types as WebAPI
import Pux.Html.Attributes as A
import Pux.Html.Events as E
import Browser.LocalStorage (STORAGE, localStorage)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Either (Either(Right, Left))
import Data.Generic (gShow)
import Data.Maybe (isJust, isNothing, Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Gonimo.Client.Effects (handleError)
import Gonimo.Client.Types (Gonimo, Settings, runGonimoT, class ReportErrorAction)
import Gonimo.Pux (noEffects, justEffect, onlyEffects, EffModel(EffModel), justGonimo)
import Gonimo.Server.DbEntities (Invitation(Invitation))
import Gonimo.Server.Types (InvitationDelivery(EmailInvitation), AuthToken, AuthToken(GonimoSecret))
import Gonimo.Types (Key(Key), Family(Family), Secret(Secret))
import Gonimo.WebAPI (putInvitationInfoByInvitationSecret, postInvitations, postFamilies, SPParams_(SPParams_), postAccounts)
import Gonimo.WebAPI.Types (InvitationInfo(InvitationInfo), AuthData(AuthData))
import Partial.Unsafe (unsafeCrashWith)
import Pux (renderToDOM, fromSimple, start)
import Pux.Html (button, input, p, h1, text, span, Html, img, div)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (defaultSettings, SPSettings_(SPSettings_))
import Signal (constant, Signal)
import Prelude hiding (div)



type State = Maybe { invitationInfo :: InvitationInfo
                   , accepted :: Maybe Boolean
                   }


init :: State
init = Nothing

data Action = LoadInvitation Secret
            | Init InvitationInfo
            | Accept
            | Decline
            | ReportError Gonimo.Error
            | Nop

instance reportErrorActionAction :: ReportErrorAction Action where
  reportError = ReportError

update :: forall eff. Settings -> Action -> State -> EffModel eff State Action
update settings action = case action of
  LoadInvitation secret -> justGonimo settings $ loadInvitation secret
  Init inv              -> \state -> noEffects $ case state of
    Nothing -> Just { invitationInfo : inv, accepted : Nothing }
    Just state -> Just state { invitationInfo = inv }
  Accept                -> noEffects <<< const init
  Decline               -> noEffects <<< const init
  Nop                   -> noEffects
  ReportError err       -> justEffect $ Gonimo.handleError Nop err

loadInvitation :: forall eff. Secret -> Gonimo eff Action
loadInvitation = map Init <<< putInvitationInfoByInvitationSecret

--------------------------------------------------------------------------------

view :: State -> Html Action
view Nothing = viewLoading "Loading your invitation - stay tight ..."
view (Just state) = case state.accepted of
  Nothing    ->  viewAskUser state.invitationInfo
  Just true  ->  viewAccepted state.invitationInfo
  Just false ->  viewDeclined state.invitationInfo

viewAskUser :: InvitationInfo -> Html Action
viewAskUser (InvitationInfo invitation) =
    div []
        [ h1 [] [ text "Gonimo Family Invitation!"]
        , p []  [ text $ "You received an invitation to join family " <> invitation.invitationInfoFamily <> "!"
                , text $ "You got invited by a device answering to the name: " <> invitation.invitationInfoSendingClient
                ]

        , div [ E.onKeyUp handleEnter ]
              [ p []
                  [ text $ "Do you really want to join the almighty family \"" <> invitation.invitationInfoFamily <>"\"?"
                    , text $ "Pick wisely, gonimo is the most awesome baby monitor on the planet, but only with the right family!"
                  ]
              , p []
                [ button [ A.title "Hello my dear family!"
                          , E.onClick $ const $ Accept
                          ]
                          [ text "Accept" ]
                ]
              , p []
                  [ button [ A.title "... of this bloody stalker!"
                          , E.onClick $ const $ Decline
                          ]
                          [ text "Decline this generous offer"]
                  ]
              ]
          ]
  where
    handleEnter :: E.KeyboardEvent -> Action
    handleEnter ev = if ev.keyCode == 13 then Accept else Nop

viewAccepted :: InvitationInfo -> Html Action
viewAccepted (InvitationInfo invitation) = viewLogo
                          $ span [ A.title "You chose wisely!" ]
                                 [ text $ "Your device <deviceName> is now a member of family: "
                                   <> invitation.invitationInfoFamily <> "!"
                                 ]

viewDeclined :: InvitationInfo -> Html Action
viewDeclined (InvitationInfo invitation) = viewLogo
                          $ span [ A.title "You chose wisely!" ]
                                 [ text $ "You did not join the stalker family: "
                                   <> invitation.invitationInfoFamily <> "!"
                                 ]
