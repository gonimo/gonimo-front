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
import Data.Bifunctor (lmap)
import Data.Either (Either(Right, Left))
import Data.Generic (gShow)
import Data.Maybe (isJust, isNothing, Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Gonimo.Client.Effects (handleError)
import Gonimo.Client.Types (Error(UnexpectedAction), Gonimo, Settings, runGonimoT, class ReportErrorAction)
import Gonimo.Pux (noEffects, onlyEffect, onlyGonimo, justEffect, onlyEffects, EffModel(EffModel), justGonimo)
import Gonimo.Server.DbEntities (Invitation(Invitation))
import Gonimo.Server.Types (InvitationDelivery(EmailInvitation), AuthToken, AuthToken(GonimoSecret))
import Gonimo.Types (Key(Key), Family(Family), Secret(Secret))
import Gonimo.WebAPI (deleteInvitationsByInvitationSecret, putInvitationInfoByInvitationSecret, postInvitations, postFamilies, SPParams_(SPParams_), postAccounts)
import Gonimo.WebAPI.Types (InvitationReply(InvitationReject, InvitationAccept), InvitationReply(InvitationReject, InvitationAccept), InvitationInfo(InvitationInfo), AuthData(AuthData))
import Partial.Unsafe (unsafeCrashWith)
import Pux (renderToDOM, fromSimple, start)
import Pux.Html (em, button, input, p, h1, text, span, Html, img, div)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (defaultSettings, SPSettings_(SPSettings_))
import Signal (constant, Signal)
import Prelude hiding (div)



type State = Maybe StateImpl

type StateImpl = { invitationInfo :: InvitationInfo
                 , invitationSecret :: Secret
                 , accepted :: Maybe Boolean
                 }

init :: State
init = Nothing

data Action = LoadInvitation Secret
            | Init (Tuple Secret InvitationInfo)
            | Accept
            | Decline
            | SetAccepted Boolean
            | ReportError Gonimo.Error
            | Nop

instance reportErrorActionAction :: ReportErrorAction Action where
  reportError = ReportError

update :: forall eff. Settings -> Action -> State -> EffModel eff State Action
update settings action Nothing = case action of
  LoadInvitation secret -> onlyGonimo settings Nothing $ loadInvitation secret
  Init (Tuple secret inv) -> noEffects $ Just { invitationInfo : inv, invitationSecret : secret, accepted : Nothing }
  Nop                   -> noEffects Nothing
  ReportError err       -> onlyEffect Nothing $ Gonimo.handleError Nop err
  _                     -> onlyEffect Nothing <<< pure <<< ReportError
                            $ UnexpectedAction "Received some Action but State is Nothing!"
update settings action (Just state) = lmap Just $ updateJust settings action state


updateJust :: forall eff. Settings -> Action -> StateImpl -> EffModel eff StateImpl Action
updateJust settings action = case action of
  LoadInvitation secret   -> justGonimo settings $ loadInvitation secret
  Init (Tuple secret inv) -> noEffects <<< _ { invitationInfo = inv, invitationSecret = secret, accepted = Nothing }
  Accept                  -> answerInvitation settings InvitationAccept
  Decline                 -> answerInvitation settings InvitationReject
  SetAccepted accepted'   -> noEffects <<<  _ { accepted = Just accepted' }
  Nop                     -> noEffects
  ReportError err         -> justEffect $ Gonimo.handleError Nop err


loadInvitation :: forall eff. Secret -> Gonimo eff Action
loadInvitation secret = Init <<< Tuple secret <$> putInvitationInfoByInvitationSecret secret

answerInvitation :: forall eff. Settings -> InvitationReply -> StateImpl -> EffModel eff StateImpl Action
answerInvitation settings reply state = onlyGonimo settings state $ do
    deleteInvitationsByInvitationSecret reply state.invitationSecret
    pure $ SetAccepted $ case reply of
      InvitationAccept  -> true
      InvitationReject  -> false

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
        [ h1 []
             [ text "Gonimo Family Invitation!"]
        , p []
            [ text $ "You received an invitation to join family: "
            , em [] [ text invitation.invitationInfoFamily ]
            , text "!"
            ]
        , p []
            [ text $ "You got invited by a device answering to the name: "
            , em [] [ text invitation.invitationInfoSendingClient ]
            , text "."
            ]

        , div [ E.onKeyUp handleEnter ]
              [ p []
                  [ text $ "Do you really want to join the almighty family \""
                    <> invitation.invitationInfoFamily <> "\"?"
                  ]
              , p []
                  [ text $ "Pick wisely! Gonimo is the most awesome baby monitor on the planet, but only with the right family!"
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
                                 , em [] [ text invitation.invitationInfoFamily ]
                                 , text "!"
                                 ]
