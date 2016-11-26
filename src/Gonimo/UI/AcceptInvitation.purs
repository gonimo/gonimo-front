module Gonimo.UI.AcceptInvitation where



import Gonimo.UI.Html
import Data.Tuple as Tuple
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
import Control.Monad.IO (IO)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.State.Class (get, put)
import Data.Array (fromFoldable)
import Data.Bifunctor (lmap)
import Data.Either (Either(Right, Left))
import Data.Generic (gShow)
import Data.Lens (lens, LensP, (.=), (^?), (^.), _Just)
import Data.Maybe (maybe, fromMaybe, isJust, isNothing, Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Gonimo.Client.Types (GonimoError(UnexpectedAction), Gonimo, Settings, class ReportErrorAction)
import Gonimo.Pux (noEffects, Component, makeChildData, liftChild, onlyModify, Update, ToChild, runGonimo, ComponentType, class MonadComponent)
import Gonimo.Server.Db.Entities (Family(Family), Device(Device), Invitation(Invitation))
import Gonimo.Server.Types (DeviceType, InvitationDelivery(EmailInvitation), AuthToken, AuthToken(GonimoSecret))
import Gonimo.Types (Key(Key), Secret(Secret))
import Gonimo.Util (fromMaybeM)
import Gonimo.WebAPI (deleteInvitationsByInvitationSecret, putInvitationsInfoByInvitationSecret, postFamilies, SPParams_(SPParams_), postAccounts)
import Gonimo.WebAPI.Lenses (_DeviceInfo, deviceInfoName)
import Gonimo.WebAPI.Types (DeviceInfo(DeviceInfo), InvitationReply(InvitationReject, InvitationAccept), InvitationReply(InvitationReject, InvitationAccept), InvitationInfo(InvitationInfo), AuthData(AuthData))
import Partial.Unsafe (unsafeCrashWith)
import Prelude (class BooleanAlgebra)
import Pux (renderToDOM, fromSimple, start)
import Pux.Html (br, em, button, input, p, h3, i, text, span, Html, img, div)
import Pux.Html.Attributes (offset)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (defaultSettings, SPSettings_(SPSettings_))
import Signal (constant, Signal)
import Prelude hiding (div)


type Props ps = { settings :: Settings
                , deviceInfos :: Array (Tuple (Key Device) DeviceInfo)
                , deviceId :: Key Device | ps
                }

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
            | SetAccepted (Maybe (Key Family))
            | EnteredFamily (Key Family) 
            | ReportError GonimoError
            | GoToBabyStation
            | Nop

instance reportErrorActionAction :: ReportErrorAction Action where
  reportError = ReportError

update :: forall ps. Action -> Component (Props ps) State (Array (IO Action))
update action = do
   r <- liftChild toJust (updateJust action)
   case r of
     Nothing -> updateNothing action
     Just v -> pure v
   -- myFromMaybeM (updateNothing action)
   -- $ liftChild toJust (updateJust action)

toJust :: forall ps. ToChild (Props ps) State (Props ps) StateImpl
toJust = do
  props <- ask
  pure $ makeChildData _Just props

-- Whether or not the invitation was accepted already ...
isAccepted :: State -> Boolean
isAccepted state = fromMaybe false $ do
  rState <- state
  rState.accepted

updateNothing :: forall ps. Update (Props ps) State Action
updateNothing action = case action of
  LoadInvitation secret   -> runGonimo $ loadInvitation secret
  Init (Tuple secret inv) -> put ( Just { invitationInfo : inv, invitationSecret : secret, accepted : (Nothing :: Maybe Boolean) } ) *> pure []
  Nop                     -> noEffects
  ReportError err         -> noEffects
  _                       -> pure [ pure (ReportError $ UnexpectedAction "Received some Action but State is Nothing!") ]

updateJust :: forall ps. Update (Props ps) StateImpl Action
updateJust action = case action of
  LoadInvitation secret   -> runGonimo $ loadInvitation secret
  Init (Tuple secret inv) -> onlyModify $ _ { invitationInfo = inv, invitationSecret = secret, accepted = (Nothing :: Maybe Boolean)}
  Accept                  -> answerInvitation InvitationAccept
  Decline                 -> answerInvitation InvitationReject
  SetAccepted mFamilyId'   -> handleSetAccepted mFamilyId' 
  Nop                     -> noEffects
  GoToBabyStation         -> noEffects
  ReportError err         -> noEffects
  EnteredFamily _         -> noEffects


loadInvitation :: Secret -> Gonimo Action
loadInvitation secret = Init <<< Tuple secret <$> putInvitationsInfoByInvitationSecret secret

answerInvitation :: forall ps. InvitationReply -> ComponentType (Props ps) StateImpl Action
answerInvitation reply = do
  state <- get
  runGonimo $ do
    SetAccepted <$> deleteInvitationsByInvitationSecret reply state.invitationSecret

handleSetAccepted :: forall ps. Maybe (Key Family) -> ComponentType (Props ps) StateImpl Action
handleSetAccepted mFamilyId' = do
  accepted .= Just (maybe false (const true) mFamilyId')
  pure $ pure <<< EnteredFamily <$> fromFoldable mFamilyId'



    --------------------------------------------------------------------------------

view :: forall ps. Props ps -> State -> Html Action
view _ Nothing = viewLoading "Loading your invitation - stay tight ..."
view props (Just state) = case state.accepted of
  Nothing    ->  viewAskUser state.invitationInfo
  Just true  ->  viewAccepted props state.invitationInfo
  Just false ->  viewDeclined state.invitationInfo

viewAskUser :: InvitationInfo -> Html Action
viewAskUser (InvitationInfo invitation) =
    div []
        [ h3 [] [ text "Gonimo Family Invitation!"]
        , div [A.className "jumbotron"]
          [ div [A.className "container"]
            [ text $ "You received an invitation to join family: "
            , em [] [ text invitation.invitationInfoFamily ]
            , text "!"
            , br [] []
            , text $ "You got invited by a device answering to the name: "
            , em [] [ text invitation.invitationInfoSendingDevice ]
            , text "."
            ]
          ]

        , div [ E.onKeyUp handleEnter ]
          [ p [] [ text $ "Do you really want to join the family \""
                <> invitation.invitationInfoFamily <> "\"?"
                 ]
          , p []
              [ text $ "Pick wisely! Gonimo is the most awesome baby monitor on the planet, but only with the right family!"
              ]
          , div [ A.className "btn-group btn-group-justified"
                , A.role "group"]
            [ div [ A.className "btn-group"
                  , A.role "group"]
              [ button [ A.className "btn btn-block btn-danger"
                       , A.type_ "button"
                       , E.onClick $ const $ Decline
                       ] [ text "Decline "
                         , i [A.className "fa fa-fw fa-times"] []
                         ]
              ]
            , div [ A.className "btn-group"
                  , A.role "group"]
              [ button [ A.title "Hello my dear family!"
                       , E.onClick $ const $ Accept
                       , A.className "btn btn-block btn-success"
                       , A.type_ "button"
                       ] [ text "Accept "
                         , span [A.className "hidden-xs"] [text "this generous offer "]
                         , i [A.className "fa fa-fw fa-check"] []
                         ]
              ]
            ]
          ]
        ]
  where
    handleEnter :: E.KeyboardEvent -> Action
    handleEnter ev = if ev.keyCode == 13 then Accept else Nop

viewAccepted :: forall ps. Props ps -> InvitationInfo -> Html Action
viewAccepted props (InvitationInfo invitation) =
  viewLogo $
  div []
  [  span [ A.title "You chose wisely!" ]
          [ p []
            [ text $ "Your device \"" <> getDeviceName props <> "\" is now a member of family: "
              <> invitation.invitationInfoFamily <> "!"
            ]
          , p []
            [ text "Now go on and ...." ]
          ]
  , button [ A.title "Go to baby station view"
           , A.className "btn btn-block btn-info"
           , A.type_ "button"
           , E.onClick $ const GoToBabyStation
           ]
    [ text "make this device a Baby Station" ]

  ]

viewDeclined :: InvitationInfo -> Html Action
viewDeclined (InvitationInfo invitation) = viewLogo
                          $ span [ A.title "You chose wisely!" ]
                                 [ text $ "You did not join the stalker family: "
                                 , em [] [ text invitation.invitationInfoFamily ]
                                 , text "!"
                                 ]

getDeviceName :: forall ps. Props ps -> String
getDeviceName props = fromMaybe "No Name Cowboy"
                      <<< (_^?_Just<<<_DeviceInfo<<<deviceInfoName)
                      $ Tuple.lookup props.deviceId props.deviceInfos

accepted :: LensP StateImpl (Maybe Boolean)
accepted = lens _.accepted (_ { accepted = _ })
