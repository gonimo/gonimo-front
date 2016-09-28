module Gonimo.UI.Invite where



import Prelude hiding (div)
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
import Gonimo.Client.Types (Settings, GonimoError, Gonimo, runGonimoT, class ReportErrorAction)
import Gonimo.Pux (noEffects, justEffect, onlyEffects, EffModel(EffModel), justGonimo)
import Gonimo.Server.DbEntities (Family(Family))
import Gonimo.Server.Types (InvitationDelivery(EmailInvitation), AuthToken, AuthToken(GonimoSecret))
import Gonimo.Types (Key(Key), Secret(Secret))
import Gonimo.WebAPI (postInvitationsOutbox, postInvitationsByFamilyId, postFamilies, SPParams_(SPParams_), postAccounts, postFunnyName)
import Gonimo.WebAPI.Types (AuthData(AuthData))
import Partial.Unsafe (unsafeCrashWith)
import Pux (renderToDOM, fromSimple, start)
import Pux.Html (button, i, input, p, h1, h2, h3, text, span, Html, img, div)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (defaultSettings, SPSettings_(SPSettings_))
import Signal (constant, Signal)

type Props ps = { settings :: Settings
                , familyId :: (Maybe (Key Family))
                | ps
                }

type State =
  { familyName     :: String
  , email          :: String
  , invitationSent :: Boolean
  , errorOccurred  :: Maybe GonimoError
  }

init :: forall eff. Gonimo eff State
init = let
    initWithFamily family = { familyName :     family
                            , email :          ""
                            , invitationSent : false
                            , errorOccurred :  Nothing
                            }
  in
     initWithFamily <$> postFunnyName

data Action = SetFamilyName String
            | SetEmail String
            | SendInvitation
            | InvitationSent
            | ReportError GonimoError
            | Nop

instance reportErrorActionAction :: ReportErrorAction Action where
  reportError = ReportError

update :: forall eff ps. Props ps -> Action -> State -> EffModel eff State Action
update props action = case action of
  (SetFamilyName name ) -> \state -> noEffects state { familyName = name }
  (SetEmail email )     -> \state -> noEffects state { email = email }
  InvitationSent        -> \state -> noEffects state { invitationSent = true }
  SendInvitation        -> \state -> justGonimo props (handleSendInvitation props state) state
  Nop                   -> noEffects
  ReportError err       -> \state -> noEffects $ state { errorOccurred = Just err }


handleSendInvitation :: forall ps eff. Props ps -> State -> Gonimo eff Action
handleSendInvitation props state = do
  (SPSettings_ settings) <- ask
  let params = case settings.params of (SPParams_ params) -> params
  Gonimo.log $ "Using AuthToken: " <> gShow params.authorization
  fid <- case props.familyId of
    Nothing   -> postFamilies state.familyName
    Just fid' -> pure fid'
  (Tuple invId invitation) <- postInvitationsByFamilyId fid
  postInvitationsOutbox $ WebAPI.SendInvitation invId (EmailInvitation state.email)
  pure InvitationSent

--------------------------------------------------------------------------------

view :: forall ps. Props ps -> State -> Html Action
view props state = if state.invitationSent
  then viewSent state
  else viewSend props state

viewSend :: forall ps. Props ps -> State -> Html Action
viewSend props state =
  div [A.className "jumbotron"]
  [ div [A.className "container"]
    [ h1 [] [ text "Welcome to Gonimo!"]
    , p []  [ text "In order to get you started, invite a second device via email to your family:"]
    , h2 [] [ div [ A.className "well"]
                [ i [A.className "fa fa-users"] []
                , text " "
                , text state.familyName ]
            ]
    , div [ E.onKeyUp handleEnter ]
      [ div [A.className "input-group"]
          if isNothing props.familyId -- We can only set the family name here, if we are creating one!
          then
          [ p [] [text "Choose a name for your family."]
          , span [A.className "input-group-addon glyphicon glyphicon-edit"] []
          , input [ A.type_ "text"
                  , A.className "form-control"
                  , E.onInput $ \ev -> SetFamilyName ev.target.value
                  , A.value state.familyName
                  ] []
          ]
          else
          []

      , p [] []
      , div [A.className "input-group"]
        [ span [A.className "input-group-addon glyphicon glyphicon-envelope"] []
        , input [ A.type_ "text"
                , A.className "form-control"
                , A.placeholder "mail@example.com"
                , E.onInput $ \ev -> SetEmail ev.target.value
                , A.value state.email
                ] []
        ]
      ]
    , button [ A.className "btn btn-block btn-info"
             , A.style [Tuple "margin-left" "0px"]
             , A.type_ "button"
             , E.onClick $ const $ SendInvitation
             ]
             [ text " Send Invitation! "
             , span [A.className "glyphicon glyphicon-send"] []
             ]
    , p []
        if isJust state.errorOccurred
        then [ text $ "Error occurred!"]
        else []
    ]
  ]
  where
    handleEnter :: E.KeyboardEvent -> Action
    handleEnter ev = if ev.keyCode == 13 then SendInvitation else Nop

viewSent :: State -> Html Action
viewSent state = viewLogo $ text "Invitation sucessfully sent!"
