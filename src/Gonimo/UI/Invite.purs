module Gonimo.UI.Invite where



import Gonimo.UI.Html
import Gonimo.Client.Effects as Gonimo
import Gonimo.Client.LocalStorage as Key
import Gonimo.Client.Types as Gonimo
import Gonimo.UI.Html as Html
import Gonimo.WebAPI.Types as WebAPI
import Pux.Html.Attributes as A
import Pux.Html.Attributes.Aria as A
import Pux.Html.Elements as H
import Pux.Html.Events as E
import Browser.LocalStorage (STORAGE, localStorage)
import Control.Apply ((*>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.IO (IO)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.State.Class (get, modify)
import Data.Either (Either(Right, Left))
import Data.Generic (gShow)
import Data.Maybe (isJust, isNothing, Maybe(..))
import Data.Tuple (uncurry, Tuple(Tuple))
import Global (encodeURIComponent, encodeURI)
import Gonimo.Client.Types (Settings, GonimoError, Gonimo, class ReportErrorAction)
import Gonimo.Pux (noEffects, onlyModify, Update, runGonimo, class MonadComponent)
import Gonimo.Server.DbEntities (Invitation(Invitation), Family(Family))
import Gonimo.Server.Types (InvitationDelivery(EmailInvitation), AuthToken, AuthToken(GonimoSecret))
import Gonimo.Types (Key(Key), Secret(Secret))
import Gonimo.WebAPI (postInvitationsOutbox, postInvitationsByFamilyId, postFamilies, SPParams_(SPParams_), postAccounts, postFunnyName)
import Gonimo.WebAPI.Types (AuthData(AuthData))
import Partial.Unsafe (unsafeCrashWith)
import Pux (renderToDOM, fromSimple, start)
import Pux.Html (a, button, br, i, input, p, h1, h2, h3, text, span, Html, img, div, small, li, ul, nav)
import Pux.Html.Attributes (offset)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (gDefaultEncodeURLPiece, defaultSettings, SPSettings_(SPSettings_))
import Signal (constant, Signal)
import Prelude hiding (div)

type Props ps = { settings  :: Settings
                , rFamilyId :: Key Family
                , rFamily   :: Family
                , baseURL :: String
                | ps
                }

type State =
  { email          :: String
  , invitationSent :: Boolean
  , invitationId   :: Key Invitation
  , invitation   :: Invitation
  }

-- | We take our props in init to ensure the caller takes care of providing proper props.
--   Ensuring this on the first call to update is a tad to late.
init :: forall ps. Tuple (Key Invitation) Invitation -> Props ps -> State
init (Tuple invKey inv) _ = { email : ""
                            , invitationSent : false
                            , invitationId : invKey
                            , invitation : inv
                            }


data Action = SetEmail String
            | SendInvitation
            | InvitationSent
            | ReportError GonimoError
            | MakeNewInvitation
            | SetInvitationData (Key Invitation) Invitation
            | GoToOverview
            | GoToBabyStation
            | Nop

instance reportErrorActionAction :: ReportErrorAction Action where
  reportError = ReportError

update :: forall ps. Update (Props ps) State Action
update action = case action of
  (SetEmail email )         -> onlyModify $ _ { email = email, invitationSent = false }
  InvitationSent            -> handleInvitationSent
  SendInvitation            -> handleSendInvitation
  MakeNewInvitation         -> handleMakeNewInvitation
  SetInvitationData id' inv -> onlyModify $ \state -> state { invitationId = id', invitation = inv }
  GoToOverview              -> noEffects
  GoToBabyStation           -> noEffects
  ReportError err           -> noEffects
  Nop                       -> noEffects

makeInviteLink :: String -> Invitation -> String
makeInviteLink baseURL (Invitation inv) = baseURL <> "?" <> "acceptInvitation="
                                          <> gDefaultEncodeURLPiece inv.invitationSecret

handleInvitationSent :: forall m ps. (MonadComponent (Props ps) State m) => m (Array (IO Action))
handleInvitationSent = do
  modify $ _ { invitationSent = true }
  pure [ pure MakeNewInvitation ] -- Prevent user from re-using the already sent invitation

handleMakeNewInvitation :: forall m ps. (MonadComponent (Props ps) State m) => m (Array (IO Action))
handleMakeNewInvitation = do
  props <- ask
  runGonimo $ uncurry SetInvitationData <$> postInvitationsByFamilyId props.rFamilyId

handleSendInvitation :: forall m ps. (MonadComponent (Props ps) State m) => m (Array (IO Action))
handleSendInvitation = do
  state <- get
  runGonimo $ do
    postInvitationsOutbox $ WebAPI.SendInvitation state.invitationId (EmailInvitation state.email)
    pure InvitationSent

--------------------------------------------------------------------------------

view :: forall ps. Props ps -> State -> Html Action
view props state =
    let invitationLink = makeInviteLink (props.baseURL) state.invitation
        escapedLink = encodeURIComponent invitationLink
     in div []
                   [ div [A.className "page-header"]
                     [ div [ A.className "container"
                           , A.style [Tuple "width" "100%"]]
                       [ h3 [] [ text "Device Management"
                               , br [] []
                               , small []
                                 [ text $ "Here you can add new devices to your"
                                       <> " families, you only have to visit the"
                                       <> " following one-time link on another device." ]
                               ]
                       ]
                     ]
                   , div [ A.className "container"
                         , A.style [Tuple "width" "100%"]]
                     [ h3 [] [text "copy & paste"]
                     , div [A.className "jumbotron"]
                       [ p [] [ text $ "You can either copy & paste it in to your"
                                    <> " favourite messenger (e.g. WhatsApp) to"
                                    <> " send it."
                              ]
                       , div [ A.className "input-group" ]
                           [ span [ A.className "input-group-addon glyphicon glyphicon-copy", A.id_ "sdflkjsll291" ]
                             []
                           , input [ A.type_ "text", A.className "form-control", A.readOnly true
                                   , A.value invitationLink
                                   ] []
                           ]
                       ]
                     ]
                   , div [ A.className "container"
                         , A.style [Tuple "width" "100%"]]
                     [ h3 [] [text "Share it"]
                     , div [A.className "jumbotron"]
                       [ p [] [ text $ "Or you can share it directly with WhatsApp, Telegram .."
                              ]
                       , p []
                         [
                           H.a [ A.href $ "whatsapp://send?text=" <> escapedLink
                               ] [ H.text "Whatsapp" ]
                         , H.text " "
                         , H.a [ A.href $ "https://telegram.me/share/url?url=" <> escapedLink
                               ] [ H.text "Telegram" ]
                         ]
                       ]
                     ]
                   , div [A.className "container"
                         , A.style [Tuple "width" "100%"]]
                     [ h3 [] [text "email"]
                     , div [A.className "jumbotron"]
                       [ p []  [ text "Or you can let us do the work send it via email …"
                               ]
                       , div [ E.onKeyUp handleEnter ]
                         [ div [A.className "input-group"]
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
                       , if state.invitationSent
                         then div [ A.className "alert alert-success"]
                              [ h3 [] [text $ "Invitation sent successfully to '"
                                           <> state.email <> "'!"
                                      , br [] []
                                      , small [] [text $ "But don't forget to add"
                                            <> " 'noreply@gonimo.com' to your address book,"
                                            <> " otherwise this invitation might end up in"
                                            <> " your spam-folder!"]
                                      ]
                              , button [ A.className "btn btn-block btn-default"
                                       , A.style [Tuple "margin-left" "0px"] ]
                                [ text $ "Oops - Did you send the invitation to the wrong recipient?"
                                , br [] [] 
                                , text "No problem just click me and then and then 'decline' to invalidate the link."
                                , br [] []
                                , br [] []
                                , text "TODO: how to preserve the previous one time link?"
                                ]
                              ]
                         else span [] []
                       ]
                     ]
                   , div [ A.className "container"
                         , A.style [Tuple "width" "100%"]]
                       [ text "Invitation successfully transmitted? Then go back to overview or make this device a baby station right away .... " ]
                   , nav []
                     [ ul [ A.className "pager" ]
                       [ li [ A.className "previous"
                            , E.onClick $ const $ GoToOverview
                            ] [a [] [ span [A.ariaHidden "true"] [text "← "]
                                    , text "Back to Overview"
                                    ]
                              ]
                       , li [ A.className "next"
                            , E.onClick $ const $ GoToBabyStation
                            ] [ a [] [ text "Make this device a baby station"
                                     , span [A.ariaHidden "true"] [text " →"]
                                     ]
                              ]
                       ]

                     ]
                   , div [A.className "jumbotron"]
                      [ div [A.className "container"]
                        [ p [] [ text "Want to add another device?" ]
                        , button [ A.className "btn btn-block btn-info"
                                 , A.style [Tuple "margin-left" "0px"]
                                 , A.type_ "button"
                                 , E.onClick $ const $ MakeNewInvitation
                                 ]
                          [ span [ A.className "glyphicon glyphicon-repeat"] []
                          , text " Generate new one-time link "
                          ]
                        ]
                      ]
                    ]
  where
    handleEnter :: E.KeyboardEvent -> Action
    handleEnter ev = if ev.keyCode == 13 then SendInvitation else Nop

viewSent :: State -> Html Action
viewSent state = viewLogo $ text "Invitation sucessfully sent!"
