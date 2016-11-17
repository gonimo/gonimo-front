module Gonimo.UI.Invite where



import Gonimo.UI.Html
import Gonimo.Client.Effects as Gonimo
import Gonimo.Client.LocalStorage as Key
import Gonimo.Client.Types as Gonimo
import Gonimo.UI.Html as Html
import Gonimo.WebAPI.Types as WebAPI
import Pux.Html.Attributes as A
import Pux.Html.Attributes.Aria as A
import Pux.Html.Attributes.Bootstrap as A
import Pux.Html.Elements as H
import Pux.Html.Events as E
import Browser.LocalStorage (STORAGE, localStorage)
import Control.Apply ((*>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
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
import Gonimo.Pux (Component, ComponentType, noEffects, onlyModify, Update, runGonimo, class MonadComponent, onClickWithDefault)
import Gonimo.Server.Db.Entities (Invitation(Invitation), Family(Family))
import Gonimo.Server.Types (InvitationDelivery(EmailInvitation), AuthToken, AuthToken(GonimoSecret))
import Gonimo.Types (Key(Key), Secret(Secret))
import Gonimo.WebAPI (postInvitationsOutbox, postInvitationsByFamilyId, postFamilies, SPParams_(SPParams_), postAccounts, postFunnyName)
import Gonimo.WebAPI.Types (AuthData(AuthData))
import Partial.Unsafe (unsafeCrashWith)
import Pux (renderToDOM, fromSimple, start)
import Pux.Html (a, button, br, i, input, p, h1, h2, h3, text, span, Html, img, div, small, li, ul, nav)
import Pux.Html.Attributes (letterSpacing, offset)
import Pux.Html.Elements (br)
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
  , invitationId   :: Key Invitation
  , invitation     :: Invitation
  , sentInvitation :: Maybe SentInvitation
  }

-- | We take our props in init to ensure the caller takes care of providing proper props.
--   Ensuring this on the first call to update is a tad to late.
init :: forall ps. Tuple (Key Invitation) Invitation -> Props ps -> State
init (Tuple invKey inv) _ = { email : ""
                            , sentInvitation : Nothing
                            , invitationId : invKey
                            , invitation : inv
                            }


data Action = SetEmail String
            | SendInvitation
            | InvitationSent SentMethod
            | ReportError GonimoError
            | MakeNewInvitation
            | SetInvitationData (Key Invitation) Invitation
            | GoToOverview
            | GoToBabyStation
            | CopyToClipboard String
            | Nop

type SentInvitation = { invitation :: Invitation
                      , sentMethod :: SentMethod
                      }

data SentMethod = SentWhatsApp | SentTelegram | SentCopyPaste | SentEmail String

instance reportErrorActionAction :: ReportErrorAction Action where
  reportError = ReportError

update :: forall ps. Update (Props ps) State Action
update action = case action of
  (SetEmail email )         -> onlyModify $ _ { email = email, sentInvitation = Nothing :: Maybe SentInvitation }
  InvitationSent sent       -> handleInvitationSent sent
  SendInvitation            -> handleSendInvitation
  MakeNewInvitation         -> handleMakeNewInvitation
  SetInvitationData id' inv -> onlyModify $ \state -> state { invitationId = id', invitation = inv }
  GoToOverview              -> noEffects
  GoToBabyStation           -> noEffects
  CopyToClipboard elemId    -> handleCopyToClipboard elemId
  ReportError err           -> noEffects
  Nop                       -> noEffects

makeInviteLink :: String -> Invitation -> String
makeInviteLink baseURL (Invitation inv) = baseURL <> "?" <> "acceptInvitation="
                                          <> gDefaultEncodeURLPiece inv.invitationSecret

handleInvitationSent :: forall ps. SentMethod -> ComponentType (Props ps) State Action
handleInvitationSent method = do
  state <- get :: Component (Props ps) State State
  modify $ _ { sentInvitation = Just $ { invitation : state.invitation , sentMethod : method } }
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
    pure $ InvitationSent (SentEmail state.email)

handleCopyToClipboard :: forall ps. String -> ComponentType (Props ps) State Action
handleCopyToClipboard elemId = pure [ do
                                         liftEff $ copyTextFromId elemId
                                         pure $ InvitationSent SentCopyPaste
                                    ]
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
                            <> " family, you only have to visit the"
                            <> " following one-time link on another device." ]
                    ]
            , div []
              [ div [ A.className "input-group"]
                [ input [ A.type_ "text", A.className "form-control", A.readOnly true
                        , A.value invitationLink
                        , A.id_ "invitationLinkUrlInput"
                        ] []
                ,  H.span [ A.className "input-group-btn"]
                   [ button [ A.className "btn btn-default"
                            , A.type_ "button"
                            , A.title "Generate new link ..."
                            , E.onClick $ const $ MakeNewInvitation
                            ]
                     [ span [ A.className "glyphicon glyphicon-repeat"] []
                     ]
                   ]
                ]
                , p [] [ br [] []
                       , text $ "You can either share it directly with WhatsApp or Telegram"
                         <> " or copy & paste it to send it via some other means:"
                  ]
            , H.div [ A.className "btn-group btn-group-justified" ]
              [ H.div [ A.className "btn-group" ]
                [ H.a [ A.className "btn btn-default"
                      , A.href $ "whatsapp://send?text=" <> escapedLink
                      , A.role "button"
                      , A.type_ "button"
                      , onClickWithDefault $ const $ InvitationSent SentWhatsApp
                      ]
                  [
                    -- H.img [ A.src "../static/pix/WhatsApp.svg", A.alt "WhatsApp" ] []
                    H.i [A.className "fa fa-fw fa-whatsapp"] []
                  , H.span [A.className "hidden-xs"] [H.text " WhatsApp"]
                  ]
                ]
              , H.div [ A.className "btn-group" ]
                [ H.a [ A.className "btn btn-default"
                      , A.href $ "tg://msg?text=" <> escapedLink -- "https://telegram.me/share/url?url=" <> escapedLink
                      , A.role "button"
                      , A.type_ "button"
                      , onClickWithDefault $ const $ InvitationSent SentTelegram
                      ]
                  [ H.i [A.className "fa fa-fw fa-telegram"] []
                  , H.span [A.className "hidden-xs"] [H.text " Telegram"]
                  ]
                ]
              , H.div [ A.className "btn-group" ]
                [ H.a [ A.className "btn btn-default"
                           , A.role "button"
                           , A.type_ "button"
                           , E.onClick $ const $ CopyToClipboard "invitationLinkUrlInput"
                           ]
                  [ H.span [ A.className "glyphicon glyphicon-copy"] []
                  , H.span [ A.className "hidden-xs"] [H.text " Copy to Clipboard"]
                  ]
                ]
              ]
            , br [] []
            , p []  [ text "Or you can let us send it via email …"
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
            ]
          ]
          , div [ A.className "container"
                , A.style [Tuple "width" "100%"]]
              [ br [] []
              , text "Invitation successfully transmitted? Then go back to overview or make this device a baby station right away .... "
              ]
          , nav []
            [ ul [ A.className "pager" ]
              [ li [ A.className "previous"
                    , E.onClick $ const $ GoToOverview
                    ] [a [] [ span [A.ariaHidden "true"] [text "← "]
                            , span [A.className "hidden-xs"] [text "Back to "]
                            , text "Overview"
                            ]
                      ]
              , li [ A.className "next"
                    , E.onClick $ const $ GoToBabyStation
                    ] [ a [] [ span [A.className "hidden-xs"] [text "Make this device a "]
                             , text "Baby Station"
                             , span [A.ariaHidden "true"] [text " →"]
                             ]
                      ]
              ]

            ]
          , viewSent props state
          ]
        ]
  where
    handleEnter :: E.KeyboardEvent -> Action
    handleEnter ev = if ev.keyCode == 13 then SendInvitation else Nop

viewSent :: forall ps. Props ps -> State -> Html Action
viewSent props state =
  case state.sentInvitation of
    Nothing -> H.span [] []
    Just sent ->
      let
        invLink = makeInviteLink (props.baseURL) sent.invitation
      in
        H.div [ A.className "alert alert-success"]
        [ viewSentMethod sent
        , H.a [ A.href invLink ]
          [ H.text "Oops - Did you send the invitation to the wrong recipient?"
          , br [] []
          , H.text "No problem! Just click me and then press 'Decline' to invalidate the invitation."
          ]
        ]

viewSentMethod :: SentInvitation -> Html Action
viewSentMethod sent = case sent.sentMethod of
  SentCopyPaste -> H.div []
                   [ H.h3 [] [ text $ "Copied invitation link to clipboard!" ]
                   , H.text "Now just paste it somewhere in order to transfer it to another device."
                   ]
  SentWhatsApp -> H.div []
                   [ H.h3 [] [ text $ "Sent link with WhatsApp!" ]
                   , H.text "That did not work out? Just copy and paste the link yourself!"
                   ]
  SentTelegram -> H.div []
                   [ H.h3 [] [ text $ "Sent link with Telegram!" ]
                   , H.text "That did not work out? Just copy and paste the link yourself!"
                   ]
  SentEmail addr -> H.div []
                   [ H.h3 [] [ text $ "Invitation successfully sent to '"  <> addr <> "'!" ]
                   , H.text "Did not receive the e-mail? Please check your spam folder!"
                   ]

foreign import copyTextFromId :: String -> Eff () Boolean
