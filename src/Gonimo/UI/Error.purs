module Gonimo.UI.Error ( class ErrorAction
                       , resetDevice
                       , nop
                       , clearError
                       , UserError(..)
                       , handleError
                       , handleSubscriber
                       , viewError ) where

import Data.Array as Arr
import Data.Unfoldable as Unfoldable
import Gonimo.Client.Effects as Gonimo
import Gonimo.Server.Error as Server
import Pux.Html.Attributes as A
import Pux.Html.Events as E
import Servant.PureScript.Affjax as Affjax
import Control.Monad.IO (IO)
import Control.Monad.State.Class (modify, class MonadState)
import Data.Either (Either(Left, Right), either)
import Data.Generic (gShow)
import Data.Maybe (fromMaybe, maybe, Maybe(Just, Nothing))
import Gonimo.Client.Types (GonimoError(RegisterSessionFailed, AjaxError, UnexpectedAction))
import Gonimo.Server.Error (ServerError)
import Pux.Html (div, button, p, h1, text, Html)
import Pux.Html.Attributes (offset)
import Servant.PureScript.Affjax (requestToString, ErrorDescription(ConnectionError), responseToString, unsafeToString, ErrorDescription(DecodingError, ParsingError, UnexpectedHTTPStatus))
import Servant.Subscriber.Connection (Notification(HttpRequestFailed, ParseError, WebSocketClosed, WebSocketOpened, WebSocketError))
import Servant.Subscriber.Internal (doDecode)
import Servant.Subscriber.Request (HttpRequest)
import Servant.Subscriber.Response (HttpResponse(HttpResponse))
import Servant.Subscriber.Util (toUserType)
import Prelude hiding (div)

type State r = { userError :: UserError | r }

class ErrorAction action where
  resetDevice :: action
  clearError :: action
  nop :: action

-- | Errors which are of concern for the user and should be displayed.
data UserError = NoError
               | DeviceInvalid -- Server does not accept our device secret.
               | AlreadyFamilyMember
               | NoSuchFamily
               | FamilyNotOnline
               | NoSuchInvitation
               | InvitationAlreadyClaimed
               | ConnectionFailed


handleError :: forall eff r m action. ( ErrorAction action, MonadState (State r) m )
               => GonimoError -> m (Array (IO action))
handleError err = case err of
  UnexpectedAction msg -> logError "An unexpected action occurred: " msg
  RegisterSessionFailed err -> handleAjaxError err
  AjaxError err -> handleAjaxError err

handleAjaxError :: forall r m action. ( ErrorAction action, MonadState (State r) m )
                   => Affjax.AjaxError -> m (Array (IO action))
handleAjaxError (Affjax.AjaxError err) = case err.description of
  ParsingError msg -> logError "Parsing error in Ajax request: " msg
  DecodingError msg -> logError "Decoding error in Ajax request: " msg
  ConnectionError msg -> do
    modify $ setError ConnectionFailed
    logError "Ajax request failed: " msg
  UnexpectedHTTPStatus response ->
    handleEncodedServerError (requestToString err.request) (responseToString response) response.response


handleSubscriber :: forall r m action. ( ErrorAction action, MonadState (State r) m )
                    => Notification -> m (Array (IO action))
handleSubscriber notification =
  case notification of
    WebSocketError msg           -> logError "A websocket error occurred: " msg
    WebSocketOpened              -> logError "WebSocket connection established!" ""
    WebSocketClosed msg          -> logError "The websocket connection got closed: " msg
    ParseError msg               -> logError "A parsing error occurred: " msg
    HttpRequestFailed req' resp' -> handleFailedHttpRequest req' resp'

handleFailedHttpRequest :: forall r m action. ( ErrorAction action, MonadState (State r) m )
                           => HttpRequest -> HttpResponse -> m (Array (IO action))
handleFailedHttpRequest req' fResp@(HttpResponse resp') =
  handleEncodedServerError (gShow req') (gShow fResp) resp'.httpBody

handleEncodedServerError :: forall r m action. ( ErrorAction action, MonadState (State r) m )
                            => String -> String -> String -> m (Array (IO action))
handleEncodedServerError req' resp' value = do
  modify updateState
  pure [ case decodingResult of
            Right _ ->  do
              Gonimo.log "Got server error!"
              Gonimo.log $ "For request: " <> req'
              Gonimo.log $ "Response was: " <> resp'
              pure nop

            Left err ->  do
              Gonimo.log $ "Decoding error while handling failed http request: " <> err
              Gonimo.log $ "For request: " <> req'
              Gonimo.log $ "Response was: " <> resp'
              pure nop
       ]
  where
    updateState state = case error of
        NoError -> state
        _       -> setError error state
    fromServer = fromMaybe NoError <<< ( fromServerError =<< _ )
    decodingResult = doDecode (toUserType fromServer) value
    error = either (const NoError) id decodingResult


viewError :: forall r action. ErrorAction action
             => State r -> Html action
viewError state = case state.userError of
  NoError -> text "No error occurred - everything is fine!"
  DeviceInvalid -> errorView (Just resetDevice) "Your account is no longer valid"
    $ div []
      [ p []
        [ text  "We are sorry, but your account is no longer known by our server." ]
      , p []
        [ text
          $ "To proceed I would create a new account for you. You will have to rejoin any "
          <> "families you were in already."
        ]
      , p [] [ text "I am sorry for any inconveniences!" ]
      ]
  AlreadyFamilyMember -> errorView (Just clearError) "You are already a member of this family!"
    $ div []
      [ p [] [ text "You cannot join a family you are already a member of."]
      , p [] [ text $ "If you want to invite another device, please send another invitation."
                    <>"The link you just used, is now invalid! (Security, you know ... )"
             ]
      ]
  ConnectionFailed -> errorView (Just clearError) "No Internet Connection?"
    $ div []
      [ p [] [ text "We cannot reach our server."]
      , p [] [ text "Please make sure that you have a working internet connection and try again."
             ]
      ]
  NoSuchFamily -> errorView (Just clearError) "Your family no longer exists!"
    $ div []
      [ p [] [ text "It probably got deleted, we are sorry about that."]
      ]
  FamilyNotOnline -> errorView (Just clearError) "Your family is not online!"
    $ div []
      [ p [] [ text "This should not happen - like ever, if it happens, this is a bug. It can also happen if you open gonimo on multiple tabs in the same browser - which currently is not prevented (also a shortcoming of the current version)."]
      ]
  InvitationAlreadyClaimed -> errorView (Just clearError) "This invitation is already claimed!"
    $ div []
      [ p [] [ text "Another device already claimed this invitation."]
      , p [] [ text $ "Only this other device is now able to accept or decline the invitation."
             ]
      , p [] [ text "Security and stuff ...." ]
      ]
  NoSuchInvitation -> errorView (Just clearError) "This invitation no longer exists!"
    $ div []
      [ p [] [ text "For security reasons, invitations are only valid once."]
      , p [] [ text $ "Please send a new invitation from a device already in the family you were about to join."
             ]
      ]

errorView :: forall action. ErrorAction action
             => Maybe action -> String -> Html action -> Html action
errorView = errorView' "Proceed"

errorView' :: forall action. ErrorAction action
             => String -> Maybe action -> String -> Html action -> Html action
errorView' btnText action heading body =
    div [ A.className "alert alert-danger", A.role "alert" ]
    $ [ h1 [] [ text heading ]
      , body
      ]
      <> Arr.fromFoldable (map renderButton action)
  where
    renderButton :: action -> Html action
    renderButton action =
      div [ A.className "btn-group", A.role "group" ]
      [ button
        [ A.className "btn btn-danger btn-lg btn-block", E.onClick $ const action ]
        [ text btnText ]
      ]

logError :: forall action m. (ErrorAction action, Applicative m)
                 => String -> String -> m (Array (IO action))
logError prefix msg = pure [ do
                              Gonimo.log $ prefix <> msg
                              pure nop
                           ]

fromServerError :: ServerError -> Maybe UserError
fromServerError err = case err of
    Server.InvalidAuthToken         -> Just DeviceInvalid
    Server.AlreadyFamilyMember      -> Just AlreadyFamilyMember
    Server.NoSuchFamily _           -> Just NoSuchFamily
    Server.FamilyNotOnline _        -> Just FamilyNotOnline
    Server.InvitationAlreadyClaimed -> Just InvitationAlreadyClaimed
    Server.NoSuchInvitation         -> Just NoSuchInvitation
    _                               -> Nothing

-- | Sets userError in state but only if it was NoError before. (Does not overwrite an existing error)
setError :: forall r. UserError -> (State r) -> (State r)
setError error state = case state.userError of
  NoError -> state { userError = error }
  _ -> state
