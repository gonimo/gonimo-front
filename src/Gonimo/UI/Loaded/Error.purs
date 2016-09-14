module Gonimo.UI.Loaded.Error where

import Prelude hiding (div)
import Gonimo.Client.Effects as Gonimo
import Pux.Html.Attributes as A
import Pux.Html.Events as E
import Data.Either (Either(Left, Right), either)
import Data.Generic (gShow)
import Data.Maybe (Maybe(Just, Nothing))
import Gonimo.Pux (onlyEffects, onlyEffect, EffModel)
import Gonimo.Server.Error (ServerError(InvalidAuthToken))
import Gonimo.UI.Loaded.Types (State, Action(ResetDevice, Nop), UserError(Crashed, DeviceInvalid, NoError))
import Pux.Html (div, button, p, h1, text, Html)
import Servant.Subscriber.Connection (Notification(HttpRequestFailed, ParseError, WebSocketClosed, WebSocketError))
import Servant.Subscriber.Internal (doDecode)
import Servant.Subscriber.Request (HttpRequest)
import Servant.Subscriber.Response (HttpResponse(HttpResponse))
import Servant.Subscriber.Util (toUserType)



handleSubscriber :: forall eff. Notification -> State -> EffModel eff State Action
handleSubscriber notification state =
  case notification of
    WebSocketError msg           -> onlyEffect state $ do
      Gonimo.log $ "A websocket error occurred: " <> msg
      pure Nop
    WebSocketClosed msg          -> onlyEffect state $ do
      Gonimo.log $ "The websocket connection got closed: " <> msg
      pure Nop
    ParseError msg               -> onlyEffect state $ do
      Gonimo.log $ "A parsing error occurred: " <> msg
      pure Nop
    HttpRequestFailed req' resp' -> handleFailedHttpRequest state req' resp'

handleFailedHttpRequest :: forall eff. State -> HttpRequest -> HttpResponse -> EffModel eff State Action
handleFailedHttpRequest state req' fResp@(HttpResponse resp') = onlyEffects newState $ case decodingResult of
    Right _ -> []
    Left err -> [ do
                     Gonimo.log $ "Decoding error while handling failed http request: " <> err
                     Gonimo.log $ "For request: " <> gShow req'
                     Gonimo.log $ "Response was: " <> gShow fResp
                     pure Nop
                ]
  where
    newState = case error of
        NoError -> state
        _       -> state { userError = error }
    fromServer :: Maybe ServerError -> UserError
    fromServer mErr = case mErr of
        Nothing -> NoError
        Just InvalidAuthToken -> DeviceInvalid
        Just _ -> Crashed -- TODO: Check other errors as well.
    decodingResult = doDecode (toUserType fromServer) resp'.httpBody
    error = either (const Crashed) id decodingResult

viewError :: State -> Html Action
viewError state = case state.userError of
  NoError -> text "No error occurred - everything is fine!"
  DeviceInvalid ->
    div [ A.className "alert alert-danger", A.role "alert" ]
    [ h1 [] [ text "Your account is no longer valid!" ]
    , p []
      [ text  "We are sorry, but your account is no longer known by our server." ]
      , p []
        [ text
          $ "To proceed I would create a new account for you. You will have to rejoin any "
          <> "families you were in already."
        ]
    , p [] [ text "I am sorry for any inconveniences!" ]
    , div [ A.className "btn-group", A.role "group" ]
      [ button
        [ A.className "btn btn-danger btn-lg btn-block", E.onClick $ const ResetDevice ]
        [ text "Proceed"]
      ]
    ]
  Crashed ->
    div [ A.className "alert alert-danger", A.role "alert"]
    [ h1 [] [ text "Your gonimo has crashed!"]
    , p []
      [ text $ "Something unexpected happened - and I don't know what to do!"
            <> "Please try again later - hopefully we have fixed the problem then!"
      ]
    ]
