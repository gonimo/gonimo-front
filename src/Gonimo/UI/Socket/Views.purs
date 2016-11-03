module Gonimo.UI.Socket.Views (view, viewParentChannelThumb) where

import Prelude
import Data.Array as Arr
import Data.Map as Map
import Gonimo.Client.Effects as Console
import Gonimo.UI.Socket.Channel as ChannelC
import Gonimo.UI.Socket.Channel.Types as ChannelC
import Pux.Html.Attributes as A
import Pux.Html.Elements as H
import Pux.Html.Events as E
import WebRTC.MediaStream.Track as Track
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.IO (IO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReader)
import Control.Monad.Reader.Class (class MonadReader, ask)
import Control.Monad.State.Class (gets, modify, put, get, class MonadState)
import Data.Array (concat, fromFoldable)
import Data.Foldable (foldl)
import Data.Lens (use, to, (^?), (^.), _Just, (.=))
import Data.Map (Map)
import Data.Maybe (isNothing, isJust, maybe, fromMaybe, Maybe(Nothing, Just))
import Data.Monoid (mempty)
import Data.Profunctor (lmap)
import Data.Traversable (traverse_)
import Data.Tuple (uncurry, snd, Tuple(Tuple))
import Gonimo.Client.Types (toIO, Settings)
import Gonimo.Pux (wrapAction, noEffects, runGonimo, Component, liftChild, toParent, ComponentType, makeChildData, ToChild, onlyModify, Update, class MonadComponent)
import Gonimo.Server.DbEntities (Family(Family), Device(Device))
import Gonimo.Server.DbEntities.Helpers (runFamily)
import Gonimo.Types (Secret(Secret), Key(Key))
import Gonimo.UI.Socket.Internal (mkChannelProps, getParentChannels)
import Gonimo.UI.Socket.Lenses (streamURL, isAvailable, mediaStream, babyName, currentFamily, localStream, authData, newBabyName)
import Gonimo.UI.Socket.Message (decodeFromString)
import Gonimo.UI.Socket.Types (makeChannelId, toCSecret, toTheirId, ChannelId(ChannelId), channel, Props, State, Action(..))
import Gonimo.WebAPI (postSocketByFamilyIdByToDevice, deleteOnlineStatusByFamilyIdByDeviceId)
import Gonimo.WebAPI.Lenses (deviceId, _AuthData)
import Gonimo.WebAPI.Subscriber (receiveSocketByFamilyIdByToDevice, receiveSocketByFamilyIdByFromDeviceByToDeviceByChannelId)
import Gonimo.WebAPI.Types (AuthData(AuthData))
import Gonimo.WebAPI.Types.Helpers (runAuthData)
import Pux.Html (Html)
import Servant.Subscriber (Subscriptions)
import WebRTC.MediaStream (mediaStreamToBlob, createObjectURL, stopStream, MediaStreamConstraints(MediaStreamConstraints), getUserMedia, MediaStream, getTracks)
import WebRTC.RTC (RTCPeerConnection)

view :: forall ps. Props ps -> State -> Html Action
view props state = H.div [ A.className "jumbotron" ]
       [ if state.isAvailable
         then viewOnline props state
         else viewOffline props state
       ]

viewOffline :: forall ps. Props ps -> State -> Html Action
viewOffline props state =
    H.div []
    [ viewVideo props state
    , H.div [ A.className "videoContainer" ]
      [ viewStartButton state
      ]
    , viewBabyNameSelect props state
    ]

viewOnline :: forall ps. Props ps -> State -> Html Action
viewOnline props state =
  H.div [ A.className "videoContainer" ]
  [ if state.previewEnabled
    then H.div [ A.className "closableBox"]
         [ H.a [ A.className "boxclose"
                 , E.onClick $ const $ EnablePreview false
               ] []
         , viewVideo props state
         ]
    else H.div [ A.className "btn-group" ]
         [ H.button [ A.className "btn btn-default btn-block"
                    , E.onClick $ const $ EnablePreview true
                    , A.type_ "button"
                    ]
           [ H.text "Adjust camera"
           ]
         ]
    , viewStopButton state
  ]

viewVideo :: forall ps. Props ps -> State -> Html Action
viewVideo props state =
  H.div [ A.className "videoContainer" ]
  [ case state.streamURL of
        Nothing -> H.span [] []
        Just url -> H.video [ A.src url
                            , A.autoPlay "true"
                            , A.controls true
                            , A.muted true
                            , A.width "100%"
                            ] []
  ]

viewBabyNameSelect :: forall ps. Props ps -> State -> Html Action
viewBabyNameSelect props state =
  let
    lastBabies = concat
                 <<< fromFoldable -- Maybe Array -> Array Array
                 <<< map (_.familyLastUsedBabyNames <<< runFamily)
                 $ props.family

   in
     H.div [ A.role "group" ]
       [ H.h3 [] [ H.text "Select a name for your baby station"
                 , H.br [][]
                 , H.small [] [H.text "the last few names you used were:"]]
       , H.div [ A.className "list-group", A.role "group" ]
         $ (viewBabyButton state <$> lastBabies) <>
         [ H.a [ A.className $ "list-group-item "
                                   <> if state.newBabyName == state.babyName
                                       then "active"
                                       else "list-group-item-action"
               , E.onClick $ const (SetBabyName state.newBabyName) ]
             [ H.div [] [H.text "Or select a new name for this baby station "]
             , H.input [ A.type_ "text", A.className "form-control", A.placeholder "New baby name"
                   , E.onInput $ \ev -> SetNewBabyName ev.target.value
                   , A.value state.newBabyName ] []
             ]
         ]
       ]

viewBabyButton :: State -> String -> Html Action
viewBabyButton state baby =
   H.a [ A.className $ "list-group-item " <> if baby == state.babyName
                                               then "active"
                                               else "list-group-item-action"
       , E.onClick $ const $ SetBabyName baby
       ] [ H.text baby ]


viewStopButton :: State -> Html Action
viewStopButton state =
  H.div [ A.className "btn-group", A.role "group" ]
  [ H.h3 [] [ H.text $ "Baby monitor running for cute " <> state.babyName <> " …"]
  , H.button [ A.className "btn btn-block btn-danger"
              , A.style [ Tuple "margin-left" "0px" ]
              , A.type_ "button"
              , E.onClick $ const $ StopBabyStation
              ]
    [ H.text "Stop Baby Monitor "
    , H.span [A.className "glyphicon glyphicon-off"] []
    ]
  ]

viewStartButton :: State -> Html Action
viewStartButton state =
  H.div []
  [
    H.button [ A.className "btn btn-block btn-info"
             , A.style [ Tuple "margin-left" "0px" ]
             , A.disabled $ isNothing state.localStream
             , A.type_ "button"
             , E.onClick $ const $ StartBabyStation
             ]
    [ H.text $ "Start Baby Station for cute " <> state.babyName <> " "
    , H.span [A.className "glyphicon glyphicon-transfer"] []
    ]
  , H.div []
    $ if isNothing state.localStream
      then [ H.div [ A.className "alert alert-danger" ]
             [ H.p [] [ H.text "We need to enable your camera/microphone first." ]
             , H.button [ A.className "btn btn-block btn-default"
                        , A.style [ Tuple "margin-left" "0px" ]
                        , A.type_ "button"
                        , E.onClick $ const $ GetUserMedia
                        ]
               [ H.text "Enable camera/microphone!" ]
             ]
           ]
      else []
  ]

viewParentChannelThumb :: forall ps. Props ps -> State
                          -> Tuple ChannelId ChannelC.State -> Html Action
viewParentChannelThumb props state (Tuple chanId cState) =
  ChannelA chanId <$> ChannelC.view (mkChannelProps props state chanId) cState

viewParentChannels :: forall ps. Props ps -> State -> Array (Html Action)
viewParentChannels props state = let
    channels = getParentChannels state
  in
     viewParentChannelThumb props state <$> channels

getSubscriptions :: forall ps. Props ps -> State -> Subscriptions Action
getSubscriptions props state =
  let
    authData' = state ^. authData <<< to runAuthData
    familyId' = state ^. currentFamily
    receiveAChannel deviceId' familyId'' =
      receiveSocketByFamilyIdByToDevice
        (maybe Nop (AcceptConnection <<< uncurry makeChannelId ) <<< join)
        familyId''
        deviceId'

    getChannelSubscriptions :: forall m. (MonadReader Settings m)
                                  => ChannelId -> m (Subscriptions Action)
    getChannelSubscriptions channelId' =
      let
        cProps = mkChannelProps props state channelId'
      in
      map (ChannelA channelId') <$> ChannelC.getSubscriptions cProps

    getChannelsSubscriptions :: forall m. (MonadReader Settings m)
                                => Map ChannelId ChannelC.State
                                -> Array (m (Subscriptions Action))
    getChannelsSubscriptions = Arr.fromFoldable <<< map getChannelSubscriptions <<< Map.keys

    subArray :: Array (Subscriptions Action)
    subArray = map (flip runReader props.settings)
                $ getChannelsSubscriptions state.channels
                <> ( if state.isAvailable
                     then fromFoldable $ do
                               familyId'' <- familyId'
                               pure $ receiveAChannel authData'.deviceId familyId''
                     else []
                   )
  in
     foldl append mempty subArray
