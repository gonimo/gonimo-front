module  Gonimo.UI.Home where

import Data.Array as Arr
import Data.Tuple as Tuple
import Gonimo.UI.Socket.Types as SocketC
import Pux.Html.Attributes as A
import Pux.Html.Elements as H
import Pux.Html.Events as E
import Data.Array (concat, fromFoldable)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (uncurry, snd, fst, Tuple(..))
import Gonimo.Client.Types (Settings, BabyStationInfo)
import Gonimo.Pux (noEffects, onlyModify, Update)
import Gonimo.Server.DbEntities (Family(Family), Device)
import Gonimo.Server.DbEntities.Helpers (runFamily)
import Gonimo.Server.Types (DeviceType(Baby, NoBaby))
import Gonimo.Types (Key)
import Gonimo.WebAPI.Types (DeviceInfo(..))
import Prelude (class BooleanAlgebra)
import Pux.Html (span, text, button, div, Html)
import Pux.Html.Attributes (letterSpacing)
import WebRTC.MediaStream (MediaStreamConstraints(MediaStreamConstraints))
import Prelude hiding (div)


type Props ps = { settings      :: Settings
                , family        :: Maybe Family
                , onlineStatus  :: DeviceType
                , onlineDevices :: Array (Tuple (Key Device) DeviceType)
                , deviceInfos   :: Array (Tuple (Key Device) DeviceInfo)
                | ps }

type State = { newBabyName :: String }

init :: State
init = { newBabyName : "baby" }

data Action = SocketA SocketC.Action
            | SetBabyName String -- Internal: keep track of user edit
            | GoToInviteView
            | Nop


connectToBaby :: Key Device -> Action
connectToBaby = SocketA <<< SocketC.ConnectToBaby

startBabyStation :: String -> MediaStreamConstraints -> Action
startBabyStation name constraints = SocketA $ SocketC.StartBabyStation name constraints

stopBabyStation :: Action
stopBabyStation = SocketA SocketC.StopBabyStation

update :: forall ps. Update (Props ps) State Action
update (SetBabyName val) = onlyModify $ _ { newBabyName = val }
update (SocketA _)       = noEffects
update GoToInviteView    = noEffects
update Nop               = noEffects

view :: forall ps. Props ps -> State -> Html Action
view props state = div []
                   [
                     button [ A.className "btn btn-lg"
                            , E.onClick $ const GoToInviteView ]
                     [ text "Add Device" ]
                   , viewAvailableBabies props state
                   ,
                     case props.onlineStatus of
                       NoBaby -> viewOffline props state
                       Baby name -> viewOnline name
                   ]

type OnlineBaby =
  { babyName :: String
  , deviceName :: String
  , deviceId :: Key Device
  }

viewAvailableBabies :: forall ps. Props ps -> State -> Html Action
viewAvailableBabies props state =
    H.div [ A.className "jumbotron" ]
    [ H.div [ A.className "container" ]
      $ [ H.h2 [] [ text "Connect to baby: " ] ]
      <> map viewOnlineBaby onlineBabies
    ]
  where
    makeOnlineBaby :: Key Device -> String -> String -> OnlineBaby
    makeOnlineBaby deviceId deviceName babyName = { deviceId : deviceId
                                                  , deviceName : deviceName
                                                  , babyName : babyName
                                                  }
    onlineBabies :: Array OnlineBaby
    onlineBabies =
      let
        isBaby :: DeviceType -> Boolean
        isBaby t = case t of
          NoBaby -> false
          Baby _ -> true

        getBabyName :: DeviceType -> Maybe String
        getBabyName NoBaby = Nothing
        getBabyName (Baby n) = Just n

        ids = map fst <<< Arr.filter (isBaby <<< snd) $ props.onlineDevices
        babyNames = Arr.concatMap (fromFoldable <<< getBabyName <<< snd) props.onlineDevices
        deviceInfos = Arr.concatMap (fromFoldable <<< flip Tuple.lookup props.deviceInfos) ids
        deviceNames = map (\(DeviceInfo info) -> info.deviceInfoName) deviceInfos

      in
       Arr.zipWith (uncurry makeOnlineBaby) (Arr.zip ids deviceNames) babyNames

viewOnlineBaby :: OnlineBaby -> Html Action
viewOnlineBaby baby =
  button [ A.className "btn btn-block btn-info"
         , A.style [Tuple "margin-left" "0px"]
         , A.type_ "button"
         , E.onClick $ const $ connectToBaby baby.deviceId
         ]
  [ text $ baby.babyName <> " "
  , span [A.className "glyphicon glyphicon-transfer"] []
  , H.p []
    [ H.small [] [ text $ "on device: " <> baby.deviceName ]
    ]
  ]

viewOnline :: String -> Html Action
viewOnline name =
    H.div [ A.className "jumbotron" ]
    [ H.div [ A.className "container" ]
      [ H.h2 [] [ text $ "Baby monitor running for baby " <> name <> " ..." ]
      , button [ A.className "btn btn-block btn-danger"
               , A.style [Tuple "margin-left" "0px"]
               , A.type_ "button"
               , E.onClick $ const $ stopBabyStation
               ]
        [ text "Stop Baby Monitor "
        , span [A.className "glyphicon glyphicon-off"] []
        ]
      ]
    ]

viewOffline :: forall ps. Props ps -> State -> Html Action
viewOffline props state =
  let
    lastBabies = concat
                 <<< fromFoldable -- Maybe Array -> Array Array
                 <<< map (_.familyLastUsedBabyNames <<< runFamily)
                 $ props.family
  in
    H.div [ A.className "jumbotron" ]
    [ H.div [ A.className "container" ]
      $ [ H.h2 [] [ text "Start baby monitor for ..." ]
        ]
        <> map (wrappingBox viewStartButton) lastBabies
        <> [ wrappingBox viewStartNewBaby state.newBabyName ]
    ]

viewStartButton :: String -> Html Action
viewStartButton baby =
  button [ A.className "btn btn-block btn-info"
         , A.style [Tuple "margin-left" "0px"]
         , A.type_ "button"
         , E.onClick $ const $ startBabyStation baby
                                 (MediaStreamConstraints { video : true, audio : true })
         ]
  [ text $ baby <> " "
  , span [A.className "glyphicon glyphicon-transfer"] []
  ]

viewStartNewBaby :: String -> Html Action
viewStartNewBaby name =
  div []
  [ H.label [ A.htmlFor "newBabyNameInput" ] [ text "Baby Name: " ]
  , H.input [ A.type_ "text", A.className "formControl"
            , E.onInput $ \ev -> SetBabyName ev.target.value
            , A.value name
            , A.id_ "newBabyNameInput"
            ] []
  , viewStartButton name
  ]

wrappingBox :: (String -> Html Action) -> String -> Html Action
wrappingBox inner name = H.div [ E.onKeyUp handleEnter
                               , A.style
                                 [ Tuple "padding" "1em"
                                 , Tuple "border-bottom" "1px solid #ccc"
                                 ]
                               ]
                         [ inner name ]
  where
    handleEnter :: E.KeyboardEvent -> Action
    handleEnter ev = if ev.keyCode == 13 then startBabyStation name (MediaStreamConstraints { audio : true, video : true }) else Nop

-- getSubscriptions :: forall ps. Props ps -> State -> Subscriptions Action
-- getSubscriptions props _ =
--   let
--     familyId = fst <$> head state.families -- Currently we just pick the first family
--     subArray :: Array (Subscriptions Action)
--     subArray = map (flip runReader (mkSettings state.authData)) <<< concat $
--       [ [ Sub.getAccountsByAccountIdFamilies (maybe Nop SetFamilies)
--                                           (runAuthData state.authData).accountId
--         ]
--       , fromFoldable $
--         Sub.getOnlineStatusByFamilyId (maybe Nop SetOnlineDevices) <$> familyId
--       , fromFoldable $
--         Sub.getFamiliesByFamilyIdDeviceInfos (maybe Nop SetDeviceInfos) <$> familyId
--       ]
--   in
--      foldl append mempty subArray
