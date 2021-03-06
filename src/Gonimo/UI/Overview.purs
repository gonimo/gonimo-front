module  Gonimo.UI.Overview where

import Data.Array as Arr
import Data.Array as Array
import Data.Tuple as Tuple
import Gonimo.UI.Socket as SocketC
import Gonimo.UI.Socket.Channel.Types as ChannelC
import Gonimo.UI.Socket.Types as SocketC
import Pux.Html.Attributes as A
import Pux.Html.Elements as H
import Pux.Html.Events as E
import Control.Monad.Eff (Eff)
import Data.Array (concat, fromFoldable)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (uncurry, snd, fst, Tuple(..))
import Gonimo.Client.Types (Settings, BabyStationInfo)
import Gonimo.Pux (noEffects, onlyModify, Update)
import Gonimo.Server.Db.Entities (Family(Family), Device)
import Gonimo.Server.Db.Entities.Helpers (runFamily)
import Gonimo.Server.Types (DeviceType(Baby, NoBaby))
import Gonimo.Types (Key)
import Gonimo.WebAPI.Types (DeviceInfo(..))
import Prelude (class BooleanAlgebra)
import Pux.Html (span, text, button, div, Html)
import Pux.Html.Attributes (letterSpacing)
import Prelude hiding (div)


type Props ps = { settings      :: Settings
                , family        :: Maybe Family
                , deviceId      :: Key Device
                , onlineStatus  :: DeviceType
                , onlineDevices :: Array (Tuple (Key Device) DeviceType)
                , deviceInfos   :: Array (Tuple (Key Device) DeviceInfo)
                , socketS       :: SocketC.State
                , sendActionSocket :: SocketC.Action -> Eff () Unit -- Needed for socket operations
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

stopBabyStation :: Action
stopBabyStation = SocketA SocketC.StopBabyStation

update :: forall ps. Update (Props ps) State Action
update (SetBabyName val) = onlyModify $ _ { newBabyName = val }
update (SocketA _)       = noEffects
update GoToInviteView    = noEffects
update Nop               = noEffects

view :: forall ps. Props ps -> State -> Html Action
view props state = div []
                   [ viewConnectedBabies props state
                   , viewAvailableBabies props state
                   , case props.onlineStatus of
                       NoBaby -> div [] []
                       Baby name -> viewOnline name
                   , div [ A.className "btn-group" ]
                     [ button [ A.className "btn btn-lg btn-block btn-info"
                              , E.onClick $ const GoToInviteView ]
                       [ text "Add Device" ]
                     ]
                   ]

type OnlineBaby =
  { babyName :: String
  , deviceName :: String
  , deviceId :: Key Device
  }

viewConnectedBabies :: forall ps. Props ps -> State -> Html Action
viewConnectedBabies props state =
  let
    parentChannels = SocketC.getParentChannels props.socketS
  in
   if Array.null parentChannels
   then span [] []
   else div [ A.className "overviewThumbnails"]
        $ viewBabyThumbnail props <$> parentChannels

viewBabyThumbnail :: forall ps. Props ps -> Tuple SocketC.ChannelId ChannelC.State -> Html Action
viewBabyThumbnail props chan@(Tuple chanId _) =
  div [ A.className "overviewThumbnail closableBox" ]
  [ H.a [ A.className "boxclose"
        , E.onClick $ const $ SocketA (SocketC.CloseChannel chanId)
        ] []
  , SocketA <$> SocketC.viewParentChannelThumb props props.socketS chan
  ]



viewAvailableBabies :: forall ps. Props ps -> State -> Html Action
viewAvailableBabies props state =
    H.div [ A.className "jumbotron" ]
    [ H.div [ A.className "container" ]
      $ [ H.h2 [] [ text "Connect to baby: " ] ]
      <> if Arr.null onlineBabies
         then
           [ H.p []
             [ H.text "Sorry no baby stations online." ]
           ]
         else
            map viewOnlineBaby onlineBabies
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

        ids = map fst
              <<< Arr.filter (isBaby <<< snd)
              <<< Arr.filter ((_ /= props.deviceId) <<< fst)
              $ props.onlineDevices
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


-- wrappingBox :: (String -> Html Action) -> String -> Html Action
-- wrappingBox inner name = H.div [ E.onKeyUp handleEnter
--                                , A.style
--                                  [ Tuple "padding" "1em"
--                                  , Tuple "border-bottom" "1px solid #ccc"
--                                  ]
--                                ]
--                          [ inner name ]
  -- where
    -- handleEnter :: E.KeyboardEvent -> Action
    -- handleEnter ev = if ev.keyCode == 13 then startBabyStation name (MediaStreamConstraints { audio : true, video : true }) else Nop

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
