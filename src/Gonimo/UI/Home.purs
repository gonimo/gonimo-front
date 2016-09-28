module  Gonimo.UI.Home where

import Pux.Html.Attributes as A
import Pux.Html.Elements as H
import Pux.Html.Events as E
import Data.Array (concat, fromFoldable)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Gonimo.Client.Types (Settings, BabyStationInfo)
import Gonimo.Pux (noEffects, EffModel(EffModel))
import Gonimo.Server.DbEntities (Family(Family), Device)
import Gonimo.Server.DbEntities.Helpers (runFamily)
import Gonimo.Server.Types (DeviceType(Baby, NoBaby))
import Gonimo.Types (Key)
import Gonimo.WebAPI.Types (DeviceInfo)
import Pux.Html (span, text, button, div, Html)
import Pux.Html.Attributes (letterSpacing)
import Prelude hiding (div)


type Props ps = { settings :: Settings
                , family :: Maybe Family
                , onlineStatus :: DeviceType
                | ps }

type State = { newBabyName :: String }

init :: State
init = { newBabyName : "baby" }

data Action = StartBabyStation String -- To be handled by parent
            | StopBabyStation  -- To be handled by parent
            | ConnectToBaby (Key Device) -- To be handled by parent
            | SetBabyName String -- Internal: keep track of user edit
            | Nop


update :: forall eff ps. Props ps -> Action -> State -> EffModel eff State Action
update _ (SetBabyName val)    = noEffects <<< _ { newBabyName = val }
update _ (StartBabyStation _) = noEffects
update _ StopBabyStation      = noEffects
update _ (ConnectToBaby _)    = noEffects
update _ Nop                  = noEffects

view :: forall ps. Props ps -> State -> Html Action
view props state = case props.onlineStatus of
  NoBaby -> viewOffline props state
  Baby name -> viewOnline name

viewOnline :: String -> Html Action
viewOnline name =
    H.div [ A.className "jumbotron" ]
    [ H.div [ A.className "container" ]
      [ H.h2 [] [ text $ "Baby monitor running for baby " <> name <> " ..." ]
      , button [ A.className "btn btn-block btn-danger"
               , A.style [Tuple "margin-left" "0px"]
               , A.type_ "button"
               , E.onClick $ const $ StopBabyStation
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
         , E.onClick $ const $ StartBabyStation baby
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
    handleEnter ev = if ev.keyCode == 13 then StartBabyStation name else Nop

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
