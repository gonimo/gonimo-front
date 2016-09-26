module  Gonimo.UI.Home where

import Pux.Html.Attributes as A
import Pux.Html.Events as E
import Data.Tuple (Tuple(..))
import Gonimo.Client.Types (Settings, BabyStationInfo)
import Gonimo.Pux (noEffects, EffModel(EffModel))
import Gonimo.Server.DbEntities (Device)
import Gonimo.Types (Key)
import Gonimo.WebAPI.Types (DeviceInfo)
import Pux.Html (span, text, button, div, Html)
import Prelude hiding (div)


type Props ps = { settings :: Settings
                , babies :: Array (Tuple (Key Device) BabyStationInfo)
                , isBabyStation :: Boolean
                | ps }

type State = {
  lastBabyNames :: Array String
             }

init :: State
init = { lastBabyNames : [] }

data Action = StartBabyStation -- To be handled by parent
            | ConnectToBaby (Key Device)
            | SetLastBabyNames (Array String)


update :: forall eff ps. Props ps -> Action -> State -> EffModel eff State Action
update _ _ = noEffects


view :: State -> Html Action
view _ =
  div [ A.className "jumbotron" ]
  [ div [ A.className "container" ]
    [ button [ A.className "btn btn-block btn-info"
             , A.style [Tuple "margin-left" "0px"]
             , A.type_ "button"
             , E.onClick $ const $ StartBabyStation
             ]
             [ text "Start Baby Station"
             , span [A.className "glyphicon glyphicon-transfer"] []
             ]
    ]
  ]



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
