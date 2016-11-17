module Gonimo.UI.Loaded.Central where

import Prelude
import Data.Array as Arr
import Gonimo.UI.AcceptInvitation as AcceptC
import Gonimo.UI.Invite as InviteC
import Data.Array (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Gonimo.UI.Loaded.Types (CentralReq(..), mkProps, Central(..), State)
import Gonimo.Util (class UserShow)

type CentralItem = { selected :: Boolean
                   , enabled :: Boolean
                   , req :: CentralReq
                   }

getCentrals :: State -> Array CentralItem
getCentrals state =
  let
    props = mkProps state
    currentCentral = centralToRequest state.central
    onlyInviteScreen = Arr.length state.deviceInfos <= 1
    reqs = [ ReqCentralOverview, ReqCentralBaby, ReqCentralInvite]
    toEntry req = { selected : (Just req == currentCentral)
                  , enabled : not onlyInviteScreen || req == ReqCentralInvite
                  , req : req
                  }
  in
   map toEntry reqs


centralToRequest :: Central -> Maybe CentralReq
centralToRequest c = case c of
  CentralInvite _ -> Just ReqCentralInvite
  CentralOverview -> Just ReqCentralOverview
  CentralBaby     -> Just ReqCentralBaby
  CentralAccept _ -> Nothing


