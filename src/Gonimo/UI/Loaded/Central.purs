module Gonimo.UI.Loaded.Central where

import Prelude
import Gonimo.UI.AcceptInvitation as AcceptC
import Gonimo.UI.Invite as InviteC
import Data.Array (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Gonimo.UI.Loaded.Types (CentralReq(..), mkProps, Central(..), State)
import Gonimo.Util (class UserShow)

getCentrals :: State -> Array (Tuple Boolean CentralReq)
getCentrals state =
  let
    props = mkProps state
    currentCentral = centralToRequest state.central
    reqs = [ ReqCentralOverview, ReqCentralBaby, ReqCentralInvite]
    toEntry req = Tuple (Just req == currentCentral) req
  in
   map toEntry reqs


centralToRequest :: Central -> Maybe CentralReq
centralToRequest c = case c of
  CentralInvite _ -> Just ReqCentralInvite
  CentralOverview -> Just ReqCentralOverview
  CentralBaby     -> Just ReqCentralBaby
  CentralAccept _ -> Nothing


