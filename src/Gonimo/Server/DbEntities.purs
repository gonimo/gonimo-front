module Gonimo.Server.DbEntities where

import Gonimo.Server.DbTypes (InvitationDelivery)
import Gonimo.Server.Types (Date, Key, Secret)

import Data.Generic (class Generic)


data Invitation =
    Invitation {
      invitationSecret :: Secret
    , invitationFamilyId :: Key Family
    , invitationCreated :: Date
    , invitationDelivery :: InvitationDelivery
    }

derive instance genericInvitation :: Generic Invitation

