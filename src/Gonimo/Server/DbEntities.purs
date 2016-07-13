-- File auto generated by purescript-bridge! --
module Gonimo.Server.DbEntities where

import Data.Maybe (Maybe)
import Gonimo.Server.Types (AuthToken, InvitationDelivery)
import Gonimo.Types (Date, Family, Key, Secret)
import Prim (String)

import Data.Generic (class Generic)


data Account =
    Account {
      accountCreated :: Date
    }

derive instance genericAccount :: Generic Account

data Client =
    Client {
      clientName :: String
    , clientAuthToken :: AuthToken
    , clientAccountId :: Key Account
    , clientLastAccessed :: Date
    , clientUserAgent :: String
    }

derive instance genericClient :: Generic Client

data Invitation =
    Invitation {
      invitationSecret :: Secret
    , invitationFamilyId :: Key Family
    , invitationCreated :: Date
    , invitationDelivery :: InvitationDelivery
    , invitationSenderId :: Key Client
    , invitationReceiverId :: Maybe (Key Account)
    }

derive instance genericInvitation :: Generic Invitation

