module Gonimo.Types where

import Data.Maybe (Maybe)
import Gonimo.Server.DbEntities (Invitation)
import Gonimo.Server.DbTypes (InvitationDelivery)
import Gonimo.Server.Types (Key, Secret)
import Prim (String)

import Data.Generic (class Generic)


data UserName =
    UserNameEmail String
  | UserNamePhone String

derive instance genericUserName :: Generic UserName

data Credentials =
    Credentials {
      userName :: UserName
    , userPassword :: String
    }

derive instance genericCredentials :: Generic Credentials

data AccountData =
    AccountData {
      credentials :: Maybe Credentials
    , secret :: AuthToken
    }

derive instance genericAccountData :: Generic AccountData

data AuthToken =
    GonimoSecret Secret

derive instance genericAuthToken :: Generic AuthToken

data SendInvitation =
    SendInvitation (Key Invitation) InvitationDelivery

derive instance genericSendInvitation :: Generic SendInvitation

