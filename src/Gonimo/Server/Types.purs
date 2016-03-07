module Gonimo.Server.Types where

import Network.HTTP.Affjax
import Network.HTTP.Affjax.Request
import Network.HTTP.RequestHeader
import Control.Monad.Aff
import Control.Monad.Reader.Class
import Control.Monad.Reader.Trans
import Data.Argonaut.Core
import Data.Generic


-- Used in the ServerT Reader monad for accessing common parameters
type Config = {
  baseUrl :: String
, headers :: Array RequestHeader
}

type ServerT e = ReaderT Config (Aff (ajax :: AJAX | e))
type ServerRequest = AffjaxRequest Json


type EmailAddress = String
type FamilyName = String

data UserName = UserNameEmail EmailAddress
  | UserNamePhone String
derive instance genericUserName :: Generic UserName


data AuthToken = GonimoSecret Secret
derive instance genericAuthToken :: Generic AuthToken

type Secret = String -- Good enough on the client side - no need to decode


newtype Invitation = Invitation {
  secret :: Secret
  , familyId :: FamilyId
  , created :: DateString -- We use a plain string to get proper JSON serialization/deserialization
  , delivery :: InvitationDelivery
}
derive instance genericInvitation :: Generic Invitation

data SendInvitation = SendInvitation InvitationId InvitationDelivery
derive instance genericSendInvitation :: Generic SendInvitation


data InvitationDelivery = EmailInvitation EmailAddress
                        | OtherDelivery
derive instance genericInvitationDelivery :: Generic InvitationDelivery

newtype Credentials = Credentials {
  userName :: UserName
, userPassword :: String
}
derive instance genericCredentials :: Generic Credentials


-- Ids

type DbId = Int

type FamilyId = DbId
type AccountId = DbId
type InvitationId = DbId
type DateString = String

newtype MyUnit = MyUnit {}

derive instance genericMyUnit :: Generic MyUnit
