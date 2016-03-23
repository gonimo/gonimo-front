module Gonimo.Server.Types where

import Prelude
import Network.HTTP.Affjax
import Network.HTTP.Affjax.Request
import Network.HTTP.RequestHeader
import Control.Monad.Aff
import Control.Monad.Reader.Class
import Control.Monad.Reader.Trans
import Data.Argonaut.Core
import Data.Tuple
import Data.Maybe
import Data.Generic
import Control.Monad.Eff.Console (CONSOLE)


-- Used in the ServerT Reader monad for accessing common parameters
type Config = {
  baseUrl :: String
, headers :: Array RequestHeader
}

type ServerT e = ReaderT Config (Aff (ajax :: AJAX | e))
type ServerRequest = AffjaxRequest Json


type EmailAddress = String
type FamilyName = String

type MachineCredentials = Tuple AccountId AuthToken

data UserName = UserNameEmail EmailAddress
  | UserNamePhone String
derive instance genericUserName :: Generic UserName

data AuthToken = GonimoSecret Secret
derive instance genericAuthToken :: Generic AuthToken
instance showAuthToken :: Show AuthToken where
  show = gShow

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

data MyUnit = MyUnit

instance genericMyUnit :: Generic MyUnit where
  toSpine _ = SArray []
  toSignature _ = SigArray (\_ -> SigInt)
  fromSpine (SArray []) = Just MyUnit
  fromSpine _ = Nothing
