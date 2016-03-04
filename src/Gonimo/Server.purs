module Gonimo.Server where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Aff.Console (log)
import qualified Control.Monad.Aff as Aff
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Data.Array as Arr
import Data.Generic
import Data.Maybe
import Data.Tuple
import Network.HTTP.Affjax
import Network.HTTP.Affjax.Request
import Data.Argonaut.Encode
import Data.Argonaut.Decode

serverBaseUrl :: String
serverBaseUrl = "http://localhost:8081/"

decodeServerJson = gAesonDecodeJson
encodeServerJson = gAesonEncodeJson

data ServerException =
  HttpError (AffjaxResponse String)
  | ConnectionError String

type FamilyId = Int
type EmailAddress = String

data AuthToken = GonimoSecret Secret
derive instance genericAuthToken :: Generic AuthToken

type Secret = String -- Good enough on the client side - no need to decode

type AccountId = Int
type InvitationId = Int

newtype Invitation = Invitation {
  secret :: Secret
  , familyId :: FamilyId
  , created :: String
}
derive instance genericInvitation :: Generic Invitation

newtype Credentials = Credentials {
  bla :: String
}

createAccountReq :: forall e. Maybe Credentials -> Tuple AccountId AuthToken
createAccountReq = doRequest expect201 <<< basicRequest POST "accounts"

createInvitation :: forall e. AuthToken -> FamilyId -> Affjax e (Tuple InvitationId Invitation)
createInvitation token = doRequest expect201 <<< addAuthHeader token
  <<< basicRequest POST "invitations"

acceptInvitation :: forall e. Secret -> Affjax e Invitation
acceptInvitation = doRequest expect200 <<< basicRequest DELETE "invitations"

-- instance isForeignInvitation :: IsForeign Invitation where
--  read = readGeneric defaultOptions

sendInvitation :: forall e. FamilyId -> Invitation -> Affjax e String
sendInvitation fid inv = post "http://localhost:8081/families/10/invitations" (gAesonEncodeJson inv)


expectOne :: StatusCode -> StatusCode -> Bool
expectOne = (==)

expect200 :: StatusCode -> Bool
expect200 = expectOne (StatusCode 200)

expect201 :: StatusCode -> Bool
expect201 = expectOne (StatusCode 201)


addAuthHeader :: AuthToken -> AffjaxRequest Json -> AffjaxRequest Json
addAuthHeader t = addHeader "Authorization" (toString t)

addHeader :: String -> String -> AffjaxRequest Json -> AffjaxRequestJson
addHeader name val r = r {
  headers = RequestHeader name val `Arr.cons` r.headers
}

basicRequest :: forall a. Generic a => Method -> String -> a -> AffjaxRequest Json
basicRequest checkStatus m path c = defaultRequest {
  method = m
  , url = serverBaseUrl <> path
  , content = encodeServerJson c
}

doRequest :: forall e a b. Generic a, Generic b => (StatusCode -> Bool) -> ServerRequest Json -> Aff e b
doRequest checkStatus req = do
  res <- affjax req
  if checkStatus res.status
  then fromEither (decodeServerJson res.response)
  else throwError <<< error $ "Unexpected result code: "
    <> show res.status <> ", received message: " <> res.response)



fromEither :: forall m e a. MonadError m, Show e => Either e a -> m a
fromEither (Left e)  = throw <<< show $ e
fromEither (Right v) = return v
