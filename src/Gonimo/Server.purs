module Gonimo.Server where

import Prelude

import Control.Bind
import Control.Monad.Aff (runAff)
import Control.Monad.Aff.Class
import Control.Monad.Aff.Console (log)
import qualified Control.Monad.Aff as Aff
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException, error, throw, Error)
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.Reader.Trans
import Data.Array as Arr
import Data.Either
import Data.Generic
import Data.Maybe
import Data.Tuple
import Network.HTTP.Affjax
import Network.HTTP.Affjax.Request
import Network.HTTP.Method
import Network.HTTP.StatusCode
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Argonaut.Aeson

import Gonimo.Server.Types

----------------------- Endpoints --------------------------------------
createAccount :: forall e. Maybe Credentials -> ServerT e MachineCredentials
createAccount = doRequest expect201 <=< basicArgRequest POST "accounts"

createInvitation :: forall e. FamilyId -> ServerT e (Tuple InvitationId Invitation)
createInvitation = doRequest expect201 <=< basicArgRequest POST "invitations"

acceptInvitation :: forall e. Secret -> ServerT e Invitation
acceptInvitation = doRequest expect200 <=< basicArgRequest DELETE "invitations"

-- instance isForeignInvitation :: IsForeign Invitation where
--  read = readGeneric defaultOptions

sendInvitation :: forall e. SendInvitation -> ServerT e MyUnit
sendInvitation = doRequest expect201 <=< basicArgRequest POST "invitationOutbox"

createFamily :: forall e. FamilyName -> ServerT e FamilyId
createFamily = doRequest expect201 <=< basicArgRequest POST "families"

----------------------- Helper functions --------------------------------

expectOne :: StatusCode -> StatusCode -> Boolean
expectOne = (==)

expect200 :: StatusCode -> Boolean
expect200 = expectOne (StatusCode 200)

expect201 :: StatusCode -> Boolean
expect201 = expectOne (StatusCode 201)

basicArgRequest :: forall m a. (MonadReader Config m, Generic a)
                => Method -> String -> a -> m ServerRequest
basicArgRequest m path= basicRequest m path <<< Just

basicEmptyRequest :: forall m. (MonadReader Config m)
                => Method -> String -> m ServerRequest
basicEmptyRequest m path= basicRequest m path (Nothing :: Maybe Int) -- Dummy type annotation

basicRequest :: forall m a. (MonadReader Config m, Generic a)
                => Method -> String -> Maybe a -> m ServerRequest
basicRequest m path c = do
  config <- ask
  return $ defaultRequest {
    method = m
  , url = config.baseUrl <> path
  , content = encodeServerJson <$> c
  , headers = config.headers
  , username = Nothing
  , password = Nothing
  }

doRequest :: forall e b. Generic b => (StatusCode -> Boolean) -> ServerRequest -> ServerT e b
doRequest checkStatus req = do
  res <- liftAff $ affjax req
  if checkStatus res.status
    then fromEither $ decodeServerJson res.response
    else throwError <<< error $ "Unexpected result code: "
          <> show res.status <> ", received message: " <> show res.response



fromEither :: forall m e a. (MonadError Error m, Show e) => Either e a -> m a
fromEither (Left e)  = throwError <<< error <<< show $ e
fromEither (Right v) = return v

encodeServerJson = gAesonEncodeJson
decodeServerJson = gAesonDecodeJson
