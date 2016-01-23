module Gonimo.Server where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Aff.Console (log)
import qualified Control.Monad.Aff as Aff
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Data.Generic
import Network.HTTP.Affjax
import Network.HTTP.Affjax.Request
import Data.Argonaut.Encode

type FamilyId = Int
type EmailAddress = String

data Invitation = EmailInvitation EmailAddress

derive instance genericInvitation :: Generic Invitation

-- instance isForeignInvitation :: IsForeign Invitation where
--  read = readGeneric defaultOptions

sendInvitation :: forall e . FamilyId -> Invitation -> Affjax e String
sendInvitation fid inv = post "http://localhost:8081/families/10/invitations" (gAesonEncodeJson inv)
