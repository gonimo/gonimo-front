module Gonimo.Server where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Aff.Console (log)
import qualified Control.Monad.Aff as Aff
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Data.Generic
import Network.HTTP.Affjax
import Data.Foreign.Generic

type FamilyId = Int
type EmailAddress = String

data Invitation = EmailInvitation EmailAddress

derive instance genericInvitation :: Generic Invitation

-- instance isForeignInvitation :: IsForeign Invitation where
--  read = readGeneric defaultOptions

sendInvitation :: forall e . FamilyId -> Invitation -> Affjax e Unit
sendInvitation fid inv = post "http://localhost:8081/1/" (toForeignGeneric inv)

