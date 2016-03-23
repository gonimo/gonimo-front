module Main where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Aff.Class
import Control.Monad.Aff.Console (log)
import qualified Control.Monad.Aff as Aff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (throwException, error)
import Control.Monad.Error.Class
import Control.Monad.Reader.Trans
import Data.Generic
import Data.Maybe
import Data.Tuple
import Data.Argonaut.Core

import Network.HTTP.Affjax
import Network.HTTP.Affjax.Request
import Network.HTTP.Method
import Network.HTTP.StatusCode

import qualified Network.HTTP.Affjax as Ajax

import Gonimo.Server
import Gonimo.Server.Types
import Gonimo.LocalStorage as Key
import Browser.LocalStorage


-- main :: forall eff . Eff (HalogenEffects eff) Unit
{-- main = runAff throwException (const (pure unit)) do
  app <- runUI helloC unit
  onLoad $ appendToBody app.node
  res <- Ajax.get "/users"
  log $ "Google result: " <> res.response
--}

testConfig =  {
  baseUrl : "http://localhost:8081/"
  , headers : []
}


getMachineCredentials :: forall e. ServerT (storage :: STORAGE | e) (Tuple AccountId AuthToken)
getMachineCredentials = do
    macc <- liftEff $ localStorage.getItem Key.machineCredentials
    case macc of
      Nothing -> createAndSave
      Just acc -> return acc
  where
    createAndSave = do
      acc <- createAccount Nothing
      liftEff $ localStorage.setItem Key.machineCredentials acc
      return acc


affMain :: forall eff. ServerT (storage :: STORAGE, console :: CONSOLE | eff ) Unit
affMain = do
  -- treq <- basicArgRequest POST "accounts" (Nothing :: Maybe Credentials)
  -- (resp :: AffjaxResponse String) <- liftAff $ affjax treq
  -- throwError $ error $ show resp.status <> ":" <> show resp.response

  liftAff $ log "Creating/getting an account ... "
  account <- getMachineCredentials
  liftAff $ log "Got account - juhu!"
  liftEff $ localStorage.setItem Key.machineCredentials account
  readAccount <- lift $ liftEff $ localStorage.getItem Key.machineCredentials
  liftAff $ log $ gShow readAccount
  return unit
  {--
  lift $ log $ gShow readAccount
  --}
  -- Tuple aid token <- createAccount Nothing
  -- liftAff $ log $ show aid <> ":" <> show token

main = runAff throwException (const (pure unit))
  $ runReaderT affMain testConfig
