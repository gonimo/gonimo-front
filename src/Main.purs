module Main where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Aff.Class
import Control.Monad.Aff.Console (log)
import qualified Control.Monad.Aff as Aff
import Control.Monad.Eff (Eff())
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

import Halogen
import Halogen.Util (appendToBody, onLoad)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import qualified Network.HTTP.Affjax as Ajax

import Gonimo.Server
import Gonimo.Server.Types
import Gonimo.LocalStorage
import Browser.LocalStorage

type Model = Unit


data Query a = Query a

helloC :: forall g . (Functor g) => Component Model Query g
helloC = component render eval
  where
    render ::  Model -> ComponentHTML Query
    render _ = H.span
                []
                [ H.text "Hello sailor!" ]
    eval :: Natural Query (ComponentDSL Model Query g)
    eval (Query next) = pure next


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

affMain :: forall eff. ServerT (storage :: STORAGE, console :: CONSOLE | eff ) Unit
affMain = do
  -- treq <- basicArgRequest POST "accounts" (Nothing :: Maybe Credentials)
  -- (resp :: AffjaxResponse String) <- liftAff $ affjax treq
  -- throwError $ error $ show resp.status <> ":" <> show resp.response
  account <- createAccount Nothing
  lift $ liftEff $ localStorage.setItem accountData account
  readAccount <- lift $ liftEff $ localStorage.getItem accountData
  throwError $ error $ gShow readAccount
  return unit
  {--
  lift $ log $ gShow readAccount
  --}
  -- Tuple aid token <- createAccount Nothing
  -- liftAff $ log $ show aid <> ":" <> show token

main = runAff throwException (const (pure unit))
  $ runReaderT affMain testConfig
