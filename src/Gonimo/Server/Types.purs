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


newtype Date = DateTime String

derive instance genericDate :: Generic Date

newtype Secret = Secret String

derive instance genericSecret :: Generic Secret

data Key a = Int

derive instance genericKey :: Generic (Key a)

data Family = Family -- Dummy type for Key Family
