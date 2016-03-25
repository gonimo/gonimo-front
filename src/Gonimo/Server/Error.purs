module Gonimo.Server.Error where

import Data.Generic

data Error =
  InvalidAuthToken

derive instance genericError :: Generic Error
