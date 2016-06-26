module Gonimo.LocalStorage where


import Data.Generic (class Generic)

import Gonimo.WebAPI.Types as API



data Key a = AuthData


authData :: Key API.AuthData
authData = AuthData

derive instance genericKey :: Generic (Key a)
