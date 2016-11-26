module Gonimo.Client.LocalStorage
       ( authData
       , currentFamily
       , Key
       ) where


import Gonimo.Types as Gonimo
import Gonimo.WebAPI.Types as API
import Data.Generic (class Generic)
import Gonimo.Server.Db.Entities (Family())



data Key a = AuthData
           | CurrentFamily


authData :: Key API.AuthData
authData = AuthData

currentFamily :: Key (Gonimo.Key Family)
currentFamily = CurrentFamily

derive instance genericKey :: Generic (Key a)
