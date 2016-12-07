module Gonimo.Client.LocalStorage
       ( authData
       , currentFamily
       , videoEnabled
       , Key
       ) where


import Gonimo.Types as Gonimo
import Gonimo.WebAPI.Types as API
import Data.Generic (class Generic)
import Gonimo.Server.Db.Entities (Family())



data Key a = AuthData
           | CurrentFamily
           | VideoEnabled


authData :: Key API.AuthData
authData = AuthData

currentFamily :: Key (Gonimo.Key Family)
currentFamily = CurrentFamily

videoEnabled :: Key Boolean
videoEnabled = VideoEnabled

derive instance genericKey :: Generic (Key a)
