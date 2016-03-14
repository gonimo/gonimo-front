module Gonimo.LocalStorage where

import Browser.LocalStorage
import Data.Generic
import Data.Tuple
import Gonimo.Server.Types

data StorageData a =
  AccountData

derive instance genericStorageData :: Generic (StorageData a)

accountData :: StorageData (Tuple AccountId AuthToken)
accountData = AccountData
