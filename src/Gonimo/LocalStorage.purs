module Gonimo.LocalStorage where

import Browser.LocalStorage
import Data.Generic
import Data.Tuple
import Gonimo.Server.Types

data Key a = MachineCredentials

derive instance genericStorageData :: Generic (Key a)

machineCredentials :: Key MachineCredentials
machineCredentials = MachineCredentials
