module Gonimo.Server.DbTypes where

import Prim (String)

import Data.Generic (class Generic)


data InvitationDelivery =
    EmailInvitation String
  | OtherDelivery 

derive instance genericInvitationDelivery :: Generic InvitationDelivery

