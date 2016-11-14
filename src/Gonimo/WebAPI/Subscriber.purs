-- File auto generated by servant-purescript! --
module Gonimo.WebAPI.Subscriber where

import Prelude

import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader.Class (ask, class MonadReader)
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Argonaut.Printer (printJson)
import Data.Maybe (Maybe, Maybe(..))
import Data.Nullable (Nullable(), toNullable)
import Data.Tuple (Tuple, Tuple(..))
import Global (encodeURIComponent)
import Gonimo.Server.Db.Entities (Account, Device, Family)
import Gonimo.Server.State.Types (MessageNumber, SessionId)
import Gonimo.Server.Types (AuthToken, DeviceType)
import Gonimo.Types (Key, Secret)
import Gonimo.WebAPI (SPParams_(..))
import Gonimo.WebAPI.Types (DeviceInfo)
import Network.HTTP.Affjax (AJAX)
import Prim (Array, String)
import Servant.PureScript.Affjax (AjaxError(..), affjax, defaultRequest)
import Servant.PureScript.Settings (SPSettings_(..), gDefaultToURLPiece)
import Servant.PureScript.Util (encodeHeader, encodeListQuery, encodeQueryItem, encodeURLPiece, getResult)
import Servant.Subscriber (ToUserType)
import Servant.Subscriber.Request (HttpRequest(..))
import Servant.Subscriber.Subscriptions (Subscriptions, makeSubscriptions)
import Servant.Subscriber.Types (Path(..))
import Servant.Subscriber.Util (TypedToUser, subGenFlagQuery, subGenListQuery, subGenNormalQuery, toUserType)

import Gonimo.WebAPI.MakeRequests as MakeRequests

getAccountsByAccountIdFamilies :: forall m a.
                                  MonadReader (SPSettings_ SPParams_) m =>
                                  TypedToUser (Array (Key Family)) a
                                  -> Key Account -> m (Subscriptions a)
getAccountsByAccountIdFamilies spToUser_ accountId = do
  spReq <- MakeRequests.getAccountsByAccountIdFamilies accountId
  pure $ makeSubscriptions spReq (toUserType spToUser_)

getFamiliesByFamilyId :: forall m a. MonadReader (SPSettings_ SPParams_) m =>
                         TypedToUser Family a -> Key Family
                         -> m (Subscriptions a)
getFamiliesByFamilyId spToUser_ familyId = do
  spReq <- MakeRequests.getFamiliesByFamilyId familyId
  pure $ makeSubscriptions spReq (toUserType spToUser_)

getFamiliesByFamilyIdDeviceInfos :: forall m a.
                                    MonadReader (SPSettings_ SPParams_) m =>
                                    TypedToUser (Array (Tuple (Key Device) DeviceInfo)) a
                                    -> Key Family -> m (Subscriptions a)
getFamiliesByFamilyIdDeviceInfos spToUser_ familyId = do
  spReq <- MakeRequests.getFamiliesByFamilyIdDeviceInfos familyId
  pure $ makeSubscriptions spReq (toUserType spToUser_)

getSocketByFamilyIdByToDevice :: forall m a.
                                 MonadReader (SPSettings_ SPParams_) m =>
                                 TypedToUser (Maybe (Tuple (Key Device) Secret)) a
                                 -> Key Family -> Key Device
                                 -> m (Subscriptions a)
getSocketByFamilyIdByToDevice spToUser_ familyId toDevice = do
  spReq <- MakeRequests.getSocketByFamilyIdByToDevice familyId toDevice
  pure $ makeSubscriptions spReq (toUserType spToUser_)

getSocketByFamilyIdByFromDeviceByToDeviceByChannelId :: forall m a.
                                                        MonadReader (SPSettings_ SPParams_) m
                                                        =>
                                                        TypedToUser (Maybe (Tuple MessageNumber String)) a
                                                        -> Key Family
                                                        -> Key Device
                                                        -> Key Device -> Secret
                                                        -> m (Subscriptions a)
getSocketByFamilyIdByFromDeviceByToDeviceByChannelId spToUser_ familyId
                                                     fromDevice toDevice
                                                     channelId = do
  spReq <- MakeRequests.getSocketByFamilyIdByFromDeviceByToDeviceByChannelId familyId
                                                                             fromDevice
                                                                             toDevice
                                                                             channelId
  pure $ makeSubscriptions spReq (toUserType spToUser_)

postSessionByFamilyIdByDeviceId :: forall m a.
                                   MonadReader (SPSettings_ SPParams_) m =>
                                   TypedToUser SessionId a -> DeviceType
                                   -> Key Family -> Key Device
                                   -> m (Subscriptions a)
postSessionByFamilyIdByDeviceId spToUser_ reqBody familyId deviceId = do
  spReq <- MakeRequests.postSessionByFamilyIdByDeviceId reqBody familyId
                                                        deviceId
  pure $ makeSubscriptions spReq (toUserType spToUser_)

getSessionByFamilyId :: forall m a. MonadReader (SPSettings_ SPParams_) m =>
                        TypedToUser (Array (Tuple (Key Device) DeviceType)) a
                        -> Key Family -> m (Subscriptions a)
getSessionByFamilyId spToUser_ familyId = do
  spReq <- MakeRequests.getSessionByFamilyId familyId
  pure $ makeSubscriptions spReq (toUserType spToUser_)

