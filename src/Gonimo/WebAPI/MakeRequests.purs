-- File auto generated by servant-purescript! --
module Gonimo.WebAPI.MakeRequests where

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
import Gonimo.Server.Db.Entities (Account, Device, Family, Invitation)
import Gonimo.Server.State.Types (MessageNumber, SessionId)
import Gonimo.Server.Types (AuthToken, Coffee, DeviceType)
import Gonimo.Types (Key, Secret)
import Gonimo.WebAPI (SPParams_(..))
import Gonimo.WebAPI.Types (AuthData, DeviceInfo, InvitationInfo, InvitationReply, SendInvitation)
import Network.HTTP.Affjax (AJAX)
import Prelude (Unit)
import Prim (Array, String)
import Servant.PureScript.Affjax (AjaxError(..), affjax, defaultRequest)
import Servant.PureScript.Settings (SPSettings_(..), gDefaultToURLPiece)
import Servant.PureScript.Util (encodeHeader, encodeListQuery, encodeQueryItem, encodeURLPiece, getResult)
import Servant.Subscriber (ToUserType)
import Servant.Subscriber.Request (HttpRequest(..))
import Servant.Subscriber.Subscriptions (Subscriptions, makeSubscriptions)
import Servant.Subscriber.Types (Path(..))
import Servant.Subscriber.Util (TypedToUser, subGenFlagQuery, subGenListQuery, subGenNormalQuery, toUserType)

postAccounts :: forall m. MonadReader (SPSettings_ SPParams_) m => m HttpRequest
postAccounts = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let baseURL = spParams_.baseURL
  let httpMethod = "POST"
  let reqPath = Path ["accounts"]
  let reqHeaders =
        []
  let reqQuery =
        []
  let spReq = HttpRequest
                { httpMethod: httpMethod
                , httpPath: reqPath
                , httpHeaders: reqHeaders
                , httpQuery: reqQuery
                , httpBody: ""
                }
  pure spReq

postInvitationsByFamilyId :: forall m. MonadReader (SPSettings_ SPParams_) m =>
                             Key Family -> m HttpRequest
postInvitationsByFamilyId familyId = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let authorization = spParams_.authorization
  let baseURL = spParams_.baseURL
  let httpMethod = "POST"
  let reqPath = Path ["invitations" , gDefaultToURLPiece familyId]
  let reqHeaders =
        [Tuple "Authorization" (gDefaultToURLPiece authorization)]
  let reqQuery =
        []
  let spReq = HttpRequest
                { httpMethod: httpMethod
                , httpPath: reqPath
                , httpHeaders: reqHeaders
                , httpQuery: reqQuery
                , httpBody: ""
                }
  pure spReq

deleteInvitationsByInvitationSecret :: forall m.
                                       MonadReader (SPSettings_ SPParams_) m =>
                                       InvitationReply -> Secret
                                       -> m HttpRequest
deleteInvitationsByInvitationSecret reqBody invitationSecret = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let authorization = spParams_.authorization
  let baseURL = spParams_.baseURL
  let httpMethod = "DELETE"
  let reqPath = Path ["invitations" , gDefaultToURLPiece invitationSecret]
  let reqHeaders =
        [Tuple "Authorization" (gDefaultToURLPiece authorization)]
  let reqQuery =
        []
  let spReq = HttpRequest
                { httpMethod: httpMethod
                , httpPath: reqPath
                , httpHeaders: reqHeaders
                , httpQuery: reqQuery
                , httpBody: printJson <<< encodeJson $ reqBody
                }
  pure spReq

postInvitationsOutbox :: forall m. MonadReader (SPSettings_ SPParams_) m =>
                         SendInvitation -> m HttpRequest
postInvitationsOutbox reqBody = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let authorization = spParams_.authorization
  let baseURL = spParams_.baseURL
  let httpMethod = "POST"
  let reqPath = Path ["invitations" , "outbox"]
  let reqHeaders =
        [Tuple "Authorization" (gDefaultToURLPiece authorization)]
  let reqQuery =
        []
  let spReq = HttpRequest
                { httpMethod: httpMethod
                , httpPath: reqPath
                , httpHeaders: reqHeaders
                , httpQuery: reqQuery
                , httpBody: printJson <<< encodeJson $ reqBody
                }
  pure spReq

putInvitationsInfoByInvitationSecret :: forall m.
                                        MonadReader (SPSettings_ SPParams_) m =>
                                        Secret -> m HttpRequest
putInvitationsInfoByInvitationSecret invitationSecret = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let authorization = spParams_.authorization
  let baseURL = spParams_.baseURL
  let httpMethod = "PUT"
  let reqPath = Path ["invitations" , "info"
        , gDefaultToURLPiece invitationSecret]
  let reqHeaders =
        [Tuple "Authorization" (gDefaultToURLPiece authorization)]
  let reqQuery =
        []
  let spReq = HttpRequest
                { httpMethod: httpMethod
                , httpPath: reqPath
                , httpHeaders: reqHeaders
                , httpQuery: reqQuery
                , httpBody: ""
                }
  pure spReq

getAccountsByAccountIdFamilies :: forall m.
                                  MonadReader (SPSettings_ SPParams_) m =>
                                  Key Account -> m HttpRequest
getAccountsByAccountIdFamilies accountId = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let authorization = spParams_.authorization
  let baseURL = spParams_.baseURL
  let httpMethod = "GET"
  let reqPath = Path ["accounts" , gDefaultToURLPiece accountId , "families"]
  let reqHeaders =
        [Tuple "Authorization" (gDefaultToURLPiece authorization)]
  let reqQuery =
        []
  let spReq = HttpRequest
                { httpMethod: httpMethod
                , httpPath: reqPath
                , httpHeaders: reqHeaders
                , httpQuery: reqQuery
                , httpBody: ""
                }
  pure spReq

postFamilies :: forall m. MonadReader (SPSettings_ SPParams_) m => String
                -> m HttpRequest
postFamilies reqBody = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let authorization = spParams_.authorization
  let baseURL = spParams_.baseURL
  let httpMethod = "POST"
  let reqPath = Path ["families"]
  let reqHeaders =
        [Tuple "Authorization" (gDefaultToURLPiece authorization)]
  let reqQuery =
        []
  let spReq = HttpRequest
                { httpMethod: httpMethod
                , httpPath: reqPath
                , httpHeaders: reqHeaders
                , httpQuery: reqQuery
                , httpBody: printJson <<< encodeJson $ reqBody
                }
  pure spReq

getFamiliesByFamilyId :: forall m. MonadReader (SPSettings_ SPParams_) m =>
                         Key Family -> m HttpRequest
getFamiliesByFamilyId familyId = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let authorization = spParams_.authorization
  let baseURL = spParams_.baseURL
  let httpMethod = "GET"
  let reqPath = Path ["families" , gDefaultToURLPiece familyId]
  let reqHeaders =
        [Tuple "Authorization" (gDefaultToURLPiece authorization)]
  let reqQuery =
        []
  let spReq = HttpRequest
                { httpMethod: httpMethod
                , httpPath: reqPath
                , httpHeaders: reqHeaders
                , httpQuery: reqQuery
                , httpBody: ""
                }
  pure spReq

getFamiliesByFamilyIdDeviceInfos :: forall m.
                                    MonadReader (SPSettings_ SPParams_) m =>
                                    Key Family -> m HttpRequest
getFamiliesByFamilyIdDeviceInfos familyId = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let authorization = spParams_.authorization
  let baseURL = spParams_.baseURL
  let httpMethod = "GET"
  let reqPath = Path ["families" , gDefaultToURLPiece familyId , "deviceInfos"]
  let reqHeaders =
        [Tuple "Authorization" (gDefaultToURLPiece authorization)]
  let reqQuery =
        []
  let spReq = HttpRequest
                { httpMethod: httpMethod
                , httpPath: reqPath
                , httpHeaders: reqHeaders
                , httpQuery: reqQuery
                , httpBody: ""
                }
  pure spReq

postSocketByFamilyIdByToDevice :: forall m.
                                  MonadReader (SPSettings_ SPParams_) m =>
                                  Key Device -> Key Family -> Key Device
                                  -> m HttpRequest
postSocketByFamilyIdByToDevice reqBody familyId toDevice = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let authorization = spParams_.authorization
  let baseURL = spParams_.baseURL
  let httpMethod = "POST"
  let reqPath = Path ["socket" , gDefaultToURLPiece familyId
        , gDefaultToURLPiece toDevice]
  let reqHeaders =
        [Tuple "Authorization" (gDefaultToURLPiece authorization)]
  let reqQuery =
        []
  let spReq = HttpRequest
                { httpMethod: httpMethod
                , httpPath: reqPath
                , httpHeaders: reqHeaders
                , httpQuery: reqQuery
                , httpBody: printJson <<< encodeJson $ reqBody
                }
  pure spReq

getSocketByFamilyIdByToDevice :: forall m. MonadReader (SPSettings_ SPParams_) m
                                 => Key Family -> Key Device -> m HttpRequest
getSocketByFamilyIdByToDevice familyId toDevice = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let authorization = spParams_.authorization
  let baseURL = spParams_.baseURL
  let httpMethod = "GET"
  let reqPath = Path ["socket" , gDefaultToURLPiece familyId
        , gDefaultToURLPiece toDevice]
  let reqHeaders =
        [Tuple "Authorization" (gDefaultToURLPiece authorization)]
  let reqQuery =
        []
  let spReq = HttpRequest
                { httpMethod: httpMethod
                , httpPath: reqPath
                , httpHeaders: reqHeaders
                , httpQuery: reqQuery
                , httpBody: ""
                }
  pure spReq

deleteSocketByFamilyIdByToDeviceByFromDeviceByChannelId :: forall m.
                                                           MonadReader (SPSettings_ SPParams_) m
                                                           => Key Family
                                                           -> Key Device
                                                           -> Key Device
                                                           -> Secret
                                                           -> m HttpRequest
deleteSocketByFamilyIdByToDeviceByFromDeviceByChannelId familyId toDevice
                                                        fromDevice
                                                        channelId = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let authorization = spParams_.authorization
  let baseURL = spParams_.baseURL
  let httpMethod = "DELETE"
  let reqPath = Path ["socket" , gDefaultToURLPiece familyId
        , gDefaultToURLPiece toDevice , gDefaultToURLPiece fromDevice
        , gDefaultToURLPiece channelId]
  let reqHeaders =
        [Tuple "Authorization" (gDefaultToURLPiece authorization)]
  let reqQuery =
        []
  let spReq = HttpRequest
                { httpMethod: httpMethod
                , httpPath: reqPath
                , httpHeaders: reqHeaders
                , httpQuery: reqQuery
                , httpBody: ""
                }
  pure spReq

putSocketByFamilyIdByFromDeviceByToDeviceByChannelId :: forall m.
                                                        MonadReader (SPSettings_ SPParams_) m
                                                        => Array String
                                                        -> Key Family
                                                        -> Key Device
                                                        -> Key Device -> Secret
                                                        -> m HttpRequest
putSocketByFamilyIdByFromDeviceByToDeviceByChannelId reqBody familyId fromDevice
                                                     toDevice channelId = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let authorization = spParams_.authorization
  let baseURL = spParams_.baseURL
  let httpMethod = "PUT"
  let reqPath = Path ["socket" , gDefaultToURLPiece familyId
        , gDefaultToURLPiece fromDevice , gDefaultToURLPiece toDevice
        , gDefaultToURLPiece channelId]
  let reqHeaders =
        [Tuple "Authorization" (gDefaultToURLPiece authorization)]
  let reqQuery =
        []
  let spReq = HttpRequest
                { httpMethod: httpMethod
                , httpPath: reqPath
                , httpHeaders: reqHeaders
                , httpQuery: reqQuery
                , httpBody: printJson <<< encodeJson $ reqBody
                }
  pure spReq

getSocketByFamilyIdByFromDeviceByToDeviceByChannelId :: forall m.
                                                        MonadReader (SPSettings_ SPParams_) m
                                                        => Key Family
                                                        -> Key Device
                                                        -> Key Device -> Secret
                                                        -> m HttpRequest
getSocketByFamilyIdByFromDeviceByToDeviceByChannelId familyId fromDevice
                                                     toDevice channelId = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let authorization = spParams_.authorization
  let baseURL = spParams_.baseURL
  let httpMethod = "GET"
  let reqPath = Path ["socket" , gDefaultToURLPiece familyId
        , gDefaultToURLPiece fromDevice , gDefaultToURLPiece toDevice
        , gDefaultToURLPiece channelId]
  let reqHeaders =
        [Tuple "Authorization" (gDefaultToURLPiece authorization)]
  let reqQuery =
        []
  let spReq = HttpRequest
                { httpMethod: httpMethod
                , httpPath: reqPath
                , httpHeaders: reqHeaders
                , httpQuery: reqQuery
                , httpBody: ""
                }
  pure spReq

deleteSocketByFamilyIdByFromDeviceByToDeviceByChannelIdMessagesByMessageNumber :: forall m.
                                                                                  MonadReader (SPSettings_ SPParams_) m
                                                                                  =>
                                                                                  Key Family
                                                                                  -> Key Device
                                                                                  -> Key Device
                                                                                  -> Secret
                                                                                  -> MessageNumber
                                                                                  -> m HttpRequest
deleteSocketByFamilyIdByFromDeviceByToDeviceByChannelIdMessagesByMessageNumber familyId
                                                                               fromDevice
                                                                               toDevice
                                                                               channelId
                                                                               messageNumber = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let authorization = spParams_.authorization
  let baseURL = spParams_.baseURL
  let httpMethod = "DELETE"
  let reqPath = Path ["socket" , gDefaultToURLPiece familyId
        , gDefaultToURLPiece fromDevice , gDefaultToURLPiece toDevice
        , gDefaultToURLPiece channelId , "messages"
        , gDefaultToURLPiece messageNumber]
  let reqHeaders =
        [Tuple "Authorization" (gDefaultToURLPiece authorization)]
  let reqQuery =
        []
  let spReq = HttpRequest
                { httpMethod: httpMethod
                , httpPath: reqPath
                , httpHeaders: reqHeaders
                , httpQuery: reqQuery
                , httpBody: ""
                }
  pure spReq

postSessionByFamilyIdByDeviceId :: forall m.
                                   MonadReader (SPSettings_ SPParams_) m =>
                                   DeviceType -> Key Family -> Key Device
                                   -> m HttpRequest
postSessionByFamilyIdByDeviceId reqBody familyId deviceId = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let authorization = spParams_.authorization
  let baseURL = spParams_.baseURL
  let httpMethod = "POST"
  let reqPath = Path ["session" , gDefaultToURLPiece familyId
        , gDefaultToURLPiece deviceId]
  let reqHeaders =
        [Tuple "Authorization" (gDefaultToURLPiece authorization)]
  let reqQuery =
        []
  let spReq = HttpRequest
                { httpMethod: httpMethod
                , httpPath: reqPath
                , httpHeaders: reqHeaders
                , httpQuery: reqQuery
                , httpBody: printJson <<< encodeJson $ reqBody
                }
  pure spReq

putSessionByFamilyIdByDeviceIdBySessionId :: forall m.
                                             MonadReader (SPSettings_ SPParams_) m
                                             => DeviceType -> Key Family
                                             -> Key Device -> SessionId
                                             -> m HttpRequest
putSessionByFamilyIdByDeviceIdBySessionId reqBody familyId deviceId
                                          sessionId = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let authorization = spParams_.authorization
  let baseURL = spParams_.baseURL
  let httpMethod = "PUT"
  let reqPath = Path ["session" , gDefaultToURLPiece familyId
        , gDefaultToURLPiece deviceId , gDefaultToURLPiece sessionId]
  let reqHeaders =
        [Tuple "Authorization" (gDefaultToURLPiece authorization)]
  let reqQuery =
        []
  let spReq = HttpRequest
                { httpMethod: httpMethod
                , httpPath: reqPath
                , httpHeaders: reqHeaders
                , httpQuery: reqQuery
                , httpBody: printJson <<< encodeJson $ reqBody
                }
  pure spReq

deleteSessionByFamilyIdByDeviceIdBySessionId :: forall m.
                                                MonadReader (SPSettings_ SPParams_) m
                                                => Key Family -> Key Device
                                                -> SessionId -> m HttpRequest
deleteSessionByFamilyIdByDeviceIdBySessionId familyId deviceId sessionId = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let authorization = spParams_.authorization
  let baseURL = spParams_.baseURL
  let httpMethod = "DELETE"
  let reqPath = Path ["session" , gDefaultToURLPiece familyId
        , gDefaultToURLPiece deviceId , gDefaultToURLPiece sessionId]
  let reqHeaders =
        [Tuple "Authorization" (gDefaultToURLPiece authorization)]
  let reqQuery =
        []
  let spReq = HttpRequest
                { httpMethod: httpMethod
                , httpPath: reqPath
                , httpHeaders: reqHeaders
                , httpQuery: reqQuery
                , httpBody: ""
                }
  pure spReq

getSessionByFamilyId :: forall m. MonadReader (SPSettings_ SPParams_) m =>
                        Key Family -> m HttpRequest
getSessionByFamilyId familyId = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let authorization = spParams_.authorization
  let baseURL = spParams_.baseURL
  let httpMethod = "GET"
  let reqPath = Path ["session" , gDefaultToURLPiece familyId]
  let reqHeaders =
        [Tuple "Authorization" (gDefaultToURLPiece authorization)]
  let reqQuery =
        []
  let spReq = HttpRequest
                { httpMethod: httpMethod
                , httpPath: reqPath
                , httpHeaders: reqHeaders
                , httpQuery: reqQuery
                , httpBody: ""
                }
  pure spReq

postFunnyName :: forall m. MonadReader (SPSettings_ SPParams_) m =>
                 m HttpRequest
postFunnyName = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let baseURL = spParams_.baseURL
  let httpMethod = "POST"
  let reqPath = Path ["funnyName"]
  let reqHeaders =
        []
  let reqQuery =
        []
  let spReq = HttpRequest
                { httpMethod: httpMethod
                , httpPath: reqPath
                , httpHeaders: reqHeaders
                , httpQuery: reqQuery
                , httpBody: ""
                }
  pure spReq

getCoffee :: forall m. MonadReader (SPSettings_ SPParams_) m => m HttpRequest
getCoffee = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let baseURL = spParams_.baseURL
  let httpMethod = "GET"
  let reqPath = Path ["coffee"]
  let reqHeaders =
        []
  let reqQuery =
        []
  let spReq = HttpRequest
                { httpMethod: httpMethod
                , httpPath: reqPath
                , httpHeaders: reqHeaders
                , httpQuery: reqQuery
                , httpBody: ""
                }
  pure spReq

