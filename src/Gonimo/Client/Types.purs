module Gonimo.Client.Types where

import Prelude
import Servant.PureScript.Affjax as Affjax
import Browser.LocalStorage (STORAGE)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff, class MonadAff)
import Control.Monad.Eff.Class (liftEff, class MonadEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (Error, EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except.Trans (runExceptT, class MonadError, ExceptT)
import Control.Monad.IO (IO)
import Control.Monad.Reader.Class (local, ask, class MonadReader)
import Control.Monad.Reader.Trans (runReaderT, ReaderT)
import Data.Either (Either(Right, Left), either)
import Data.Generic (class Generic)
import Gonimo.WebAPI (SPParams_)
import Gonimo.WebAPI.Types (DeviceInfo)
import Network.HTTP.Affjax (AJAX)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (SPSettings_)
import Signal.Channel (CHANNEL)
import WebSocket (WEBSOCKET)


type Settings = SPSettings_ SPParams_

data GonimoError = AjaxError Affjax.AjaxError
                 | UnexpectedAction String

-- | Action type to use when errors should be handled by the parent component.
data ReportParent action = ReportError GonimoError
                         | Child action

-- | Action types that can be used for error reporting:
class ReportErrorAction action where
  reportError :: GonimoError -> action

instance reportErrorActionReportParent :: ReportErrorAction (ReportParent action) where
  reportError = ReportError

newtype Gonimo a = Gonimo (ReaderT Settings (ExceptT GonimoError IO) a)

-- We have unwrapGonimo in Gonimo.Pux, so call it unwrap
unwrapGonimo :: forall a. Gonimo a -> (ReaderT Settings (ExceptT GonimoError IO) a)
unwrapGonimo (Gonimo m) = m

toIO :: forall action. ReportErrorAction action
        => Settings -> Gonimo action -> IO action
toIO settings m = map (either reportError id) <<< runExceptT <<< flip runReaderT settings <<< unwrapGonimo $ m

instance functorGonimo :: Functor (Gonimo) where
  map f (Gonimo m) = Gonimo $ map f m

instance applyGonimo :: Apply (Gonimo) where
  apply (Gonimo mf) (Gonimo ma) = Gonimo $ apply mf ma

instance applicativeGonimo :: Applicative (Gonimo) where
  pure = Gonimo <<< pure

instance bindGonimo :: Bind (Gonimo) where
  bind (Gonimo ma) mf = Gonimo $ bind ma (unwrapGonimo <<< mf)

instance monadGonimo :: Monad (Gonimo)

instance monadReaderSettingsGonimo :: MonadReader (SPSettings_ SPParams_) (Gonimo) where
  ask = Gonimo ask
  --local :: forall a. (r -> r) -> m a -> m a
  local f (Gonimo ma) = Gonimo (local f ma)

instance monadErrorErrorGonimo :: MonadError GonimoError (Gonimo) where
  throwError = Gonimo <<< throwError
  catchError (Gonimo ma) ef = Gonimo $ catchError ma (unwrapGonimo <<< ef)

instance monadErrorAjaxErrorGonimo :: MonadError AjaxError (Gonimo) where
  throwError = Gonimo <<< throwError <<< AjaxError
  catchError (Gonimo ma) ef = Gonimo $ catchError ma (unwrapGonimo <<< handleAjax)
    where
      handleAjax (AjaxError err) = ef err
      handleAjax err = throwError err

instance monadEffeffGonimo :: MonadEff eff (Gonimo) where
  liftEff = Gonimo <<< liftEff

instance monadAffeffGonimo :: MonadAff eff (Gonimo) where
  liftAff = Gonimo <<< liftAff


-- | Info about an online baby station.
type BabyStationInfo =
  { deviceInfo :: DeviceInfo
  , babyName :: String
  }
