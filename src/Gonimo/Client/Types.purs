module Gonimo.Client.Types where

import Prelude
import Servant.PureScript.Affjax as Affjax
import Browser.LocalStorage (STORAGE)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff, class MonadAff)
import Control.Monad.Eff.Class (liftEff, class MonadEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except.Trans (runExceptT, class MonadError, ExceptT)
import Control.Monad.Reader.Class (local, ask, class MonadReader)
import Control.Monad.Reader.Trans (runReaderT, ReaderT)
import Data.Either (Either(Right, Left), either)
import Data.Generic (class Generic)
import Gonimo.WebAPI (SPParams_)
import Network.HTTP.Affjax (AJAX)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (SPSettings_)
import Signal.Channel (CHANNEL)


type Settings = SPSettings_ SPParams_

data Error = AjaxError Affjax.AjaxError
           | URLRouteError String

derive instance genericError :: Generic Error

-- | Action types that can be used for error reporting:
class ReportErrorAction action where
  reportError :: Error -> action

type GonimoEff eff = ( ajax :: AJAX
                      , channel :: CHANNEL
                      , console :: CONSOLE
                      , storage :: STORAGE | eff
                      )

newtype GonimoT eff a = GonimoT (ReaderT Settings (ExceptT Error (Aff eff)) a)

type Gonimo eff = GonimoT (GonimoEff eff)

runGonimoT :: forall eff a. GonimoT eff a -> (ReaderT Settings (ExceptT Error (Aff eff)) a)
runGonimoT (GonimoT m) = m

toAff :: forall eff action. ReportErrorAction action
         => Settings -> Gonimo eff action -> Aff (GonimoEff eff) action
toAff settings m = do
  map (either reportError id) <<< runExceptT <<< flip runReaderT settings <<< runGonimoT $ m

instance functorGonimoT :: Functor (GonimoT eff) where
  map f (GonimoT m) = GonimoT $ map f m

instance applyGonimoT :: Apply (GonimoT eff) where
  apply (GonimoT mf) (GonimoT ma) = GonimoT $ apply mf ma

instance applicativeGonimoT :: Applicative (GonimoT eff) where
  pure = GonimoT <<< pure

instance bindGonimoT :: Bind (GonimoT eff) where
  bind (GonimoT ma) mf = GonimoT $ bind ma (runGonimoT <<< mf)

instance monadGonimoT :: Monad (GonimoT eff)

instance monadReaderSettingsGonimoT :: MonadReader (SPSettings_ SPParams_) (GonimoT eff) where
  ask = GonimoT ask
  --local :: forall a. (r -> r) -> m a -> m a
  local f (GonimoT ma) = GonimoT (local f ma)

instance monadErrorErrorGonimoT :: MonadError Error (GonimoT eff) where
  throwError = GonimoT <<< throwError
  catchError (GonimoT ma) ef = GonimoT $ catchError ma (runGonimoT <<< ef)

instance monadErrorAjaxErrorGonimoT :: MonadError AjaxError (GonimoT eff) where
  throwError = GonimoT <<< throwError <<< AjaxError
  catchError (GonimoT ma) ef = GonimoT $ catchError ma (runGonimoT <<< handleAjax)
    where
      handleAjax (AjaxError err) = ef err
      handleAjax err = throwError err

instance monadEffeffGonimoT :: MonadEff eff (GonimoT eff) where
  liftEff = GonimoT <<< liftEff

instance monadAffeffGonimoT :: MonadAff eff (GonimoT eff) where
  liftAff = GonimoT <<< liftAff
