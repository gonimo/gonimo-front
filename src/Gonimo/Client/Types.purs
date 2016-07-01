module Gonimo.Client.Types where

import Prelude
import Servant.PureScript.Affjax as Affjax
import Browser.LocalStorage (STORAGE)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff, class MonadAff)
import Control.Monad.Eff.Class (liftEff, class MonadEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except.Trans (class MonadError, ExceptT)
import Control.Monad.Reader.Class (local, ask, class MonadReader)
import Control.Monad.Reader.Trans (ReaderT)
import Data.Generic (class Generic)
import Gonimo.WebAPI (SPParams_)
import Network.HTTP.Affjax (AJAX)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (SPSettings_)
import Signal.Channel (CHANNEL)


type Settings = SPSettings_ SPParams_

data Error = AjaxError Affjax.AjaxError

derive instance genericError :: Generic Error

type EffEffects eff = ( ajax :: AJAX
                      , channel :: CHANNEL
                      , console :: CONSOLE
                      , storage :: STORAGE | eff
                      )

newtype EffectsT eff a = EffectsT (ReaderT Settings (ExceptT Error (Aff eff)) a)

type Effects eff = EffectsT (EffEffects eff)

runEffectsT :: forall eff a. EffectsT eff a -> (ReaderT Settings (ExceptT Error (Aff eff)) a)
runEffectsT (EffectsT m) = m


instance functorEffectsT :: Functor (EffectsT eff) where
  map f (EffectsT m) = EffectsT $ map f m

instance applyEffectsT :: Apply (EffectsT eff) where
  apply (EffectsT mf) (EffectsT ma) = EffectsT $ apply mf ma

instance applicativeEffectsT :: Applicative (EffectsT eff) where
  pure = EffectsT <<< pure

instance bindEffectsT :: Bind (EffectsT eff) where
  bind (EffectsT ma) mf = EffectsT $ bind ma (runEffectsT <<< mf)

instance monadEffectsT :: Monad (EffectsT eff)

instance monadReaderSettingsEffectsT :: MonadReader (SPSettings_ SPParams_) (EffectsT eff) where
  ask = EffectsT ask
  --local :: forall a. (r -> r) -> m a -> m a
  local f (EffectsT ma) = EffectsT (local f ma)

instance monadErrorErrorEffectsT :: MonadError Error (EffectsT eff) where
  throwError = EffectsT <<< throwError
  catchError (EffectsT ma) ef = EffectsT $ catchError ma (runEffectsT <<< ef)

instance monadErrorAjaxErrorEffectsT :: MonadError AjaxError (EffectsT eff) where
  throwError = EffectsT <<< throwError <<< AjaxError
  catchError (EffectsT ma) ef = EffectsT $ catchError ma (runEffectsT <<< handleAjax)
    where
      handleAjax (AjaxError err) = ef err
      --handleAjax err = throwError err

instance monadEffeffEffectsT :: MonadEff eff (EffectsT eff) where
  liftEff = EffectsT <<< liftEff

instance monadAffeffEffectsT :: MonadAff eff (EffectsT eff) where
  liftAff = EffectsT <<< liftAff
