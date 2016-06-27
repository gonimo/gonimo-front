-- | Functions for getting the frontend up & running.
module Gonimo.Client.Init where

import Prelude
import Gonimo.Client.Types as Client
import Gonimo.LocalStorage as Key
import Pux.Html.Attributes as A
import Browser.LocalStorage (STORAGE, localStorage)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(..))
import Gonimo.Client.Types (runEffectsT, Settings)
import Gonimo.Server.Types (AuthToken, AuthToken(GonimoSecret))
import Gonimo.Types (Secret(Secret))
import Gonimo.WebAPI (SPParams_(SPParams_), postAccounts)
import Gonimo.WebAPI.Types (AuthData(AuthData))
import Partial.Unsafe (unsafeCrashWith)
import Pux (noEffects, onlyEffects, EffModel, renderToDOM, fromSimple, start)
import Pux.Html (text, span, Html, img, div)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (defaultSettings, SPSettings_(SPSettings_))
import Signal (constant, Signal)




type LoadedState = {
               authData :: AuthData
             , settings :: Settings
             }

data Action = Start
            | Init LoadedState
            | ReportError Client.Error

init :: forall eff. Aff (Client.Effects eff) Action
init = do
  let
    mkSettings :: AuthToken -> Settings
    mkSettings secret = defaultSettings $ SPParams_ {
          authorization : secret
        , baseURL : "http://localhost:8081/"
        }
    initSettings = mkSettings $ GonimoSecret (Secret "blabala")
  r <- runExceptT <<< flip runReaderT initSettings <<< runEffectsT $ getAuthData 
  case r of
    Left err -> pure $ ReportError err
    Right (authData@(AuthData auth)) -> pure $ Init { authData : authData
                                                    , settings : mkSettings auth.authToken
                                                    }

getAuthData :: forall eff. Client.Effects eff AuthData
getAuthData = do
  md <- liftEff $ localStorage.getItem Key.authData
  case md of
    Nothing -> do
      auth <- postAccounts
      liftEff $ localStorage.setItem Key.authData auth
      pure auth
    Just d  -> pure d
