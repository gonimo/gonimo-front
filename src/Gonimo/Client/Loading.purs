module Gonimo.Client.Loading where

import Prelude
import Gonimo.Client.Effects as Gonimo
import Gonimo.Client.LocalStorage as Key
import Gonimo.Client.Types as Client
import Pux.Html.Attributes as A
import Pux.Html.Events as E
import Browser.LocalStorage (STORAGE, localStorage)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Either (Either(Right, Left))
import Data.Generic (gShow)
import Data.Maybe (Maybe(..))
import Gonimo.Client.Html (viewLogo)
import Gonimo.Client.Types (runEffectsT, Settings)
import Gonimo.Server.Types (AuthToken, AuthToken(GonimoSecret))
import Gonimo.Types (Secret(Secret))
import Gonimo.WebAPI (SPParams_(SPParams_), postAccounts)
import Gonimo.WebAPI.Types (AuthData(AuthData))
import Partial.Unsafe (unsafeCrashWith)
import Pux (noEffects, onlyEffects, EffModel, renderToDOM, fromSimple, start)
import Pux.Html (text, span, Html, img, div, button)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (defaultSettings, SPSettings_(SPSettings_))
import Signal (constant, Signal)

data State = Loading


type LoadedState = {
               authData :: AuthData
             , settings :: Settings
             }

data Action = Start
            | Init LoadedState
            | ReportError Client.Error
            | Nop


update :: forall eff. Action -> State -> EffModel State Action (Client.EffEffects eff)
update Start s                     = onlyEffects s [ init ]
update (ReportError error) Loading = onlyEffects Loading $ [ do Gonimo.error error
                                                                pure Nop
                                                          ]
update Nop Loading                 = noEffects Loading
update _ _                         = onlyEffects Loading $ [ do Gonimo.log "Loading can only handle Start, ReportError and Nop!"
                                                                unsafeCrashWith "Shit happens!"
                                   ]


view :: State -> Html Action
view _ = viewLogo $ div []
                      [ span [] [ text "Loading your gonimo, stay tight ..."]
                      , button [E.onClick (const Start)] [ text "Start it!"]
                      ]


init :: forall eff. Aff (Client.EffEffects eff) Action
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
  Gonimo.log $ "Got authdata from local storage: " <> gShow md
  case md of
    Nothing -> do
      auth <- postAccounts
      Gonimo.log $ "Got Nothing - called postAccounts and got: " <> gShow auth
      Gonimo.log $ "Calling setItem with : " <> gShow Key.authData
      liftEff $ localStorage.setItem Key.authData auth
      pure auth
    Just d  -> pure d
