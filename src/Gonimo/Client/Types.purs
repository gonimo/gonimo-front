module Gonimo.Client.Types where


import Control.Monad.Aff (Aff)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Reader.Trans (ReaderT)
import Gonimo.WebAPI (SPParams_)
import Network.HTTP.Affjax (AJAX)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (SPSettings_)
import Signal.Channel (CHANNEL)


type Settings = SPSettings_ SPParams_

type Effects eff = ReaderT Settings (ExceptT AjaxError (Aff ( ajax :: AJAX, channel :: CHANNEL | eff)))
