module Gonimo.UI.Socket.Serializer where

import Prelude
import Gonimo.Client.LocalStorage as Storage
import Control.Monad.Eff (Eff)
import DOM.WebStorage.Generic (getItem, setItem)
import DOM.WebStorage.Storage (STORAGE, ForeignStorage)
import Data.Maybe (Maybe(Just, Nothing))
import Gonimo.UI.Socket.Types (State)
import Gonimo.Util (differentObject)


store :: forall eff. ForeignStorage -> State -> Eff (storage :: STORAGE | eff) Unit
store storage state = case state.currentFamily of
  Nothing -> pure unit
  Just v  -> setItem storage Storage.currentFamily v

load :: forall eff. ForeignStorage -> State -> Eff (storage :: STORAGE | eff) State
load storage state = do
  mv <- getItem storage Storage.currentFamily
  pure $ case mv of
    Nothing -> state
    Just _ ->  state { currentFamily = mv }

needsWrite :: State -> State -> Boolean
needsWrite old new = differentObject old.currentFamily new.currentFamily
