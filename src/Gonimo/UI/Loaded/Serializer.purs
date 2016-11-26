module Gonimo.UI.Loaded.Serializer where

import Prelude
import Gonimo.UI.Socket.Serializer as SocketS
import Control.Monad.Eff (Eff)
import DOM.WebStorage.Storage (STORAGE, ForeignStorage)
import Gonimo.UI.Loaded.Types (State)

store :: forall eff. ForeignStorage -> State -> Eff (storage :: STORAGE | eff) Unit
store storage state = SocketS.store storage state.socketS

load :: forall eff. ForeignStorage -> State -> Eff (storage :: STORAGE | eff) State
load storage state = do
  newS <- SocketS.load storage state.socketS
  pure $ state { socketS = newS }

needsWrite :: State -> State -> Boolean
needsWrite old new = SocketS.needsWrite old.socketS new.socketS
