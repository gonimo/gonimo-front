module Gonimo.UI.Socket.Serializer where

import Prelude
import Gonimo.Client.LocalStorage as Storage
import Control.Monad.Eff (Eff)
import DOM.WebStorage.Generic (getItem, setItem)
import DOM.WebStorage.Storage (STORAGE, ForeignStorage)
import Data.Lens (_Just, (?~), (.~), (^?), (^.))
import Data.Maybe (fromMaybe, Maybe(Just, Nothing))
import Gonimo.UI.Socket.Lenses (video, _MediaStreamConstraints, constraints)
import Gonimo.UI.Socket.Types (State)
import Gonimo.Util (differentObject)


store :: forall eff. ForeignStorage -> State -> Eff (storage :: STORAGE | eff) Unit
store storage state = do
  case state.currentFamily of
    Nothing -> pure unit
    Just v  -> setItem storage Storage.currentFamily v
  let haveVideo = fromMaybe true $ state^?constraints<<<_MediaStreamConstraints<<<video
  setItem storage Storage.videoEnabled haveVideo

load :: forall eff. ForeignStorage -> State -> Eff (storage :: STORAGE | eff) State
load storage state = do
  mv <- getItem storage Storage.currentFamily
  let newState =
        case mv of
          Nothing -> state
          Just _ ->  state { currentFamily = mv }
  videoEnabled <- fromMaybe true <$> getItem storage Storage.videoEnabled
  pure $ newState # constraints<<<_MediaStreamConstraints<<<video .~ videoEnabled


needsWrite :: State -> State -> Boolean
needsWrite old new = differentObject old.currentFamily new.currentFamily
                     || differentObject old.constraints new.constraints
