module Gonimo.AppState where

import Prelude

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Aff.Class
import Control.Monad.Error.Class
import Control.Monad.Eff.Console hiding (error)
import Control.Monad.State.Class (modify, get)
import Control.Monad.State.Trans

import Control.Monad.Eff.Exception


type GonimoT e = StateT Int (Aff (err :: EXCEPTION | e))

-- testGonimo :: forall m. ( MonadState Int m, Monad m) => m Int
testGonimo :: forall e . GonimoT e Int
testGonimo = do
  modify (\s -> s + 1)
  --throwError $ error "WTF"
  get


runGonimo :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
runGonimo = runAff handleError handleResult $ runStateT testGonimo 0
   where
     handleError e = log $ "Caught error: " <> show e
     handleResult r = log $ "Got result: " <> show r

{--
import Gonimo.AppState
runGonimo
--}
