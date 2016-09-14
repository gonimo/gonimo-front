module Gonimo.Client.Effects where

import Prelude
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Generic (gShow)
import Gonimo.Client.Types (GonimoError)

-- run :: forall eff a. Settings -> Gonimo eff -> Aff (Effects eff) a
-- run = 

log :: forall m eff. MonadEff (console :: CONSOLE | eff) m => String -> m Unit
log = liftEff <<< Console.log

error :: forall m eff. MonadEff (console :: CONSOLE | eff) m => GonimoError -> m Unit
error = liftEff <<< Console.error <<< gShow


-- | Little UI helper for printing an error and returning some action.
handleError :: forall action m eff. MonadEff (console :: CONSOLE | eff) m => action -> GonimoError -> m action
handleError action err = do
  error err
  pure action
