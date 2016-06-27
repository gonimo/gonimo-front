module Gonimo.Client.Effects where

import Prelude
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Generic (gShow)
import Gonimo.Client.Types (Error, EffectsT)

-- run :: forall eff a. Settings -> Effects eff -> Aff (Effects eff) a
-- run = 

log :: forall eff. String -> EffectsT (console :: CONSOLE | eff) Unit
log = liftEff <<< Console.log

error :: forall eff. Error -> EffectsT (console :: CONSOLE | eff) Unit
error = liftEff <<< Console.error <<< gShow
