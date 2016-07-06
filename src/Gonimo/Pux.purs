-- | Gonimo specific Pux helpers and replacements for defaults.

module Gonimo.Pux where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Bifunctor (class Bifunctor)
import Gonimo.Client.Types (GonimoEff, Gonimo, Settings, class ReportErrorAction)
import Gonimo.Client.Types as Gonimo

type EffModelImpl state action eff =
  { state :: state
  , effects :: Array (Aff (GonimoEff eff) action)
  }

newtype EffModel eff state action = EffModel (EffModelImpl state action eff)

runEffModel :: forall state action eff. EffModel eff state action -> EffModelImpl state action eff
runEffModel (EffModel model) = model


instance bifunctorEffModelEff :: Bifunctor (EffModel eff) where
  bimap f g (EffModel model) = EffModel
    { state : f model.state
    , effects : map (map g) model.effects
    }


onlyEffects :: forall state action eff
               .  state -> Array (Aff (GonimoEff eff) action)
               -> EffModel eff state action
onlyEffects state effects = EffModel { state : state
                                     , effects : effects
                                     }

-- | Like `onlyEffects` but for a single effect
onlyEffect :: forall state action eff
               . state -> Aff (GonimoEff eff) action
               -> EffModel eff state action
onlyEffect state eff = onlyEffects state [eff]

-- | Like onlyEffects but with arguments flipped
justEffects :: forall state action eff
               .  Array (Aff (GonimoEff eff) action)
               -> state -> EffModel eff state action
justEffects = flip onlyEffects

-- | Like `justEffects` but for a single effect
justEffect :: forall state action eff
               .  Aff (GonimoEff eff) action
               -> state -> EffModel eff state action
justEffect eff = justEffects [eff]


noEffects :: forall state action eff. state -> EffModel eff state action
noEffects state = EffModel { state : state, effects : [] }


onlyGonimos :: forall state action eff
               . ReportErrorAction action => Settings -> state -> Array (Gonimo eff action)
               -> EffModel eff state action
onlyGonimos settings state = onlyEffects state <<< map (Gonimo.toAff settings)

-- | Like `onlyGonimos` but for a single effect
onlyGonimo :: forall state action eff
               . ReportErrorAction action => Settings ->state -> Gonimo eff action
               -> EffModel eff state action
onlyGonimo settings state eff = onlyGonimos settings state [eff]

-- | Like onlyGonimos but with arguments flipped
justGonimos :: forall state action eff
               . ReportErrorAction action => Settings -> Array (Gonimo eff action)
               -> state -> EffModel eff state action
justGonimos settings = flip $ onlyGonimos settings

-- | Like `justGonimos` but for a single effect
justGonimo :: forall state action eff
               . ReportErrorAction action => Settings -> Gonimo eff action
               -> state -> EffModel eff state action
justGonimo settings eff = justGonimos settings [eff]

-- | Convert an our EffModel using update function to one usable for Pux.Config
toPux :: forall state action eff. (action -> state -> EffModel eff state action)
         -> action -> state -> EffModelImpl state action eff
toPux ours action state = runEffModel $ ours action state

