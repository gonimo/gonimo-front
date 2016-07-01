-- | Gonimo specific Pux helpers and replacements for defaults.

module Gonimo.Pux where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Bifunctor (class Bifunctor)
import Gonimo.Client.Types (EffEffects)

type EffModelImpl state action eff =
  { state :: state
  , effects :: Array (Aff (EffEffects eff) action)
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
               .  state -> Array (Aff (EffEffects eff) action)
               -> EffModel eff state action
onlyEffects state effects = EffModel { state : state
                                     , effects : effects
                                     }

-- | Like onlyEffects but with arguments flipped
justEffects :: forall state action eff
               .  Array (Aff (EffEffects eff) action)
               -> state -> EffModel eff state action
justEffects = flip onlyEffects

-- | Like `justEffects` but for a single effect
justEffect :: forall state action eff
               .  Aff (EffEffects eff) action
               -> state -> EffModel eff state action
justEffect eff = justEffects [eff]

noEffects :: forall state action eff. state -> EffModel eff state action
noEffects state = EffModel { state : state, effects : [] }


-- | Convert an our EffModel using update function to one usable for Pux.Config
toPux :: forall state action eff. (action -> state -> EffModel eff state action)
         -> action -> state -> EffModelImpl state action eff
toPux ours action state = runEffModel $ ours action state

