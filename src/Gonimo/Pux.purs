-- | Gonimo specific Pux helpers and replacements for defaults.

module Gonimo.Pux where

import Prelude
import Gonimo.Client.Types as Gonimo
import Control.Alternative (empty, class Alternative)
import Control.Monad.Aff (Aff)
import Control.Monad.IO (IO)
import Control.Monad.Maybe.Trans (runMaybeT, MaybeT(MaybeT))
import Control.Monad.Reader.Class (local, ask, class MonadReader)
import Control.Monad.Reader.Trans (runReaderT, ReaderT(ReaderT))
import Control.Monad.State.Class (put, get, state, class MonadState)
import Control.Monad.State.Trans (runStateT, StateT(StateT))
import Data.Bifunctor (bimap, class Bifunctor)
import Data.Lens (prism, (^?), PrismP, (.~), (^.), LensP)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple))
import Gonimo.Client.Types (GonimoEff, Gonimo, Settings, class ReportErrorAction)

class HasChild parent child where
  toParent   :: parent -> child -> parent
  toChild    :: forall f. Alternative f => parent -> f child


newtype Component props state action = Component (MaybeT (ReaderT props (StateT state IO)) action)

runComponent :: forall props state action. Component props state action -> (MaybeT (ReaderT props (StateT state IO))) action
runComponent (Component m) = m

liftChild :: forall childProps childState parentProps parentState action.
            (HasChild parentState childState)
            => childProps
            -> Component childProps childState action
            -> Component parentProps parentState action
liftChild props child = do
  oldParent <- get
  oldChild <- toChild oldParent
  let mr = runComponent child props oldChild
  case mr of
    Nothing -> empty
    Just (Tuple action newChild) -> if differentObject newChild oldChild
                                    then put $ toParent oldParent newChild
                                    else pure action

instance functorComponent :: Functor (Component props state) where
  map f (Component m) = Component $ map f m

instance applyComponent :: Apply (Component props state) where
  apply (Component mf) (Component ma) = Component $ apply mf ma

instance applicativeComponent :: Applicative (Component props state) where
  pure = Component <<< pure

instance bindComponent :: Bind (Component props state) where
  bind (Component ma) mf = Component $ bind ma (runComponent <<< mf)

instance monadComponent :: Monad (Component props state)

instance monadReaderPropsComponent :: MonadReader props (Component props state) where
  ask = Component ask
  --local :: forall a. (r -> r) -> m a -> m a
  local f (Component ma) = Component (local f ma)

instance monadStateStateComponent :: MonadState state (Component props state) where
  -- get = Component <<< MaybeT <<< runMaybeT <<< ReaderT <<< flip runReaderT
  --       <<< StateT $ \ s -> runStateT (pure s) s
  state f = Component $ state f


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

updateChild :: forall eff parentState childState parentAction childAction props.
            LensP parentState childState
            -> (childAction -> parentAction)
            -> (props -> childAction -> childState -> EffModel eff childState childAction)
            -> props -> childAction -> parentState -> EffModel eff parentState parentAction
updateChild lens mkAction childUpdate props action state =
    bimap smartUpdate mkAction $ childUpdate props action (state ^. lens)
  where
    smartUpdate :: childState -> parentState
    smartUpdate newChild =if differentObject newChild (state ^. lens)
                          then state # lens .~ newChild
                          else  state

updatePrismChild :: forall eff parentState childState parentAction childAction props.
            PrismP parentState childState
            -> (childAction -> parentAction)
            -> (props -> childAction -> childState -> EffModel eff childState childAction)
            -> props -> childAction -> parentState -> Maybe (EffModel eff parentState parentAction)
updatePrismChild prism mkAction childUpdate props action state =
    bimap smartUpdate mkAction <<< childUpdate props action <$> state ^? prism
  where
    smartUpdate :: childState -> parentState
    smartUpdate newChild = case state ^? prism of
                                Nothing -> state
                                Just oldChild -> if differentObject newChild oldChild
                                                    then state # prism .~ newChild
                                                    else state

-- toParent :: forall eff parentState childState parentAction childAction.
--             LensP parentState childState
--             -> (childAction -> parentAction)
--             -> parentState -> EffModel eff childState childAction
--             -> EffModel eff parentState parentAction
-- toParent lens mkAction state = bimap smartUpdate mkAction
--   where
--     smartUpdate :: childState -> parentState
--     smartUpdate newChild = if differentObject newChild (state ^. lens)
--                            then lens .~ newChild $ state
--                            else state

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

type Props ps = { settings :: Settings | ps }

onlyGonimos :: forall state action eff ps
               . ReportErrorAction action => Props ps -> state -> Array (Gonimo eff action)
               -> EffModel eff state action
onlyGonimos props state = onlyEffects state <<< map (Gonimo.toAff props.settings)

-- | Like `onlyGonimos` but for a single effect
onlyGonimo :: forall state action eff ps
               . ReportErrorAction action => Props ps -> state -> Gonimo eff action
               -> EffModel eff state action
onlyGonimo props state eff = onlyGonimos props state [eff]

-- | Like onlyGonimos but with arguments flipped
justGonimos :: forall state action eff ps
               . ReportErrorAction action => Props ps -> Array (Gonimo eff action)
               -> state -> EffModel eff state action
justGonimos props = flip $ onlyGonimos props

-- | Like `justGonimos` but for a single effect
justGonimo :: forall state action eff ps
               . ReportErrorAction action => Props ps -> Gonimo eff action
               -> state -> EffModel eff state action
justGonimo props eff = justGonimos props [eff]

-- | Convert an our EffModel using update function to one usable for Pux.Config
toPux :: forall state action eff. (action -> state -> EffModel eff state action)
         -> action -> state -> EffModelImpl state action eff
toPux ours action state = runEffModel $ ours action state

foreign import differentObject :: forall a b. a -> b -> Boolean
