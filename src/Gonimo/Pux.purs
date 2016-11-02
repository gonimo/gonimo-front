-- | Gonimo specific Pux helpers and replacements for defaults.

module Gonimo.Pux ( Props
                  , makeChildData
                  , ChildData
                  , ToChild
                  , Component
                  , Update
                  , ComponentType
                  , liftChild
                  , toPux
                  , runGonimos
                  , runGonimo
                  , onlyModify
                  , noEffects
                  , wrapAction
                  , toParent
                  , toParentM
                  , class MonadComponent
                  , onClickWithDefault
                  ) where

import Prelude
import Data.Array as Arr
import Gonimo.Client.Types as Gonimo
import Control.Alternative (empty, class Alternative)
import Control.Monad.Aff (Aff)
import Control.Monad.IO (IO)
import Control.Monad.IO.Class (class MonadIO, liftIO)
import Control.Monad.Maybe.Trans (runMaybeT, MaybeT(MaybeT))
import Control.Monad.Reader.Class (ask, local, class MonadReader)
import Control.Monad.Reader.Trans (runReaderT, ReaderT(ReaderT))
import Control.Monad.State (State)
import Control.Monad.State.Class (modify, put, get, state, class MonadState)
import Control.Monad.State.Trans (runStateT, StateT(StateT))
import Control.Monad.Trans (lift, class MonadTrans)
import Data.Bifunctor (bimap, class Bifunctor)
import Data.Function.Uncurried (runFn2, Fn2)
import Data.Identity (runIdentity)
import Data.Lens (SetterP, (.=), prism, (^?), TraversalP, (.~), (^.), LensP)
import Data.Maybe (maybe, Maybe(Just, Nothing))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Gonimo.Client.Types (Gonimo, Settings, class ReportErrorAction)
import Gonimo.Util (fromMaybeM, runIOToSomeAff)
import Partial.Unsafe (unsafeCrashWith)
import Pux (noEffects, EffModel)
import Pux.Html (Attribute)
import Pux.Html.Events (MouseEvent)


type Props ps = { settings :: Settings | ps }

-- First this was a record but records containing prisms is something psc does not seem to like.
-- | A helper data structure for transforming child components to parent components:
data ChildData parentState childState childProps =
  ChildData (TraversalP parentState childState) childProps

makeChildData :: forall parentState childState childProps. TraversalP parentState childState -> childProps -> (ChildData parentState childState childProps)
makeChildData fromToParent props = ChildData fromToParent props

childDataFromToParent :: forall parentState childState childProps.
                         ChildData parentState childState childProps
                         -> TraversalP parentState childState
childDataFromToParent (ChildData fromToParent _) = fromToParent

childDataProps :: forall parentState childState childProps.
                         ChildData parentState childState childProps
                         -> childProps
childDataProps (ChildData _ props) = props


-- | Create ChildData from a parent component:
type ToChild parentProps parentState childProps childState =
  Component parentProps parentState (ChildData parentState childState childProps)

-- | A component is just a Reader monad with react style props and a State monad
-- | with well our component state.
newtype Component props state a = Component (ReaderT props (State state) a)

-- | Generic type which depends on MonadComponent instead on a concrete Component.
-- | Also it assumes a monadic value of type `Array (IO Action)`
-- type ComponentType props state action = forall m. MonadComponent props state m => m (Array (IO action))
type ComponentType props state action = Component props state (Array (IO action))

-- | Stateful Update function which can be converted to a Pux eff model ...
-- I think we need fundeps for this to work properly in client code:
-- type Update props state action = forall m. (MonadComponent props state m)
--                                  => action -> m (Array (IO action))
type Update props state action = action -> ComponentType props state action

-- | Concrete instance for Update
type UpdateComponent props state action = action -> Component props state (Array (IO action))

runComponent :: forall props state a. Component props state a -> ReaderT props (State state) a
runComponent (Component m) = m

runComponentFull :: forall props state a. props -> state -> Component props state a -> Tuple a state
runComponentFull props state = runIdentity <<< flip runStateT state <<< flip runReaderT props <<< runComponent


-- | Run a child component and transform it to a parent component, updating the
-- | state only if it actually changed.
-- | We return Nothing if the child is currently not present in the parent.
liftChild :: forall childProps childState parentProps parentState a.
             ToChild parentProps parentState childProps childState
            -> Component childProps childState a
            -> Component parentProps parentState (Maybe a)
liftChild toChild child = do
  childData <- toChild
  oldParent <- get
  let
    fromToParent = childDataFromToParent childData
    childProps   = childDataProps childData

    runComponentCheck oldChild = do
      case runComponentFull childProps oldChild child of
        Tuple a newChild -> do
          when (differentObject newChild oldChild)
            $ fromToParent .= newChild
          pure a
  traverse runComponentCheck (oldParent ^? fromToParent)

-- | Convert a component to an EffModel usable with Pux.
-- | The restriction to of props to Unit for this root component is currently not justified
-- | and can be dropped if necessary. I just have some vague idea for future enhancements where
-- | properties for the root level component could be in the way.
toEffModel :: forall state action eff. state -> Component Unit state (Array (IO action)) -> EffModel state action eff
toEffModel initState c =
  case runComponentFull unit initState c of
    Tuple effects state -> { state : state
                           , effects : runIOToSomeAff <$> effects
                           }

-- | We are still using Pux - so let's a get a Pux like Update function which provides an EffModel ...
toPux :: forall state action eff. Update Unit state action -> action -> state -> EffModel state action eff
toPux update action state = toEffModel state $ update action



runGonimos :: forall action ps m
               . ( ReportErrorAction action
                 , MonadReader (Props ps) m
                 )
               => Array (Gonimo action) -> m (Array (IO action))
runGonimos effects = do
  props <- ask
  pure (Gonimo.toIO props.settings <$> effects)

runGonimo :: forall action ps m
               . ( ReportErrorAction action
                 , MonadReader (Props ps) m
                 )
               => Gonimo action -> m (Array (IO action))
runGonimo effect = do
  props <- ask
  pure $ [ Gonimo.toIO props.settings effect ]

-- | Only modify state, but trigger no effects:
onlyModify :: forall state action m . ( MonadState state m)
             => (state -> state) -> m (Array (IO action))
onlyModify  f = modify f *> pure []

noEffects :: forall m a. (Applicative m) => m (Array a)
noEffects = pure []

wrapAction :: forall action. action -> Array (IO action)
wrapAction = Arr.singleton <<< pure

toParent :: forall childAction parentAction parentProps parentState.
                  Array (IO parentAction)
                  -> (childAction -> parentAction)
                  -> Component parentProps parentState (Maybe (Array (IO childAction)))
                  -> Component parentProps parentState (Array (IO parentAction))
toParent onNothing mkAction child = maybe onNothing (map (map mkAction)) <$> child

toParentM :: forall childAction parentAction parentProps parentState.
                  Component parentProps parentState (Array (IO parentAction))
                  -> (childAction -> parentAction)
                  -> Component parentProps parentState (Maybe (Array (IO childAction)))
                  -> Component parentProps parentState (Array (IO parentAction))
toParentM onNothing mkAction child = fromMaybeM onNothing <<< map (map (map (map mkAction))) $ child

class (MonadReader props m, MonadState state m) <= MonadComponent props state m

instance monadComponentComponent :: MonadComponent props state (Component props state)

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


foreign import differentObject :: forall a b. a -> b -> Boolean


onClickWithDefault :: forall action. (MouseEvent -> action) -> Attribute action
onClickWithDefault = runFn2 handlerWithDefault "onClick"

foreign import handlerWithDefault :: forall ev a. Fn2 String (ev -> a) (Attribute a)
