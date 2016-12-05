module Gonimo.Util where

import Prelude
import Data.Argonaut.Generic.Aeson as Aeson
import Control.Monad.Aff (makeAff, Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.IO (IO)
import DOM (DOM)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(Right, Left))
import Data.Generic (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Signal (constant, Signal)
import Unsafe.Coerce (unsafeCoerce)
import WebRTC.MediaStream (MediaStream)

-- | Things that can be shown to the user
class UserShow a where
  userShow  :: a -> String

-- getHRef :: forall eff. Eff eff String
-- getHRef = coerceEffects $ do
--   w <- DOM.window
--   l <- Dom.location w
--   href l


foreign import createUrlSignal :: forall eff url.
                                  (url -> Signal url) ->
                                  Eff (dom :: DOM | eff) (Signal String)

-- | Returns a signal containing the full href, unlike pux routers sampleUrl.
sampleUrl :: forall eff. Eff (dom :: DOM | eff) (Signal String)
sampleUrl = createUrlSignal constant

toString :: forall a. Generic a => a -> String
toString = show <<< Aeson.encodeJson

fromString :: forall a. Generic a => String -> Maybe a
fromString = hush <<< (Aeson.decodeJson <=< jsonParser)

hush :: forall a b. Either a b -> Maybe b
hush (Left _) = Nothing
hush (Right v) = Just v

coerceEffects :: forall m eff1 eff2 a. m eff1 a -> m eff2 a
coerceEffects = unsafeCoerce

runIOToSomeAff :: forall eff a. IO a -> Aff eff a
runIOToSomeAff = unsafeCoerce

fromMaybeM :: forall a m. Monad m => m a -> m (Maybe a) -> m a
fromMaybeM fallback m = do
  r <- m
  case r of
    Nothing -> fallback
    Just v -> pure v

foreign import differentObject :: forall a b. a -> b -> Boolean


foreign import boostVolumeMediaStream :: forall eff. MediaStream -> Eff eff MediaStream

foreign import data Audio :: *

foreign import _loadSound :: forall eff. (Audio -> Eff eff Unit)
                             -> (Error -> Eff eff Unit)
                             -> String -> Eff eff Unit

loadSound :: forall eff. String -> Aff eff Audio
loadSound url = makeAff (\e s -> _loadSound s e url)

foreign import playSound :: forall eff. Audio -> Eff eff Unit
foreign import stopSound :: forall eff. Audio -> Eff eff Unit
