module Gonimo.Util where

import Prelude
import Data.Argonaut.Generic.Aeson as Aeson
import Control.Monad.Aff (Aff)
import Control.Monad.IO (IO)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(Right, Left))
import Data.Generic (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Unsafe.Coerce (unsafeCoerce)

-- | Things that can be shown to the user
class UserShow a where
  userShow  :: a -> String

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
