module Gonimo.Util where

import Prelude
import Data.Argonaut.Generic.Aeson as Aeson
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(Right, Left))
import Data.Generic (class Generic)
import Data.Maybe (Maybe(Just, Nothing))

toString :: forall a. Generic a => a -> String
toString = show <<< Aeson.encodeJson

fromString :: forall a. Generic a => String -> Maybe a
fromString = hush <<< (Aeson.decodeJson <=< jsonParser)

hush :: forall a b. Either a b -> Maybe b
hush (Left _) = Nothing
hush (Right v) = Just v
