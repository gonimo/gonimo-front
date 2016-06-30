-- | Replacement types for translated server types.
module Gonimo.Types where

import Data.Generic (class Generic)

newtype Date = DateTime String

derive instance genericDate :: Generic Date

newtype Secret = Secret String

derive instance genericSecret :: Generic Secret

newtype Key a = Key Int

derive instance genericKey :: Generic (Key a)

data Family = Family -- Dummy type for Key Family
