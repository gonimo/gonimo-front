-- | Replacement types for translated server types.
module Gonimo.Types where

import Prelude

import Data.Generic (class Generic)

newtype Date = DateTime String

dateToString :: Date -> String
dateToString (DateTime t) = t

derive instance genericDate :: Generic Date

newtype Secret = Secret String

derive instance genericSecret :: Generic Secret

newtype Key a = Key Int

derive instance genericKey :: Generic (Key a)

instance eqKey :: Eq (Key a) where
  eq (Key a) (Key b) = eq a b

instance ordKey :: Ord (Key a) where
  compare (Key a) (Key b) = compare a b
