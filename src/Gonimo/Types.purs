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

instance eqSecret :: Eq Secret where
  eq (Secret str1) (Secret str2) = eq str1 str2

instance ordSecret :: Ord Secret where
  compare (Secret str1) (Secret str2) = compare str1 str2

newtype Key a = Key Int

derive instance genericKey :: Generic (Key a)

instance eqKey :: Eq (Key a) where
  eq (Key a) (Key b) = eq a b

instance ordKey :: Ord (Key a) where
  compare (Key a) (Key b) = compare a b
