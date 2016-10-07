module Pux.Html.Attributes.Bootstrap where

import Pux.Html (Attribute)
import Pux.Html.Attributes (attr)

dataToggle :: forall a. String -> Attribute a
dataToggle = attr "data-toggle"


dataTarget :: forall a. String -> Attribute a
dataTarget = attr "data-target"



-- Non bootstrap:

integrity :: forall a. String -> Attribute a
integrity = attr "integrity"

