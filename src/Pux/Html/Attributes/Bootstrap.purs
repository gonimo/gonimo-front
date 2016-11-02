module Pux.Html.Attributes.Bootstrap where

import Pux.Html (Attribute)
import Pux.Html.Attributes (attr)

dataToggle :: forall a. String -> Attribute a
dataToggle = attr "data-toggle"


dataTarget :: forall a. String -> Attribute a
dataTarget = attr "data-target"

dataPlacement :: forall a. String -> Attribute a
dataPlacement = attr "data-placement"


-- Non bootstrap:

dataClipboardTarget :: forall a. String -> Attribute a
dataClipboardTarget = attr "data-clipboard-target"

