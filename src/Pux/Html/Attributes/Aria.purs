module Pux.Html.Attributes.Aria where

import Pux.Html (Attribute)
import Pux.Html.Attributes (attr)

-- | aria attributes taken from
-- https://www.w3.org/TR/html-aria/#allowed-aria-roles-states-and-properties

ariaActivedescendant :: forall a. String -> Attribute a
ariaActivedescendant = attr "aria-activedescendant"

ariaAtomic :: forall a. String -> Attribute a
ariaAtomic = attr "aria-atomic"

ariaAutocomplete :: forall a. String -> Attribute a
ariaAutocomplete = attr "aria-autocomplete"

ariaBusy :: forall a. String -> Attribute a
ariaBusy = attr "aria-busy"

ariaChecked :: forall a. String -> Attribute a
ariaChecked = attr "aria-checked"

ariaColcount :: forall a. String -> Attribute a
ariaColcount = attr "aria-colcount"

ariaColindex :: forall a. String -> Attribute a
ariaColindex = attr "aria-colindex"

ariaControls :: forall a. String -> Attribute a
ariaControls = attr "aria-controls"

ariaCurrent :: forall a. String -> Attribute a
ariaCurrent = attr "aria-current"

ariaDescribedby :: forall a. String -> Attribute a
ariaDescribedby = attr "aria-describedby"

ariaDetails :: forall a. String -> Attribute a
ariaDetails = attr "aria-details"

ariaDisabled :: forall a. String -> Attribute a
ariaDisabled = attr "aria-disabled"

ariaDropeffect :: forall a. String -> Attribute a
ariaDropeffect = attr "aria-dropeffect"

ariaErrormessage :: forall a. String -> Attribute a
ariaErrormessage = attr "aria-errormessage"

ariaExpanded :: forall a. String -> Attribute a
ariaExpanded = attr "aria-expanded"

ariaFlowto :: forall a. String -> Attribute a
ariaFlowto = attr "aria-flowto"

ariaGrabbed :: forall a. String -> Attribute a
ariaGrabbed = attr "aria-grabbed"

ariaHaspopup :: forall a. String -> Attribute a
ariaHaspopup = attr "aria-haspopup"

ariaHidden :: forall a. String -> Attribute a
ariaHidden = attr "aria-hidden"

ariaInvalid :: forall a. String -> Attribute a
ariaInvalid = attr "aria-invalid"

ariaKeyshortcuts :: forall a. String -> Attribute a
ariaKeyshortcuts = attr "aria-keyshortcuts"

ariaLabel :: forall a. String -> Attribute a
ariaLabel = attr "aria-label"

ariaLabelledby :: forall a. String -> Attribute a
ariaLabelledby = attr "aria-labelledby"

ariaLevel :: forall a. String -> Attribute a
ariaLevel = attr "aria-level"

ariaLive :: forall a. String -> Attribute a
ariaLive = attr "aria-live"

ariaModal :: forall a. String -> Attribute a
ariaModal = attr "aria-modal"

ariaMultiline :: forall a. String -> Attribute a
ariaMultiline = attr "aria-multiline"

ariaMultiselectable :: forall a. String -> Attribute a
ariaMultiselectable = attr "aria-multiselectable"

ariaOrientation :: forall a. String -> Attribute a
ariaOrientation = attr "aria-orientation"

ariaOwns :: forall a. String -> Attribute a
ariaOwns = attr "aria-owns"

ariaPlaceholder :: forall a. String -> Attribute a
ariaPlaceholder = attr "aria-placeholder"

ariaPosinset :: forall a. String -> Attribute a
ariaPosinset = attr "aria-posinset"

ariaPressed :: forall a. String -> Attribute a
ariaPressed = attr "aria-pressed"

ariaReadonly :: forall a. String -> Attribute a
ariaReadonly = attr "aria-readonly"

ariaRelevant :: forall a. String -> Attribute a
ariaRelevant = attr "aria-relevant"

ariaRequired :: forall a. String -> Attribute a
ariaRequired = attr "aria-required"

ariaRoledescription :: forall a. String -> Attribute a
ariaRoledescription = attr "aria-roledescription"

ariaRowcount :: forall a. String -> Attribute a
ariaRowcount = attr "aria-rowcount"

ariaRowindex :: forall a. String -> Attribute a
ariaRowindex = attr "aria-rowindex"

ariaSelected :: forall a. String -> Attribute a
ariaSelected = attr "aria-selected"

ariaSetsize :: forall a. String -> Attribute a
ariaSetsize = attr "aria-setsize"

ariaSort :: forall a. String -> Attribute a
ariaSort = attr "aria-sort"

ariaValuemax :: forall a. String -> Attribute a
ariaValuemax = attr "aria-valuemax"

ariaValuemin :: forall a. String -> Attribute a
ariaValuemin = attr "aria-valuemin"

ariaValuenow :: forall a. String -> Attribute a
ariaValuenow = attr "aria-valuenow"

ariaValuetext :: forall a. String -> Attribute a
ariaValuetext = attr "aria-valuetext"

