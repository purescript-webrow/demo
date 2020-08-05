module Frontend where

import Prelude

import Components (formComponentBuilder, runAuth, runMessage)
import Data.Foldable (for_)
import Data.Maybe (Maybe)
import Effect (Effect)
import React.Basic (element)
import React.Basic.DOM (hydrate)
import Run (case_, run) as Run
import Web.DOM.Element (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode) as HTMLDocument
import Web.HTML.Window (document)
import WebRow.Applets.Auth.Forms (loginForm)
import WebRow.Templating.MUI.Auth (appElementId)

getElementById' ∷ String → Effect (Maybe Element)
getElementById' id =
  window >>= document >>= HTMLDocument.toNonElementParentNode >>> getElementById id

interpretAuth initialForm = Run.run Run.case_ ((runAuth <<< runMessage) initialForm)

interpretPasswordCheck initialForm = Run.run Run.case_ (runMessage initialForm)

main ∷ Effect Unit
main = do
  container ← getElementById' appElementId
  formComponentBuilder loginForm interpretAuth >>= flip element {} >>> hydrate >>> for_ container
  -- loginFormComponentBuilder passwordForm interpretPasswordCheck >>= flip element {} >>> hydrate >>> for_ container
  -- loginFormComponentBuilder emailTakenForm >>= flip element {} >>> hydrate >>> for_ container

