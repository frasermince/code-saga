module Highlighter where
import React (ReactClass)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (reactClassWithProps)

foreign import highlightClass :: ∀ props. ReactClass props

codeField :: ∀ ev props. props → HTML ev → HTML ev
codeField = reactClassWithProps highlightClass "presentation-code"
