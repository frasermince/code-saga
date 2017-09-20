module App.View.Homepage where

import App.Events (Event, Event(..))
import App.State (State)
import Control.Bind (discard)
import Data.Function (($))
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (a, div, h1, button)
import Text.Smolder.HTML.Attributes (href, className)
import Text.Smolder.Markup ((!), text, (#!))

view :: State -> HTML Event
view s =
  div do
    h1 $ text "Presentations"
    button #! onClick (Navigate "/presentation/multiply-me/1") $ text "Rails App Refactor"
    button #! onClick (Navigate "/presentation/mal/1") $ text "Lisp Interpreter Written in Haskell"
