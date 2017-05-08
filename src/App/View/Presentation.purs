module App.View.Presentation where

import Prelude
-- import App.Prelude
import App.State (State)
import App.Events (Event)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (a, div, h1)
import Text.Smolder.Markup ((!), text)
import Text.Smolder.HTML.Attributes (className, id)

view ∷ State → HTML Event
view s =
  div ! id "first-slide" $ do
    h1 $ text "Name"
