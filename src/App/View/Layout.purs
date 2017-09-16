module App.View.Layout where

import App.View.Homepage as Homepage
import App.View.NotFound as NotFound
import App.View.Presentation as Presentation
import App.Routes (Route(NotFound, Home, Presentation))
import App.State (State(..))
import App.Util (findSlide)
import App.Events (Event)
import CSS (CSS, fromString, (?), fontSize, display, inlineBlock, marginTop, marginRight, marginLeft, px, value, key, color, backgroundColor, padding, borderRadius)
import CSS.Border (border, solid)
import CSS.TextAlign (center, textAlign)
import CSS.Text (textDecoration, noneTextDecoration, letterSpacing)
import CSS.Text.Transform (textTransform, uppercase)
import Color (rgb)
import Control.Bind (discard)
import Data.Function (($), (#))
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!))
import Data.Maybe (maybe)

view :: State -> HTML Event
view (State st) =
  div ! className "app" $ do
    case st.route of
      (Home) -> Homepage.view (State st)
      (Presentation name number) -> goToPresentation number
      (NotFound url) -> notFound
  where goToPresentation number = maybe notFound (Presentation.view) (findSlide (State st) number)
        notFound = NotFound.view (State st)
