module App.View.Presentation where

import Prelude
import App.Prelude
import App.State (State(..), SlideData(..))
import App.Events (Event)
import Pux.DOM.HTML (HTML, style)
import Pux.DOM.Events (onLoad, onClick)
import Text.Smolder.HTML (div, h1, pre, code, button)
import Text.Smolder.Markup ((!), text, (#!))
import Text.Smolder.HTML.Attributes (className, id)
import App.Routes (Route(..))
import Data.Array (index)
import Data.Maybe (maybe, Maybe)
import Data.Newtype (unwrap)
import App.Events (Event(..))
import CSS.TextAlign (leftTextAlign, textAlign)
import Highlighter (codeField)
import CSS (CSS, fromString, (?), fontSize, display, inlineBlock, marginTop, marginRight, marginLeft, px, value, key, color, backgroundColor, padding, borderRadius)

view ∷ SlideData → HTML Event
view (SlideData s) = do
  div ! className "presentation" $ do
    style css

    codeField {content: s.content} (text "")-- $ pre $ code ! className "presentation-code" $ text s.content
    button ! className "previous" #! onClick PreviousSlide $ text "previous"
    button ! className "next" #! onClick NextSlide  $ text "next"
  div ! className "presentation-annotation" $ do
    text s.annotation


css ∷ CSS
css = do
  fromString ".presentation pre" ? do
    textAlign leftTextAlign
