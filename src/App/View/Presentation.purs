module App.View.Presentation where

import Prelude
import App.State (SlideData(..))
import Pux.DOM.HTML (HTML, style)
import Pux.DOM.HTML.Attributes as Att
import Pux.DOM.Events (onClick)
import Text.Smolder.HTML (div, button, pre, code)
import Text.Smolder.Markup ((!), text, (#!))
import Text.Smolder.HTML.Attributes (className)
import App.Events (Event(..))
import CSS.TextAlign (leftTextAlign, textAlign)
import Highlighter (codeField)
import CSS (CSS, fromString, (?), fontSize, display, marginTop, marginRight, marginLeft, px, key, backgroundColor, padding, borderRadius, grid, height, pct, border, solid, graytone, white, width, boxSizing, borderBox, flex)
import CSS.Overflow (overflow, scroll, overflowY)
import CSS.Display (floatLeft, float)
import CSS.Geometry (lineHeight)
import CSS.Border (borderBottom)
import CSS.Common (auto)
import CSS.Flexbox (flexDirection, column)

view ∷ SlideData → HTML Event
view (SlideData s) = do
  style css
  div ! Att.style presentation ! className "presentation" $ do

    div ! Att.style file ! className "file" $ do
      div ! Att.style header ! className "file-header" $ text $ s.fileName
      codeField {content: s.content, lineNumber: s.lineNumber} (text "")
      -- pre $ code ! className "presentation-code" $ text s.content
    div ! Att.style annotation ! className "annotation" $ do
      div ! Att.style header $ do
        button ! className "previous" #! onClick PreviousSlide $ text "previous"
        button ! className "next" #! onClick NextSlide  $ text "next"
      div ! className "content" ! Att.style content $ do
        text s.annotation

css ∷ CSS
css = do
  fromString ".presentation pre" ? do
    marginTop (px 0.0)
    textAlign leftTextAlign
    backgroundColor white
    overflowY scroll
    -- height $ pct 45.0

previous ∷ CSS
previous = do
  float floatLeft

presentation ∷ CSS
presentation = do
  marginLeft auto
  marginRight auto
  width $ px 980.0
  display grid
  key (fromString "grid-template-rows") "60% 40%"
  key (fromString "grid-template-columns") "1fr"
  key (fromString "grid-gap") "10px"
  height $ pct 99.0

content ∷ CSS
content = do
  padding (px 25.0) (px 45.0) (px 45.0) (px 25.0) 
  boxSizing borderBox
  -- height $ pct 83.0
  overflowY scroll

annotation ∷ CSS
annotation = do
  -- height $ pct 100.0
  fontSize $ px 16.0
  key (fromString "grid-row") "2 / 3"
  container

file ∷ CSS
file = do
  key (fromString "grid-row") "1 / 2"
  container

container ∷ CSS
container = do
  codeBorder
  display flex
  flexDirection column
  key (fromString "grid-column") "1 / 2"

header ∷ CSS
header = do
  borderBottom solid (px 1.0) (graytone 0.75)
  padding (px 5.0) (px 10.0) (px 5.0) (px 10.0)
  lineHeight (px 32.0)
  height (px 32.0)
  backgroundColor $ graytone 0.97
  fontSize $ px 14.0

codeBorder ∷ CSS
codeBorder = do
  border solid (px 1.0) (graytone 0.75)
  borderRadius (px 3.0) (px 3.0) (px 3.0) (px 3.0)
