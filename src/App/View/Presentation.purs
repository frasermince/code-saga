module App.View.Presentation where

import Prelude
import App.State (SlideData(..))
import Pux.DOM.HTML (HTML, style)
import Pux.DOM.HTML.Attributes as Att
import Pux.DOM.Events (onClick)
import Text.Smolder.HTML (div, button, pre, code, strong)
import Text.Smolder.Markup ((!), text, (#!))
import Text.Smolder.HTML.Attributes (className)
import App.Events (Event(..))
import CSS.TextAlign (leftTextAlign, textAlign)
import Highlighter (codeField)
import CSS (CSS, fromString, (?), fontSize, display, marginTop, marginRight, marginLeft, margin, px, key, backgroundColor, padding, borderRadius, grid, height, pct, border, solid, graytone, white, width, boxSizing, borderBox, flex, rgb, paddingLeft, borderColor, rgba, color, black, rem)
import CSS.Overflow (overflow, scroll, overflowY)
import CSS.Display (floatLeft, float, floatRight)
import CSS.Geometry (lineHeight)
import CSS.Border (borderBottom, border)
import CSS.Common (auto)
import CSS.Font (FontWeight(..), fontWeight, bold)
import CSS.Flexbox (flexDirection, column)

view ∷ SlideData → HTML Event
view (SlideData s) = do
  style css
  div ! Att.style presentation ! className "presentation" $ do

    div ! Att.style file ! className "file" $ do
      div ! Att.style header ! className "file-header" $ strong ! Att.style fileName $ text $ s.fileName
      codeField {content: s.content, lineNumber: s.lineNumber} (text "")
      -- pre $ code ! className "presentation-code" $ text s.content
    div ! Att.style annotation ! className "annotation" $ do
      div ! Att.style contentHeader $ do
        button ! Att.style leftButton ! className "previous" #! onClick PreviousSlide $ text "Back"
        button ! Att.style rightButton ! className "next" #! onClick NextSlide  $ text "Next"
      div ! className "content" ! Att.style content $ do
        text s.annotation

buttonStyle ∷ CSS
buttonStyle = do
  padding (px 8.0) (px 12.0) (px 8.0) (px 12.0)
  height $ px 32.0
  width $ px 76.0
  backgroundColor $ rgb 100 100 100
  -- borderRadius (rem 0.25) (rem 0.25) (rem 0.25) (rem 0.25)
  borderColor $ (rgb 52 58 64)
  color $ (rgb 255 255 255)
  fontWeight bold
  fontSize $ rem 0.8
  border solid (px 1.0) (rgba 0 0 0 0.0)

leftButton ∷ CSS
leftButton = do
  buttonStyle
  float floatLeft

rightButton ∷ CSS
rightButton = do
  buttonStyle
  float floatRight

css ∷ CSS
css = do
  fromString ".presentation pre" ? do
    textAlign leftTextAlign
    backgroundColor white
    -- height $ pct 45.0

previous ∷ CSS
previous = do
  float floatLeft

fileName ∷ CSS
fileName =  do
  paddingLeft $ px 58.0

presentation ∷ CSS
presentation = do
  marginLeft auto
  marginRight auto
  width $ pct 90.0
  display grid
  key (fromString "grid-template-rows") "60% 40%"
  key (fromString "grid-template-columns") "1fr"
  key (fromString "grid-gap") "10px"
  height $ pct 99.0

content ∷ CSS
content = do
  marginLeft $ px 40.0
  padding (px 25.0) (px 45.0) (px 45.0) (px 25.0) 
  boxSizing borderBox
  fontSize $ px 18.0
  lineHeight $ pct 150.0
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

contentHeader ∷ CSS
contentHeader = do
  header
  margin (px 0.0) (px 0.0) (px 0.0) (px 0.0)
  padding (px 0.0) (px 0.0) (px 0.0) (px 0.0)

header ∷ CSS
header = do
  borderBottom solid (px 1.0) (rgb 229 229 229)
  padding (px 5.0) (px 10.0) (px 5.0) (px 10.0)
  lineHeight (px 32.0)
  height (px 32.0)
  backgroundColor $ (rgb 250 250 250)
  fontSize $ px 14.0

codeBorder ∷ CSS
codeBorder = do
  border solid (px 1.0) (graytone 0.75)
  borderRadius (px 3.0) (px 3.0) (px 3.0) (px 3.0)
