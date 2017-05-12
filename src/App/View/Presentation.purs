module App.View.Presentation where

import Prelude
import App.Prelude
import App.State (State(..), SlideData(..))
import App.Events (Event)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onLoad, onClick)
import Text.Smolder.HTML (div, h1, pre, code, button)
import Text.Smolder.Markup ((!), text, (#!))
import Text.Smolder.HTML.Attributes (className, id)
import App.Routes (Route(..))
import Data.Array (index)
import Data.Maybe (maybe, Maybe)
import Data.Newtype (unwrap)
import App.Events (Event(..))
import Data.Ring ((-))

-- projectCode :: State → HTML Event
-- projectCode (State {slides: slides, route: (Slide name number)}) =
--   maybe notFound fetch getNameFromSlide
--   where fetch fileName = text fileName
--         getNameFromSlide ∷ Maybe String
--         getNameFromSlide = _.fileName ∘ unwrap <$> findSlide
--         findSlide ∷ Maybe SlideData
--         findSlide = index slides (number - 1)
-- projectCode _ = notFound

notFound = div #! onLoad (Navigate "/not_found") $ text "No slide"

view ∷ State → HTML Event
view s =
  div ! className "presentation" $ do
    pre $ code ! className "presentation-code" $ maybe notFound text (unwrap $ _.currentSlideContent $ unwrap $ s)
    button ! className "previous" #! onClick PreviousSlide $ text "previous"
    button ! className "next" #! onClick NextSlide  $ text "next"
