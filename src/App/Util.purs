module App.Util where

import Prelude
import App.Prelude
import App.State (State(..), SlideData(..))
import Data.Maybe (Maybe(..))
import Data.Array (index)

findSlide ∷ State → Int → Maybe SlideData
findSlide (State st) number = index st.slides (number - 1)
