module App.Util where

import Prelude
import App.Prelude
import App.State (State(..), SlideData(..))
import Data.Maybe (Maybe(..))
import Data.Array (index)
import Data.Map (lookup)
import Data.Newtype (unwrap)

findSlide ∷ State → String → Int → Maybe SlideData
findSlide (State st) "multiply-me" number = index (_.multiplyMe $ unwrap st.presentations) (number - 1)
findSlide (State st) "mal" number = index (_.mal $ unwrap st.presentations) (number - 1)
findSlide (State st) _ number = Nothing

