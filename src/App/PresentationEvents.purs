module App.PresentationEvents where

import Prelude
import App.Prelude
import App.Util (findSlide)
import Pux (EffModel, noEffects, onlyEffects)
import App.State (State(..), SlideData(..))
import Data.Maybe (Maybe(..))
import App.Routes (Route(..))
import Data.Newtype (unwrap, wrap)
import App.Effects (AppEffects)

data Event = PageView String Int

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView name number) (State st) = noEffects $ makeState

  where makeState ∷ State
        makeState = slideState $ getFileContents <$> findSlide (State st) number

        getFileContents ∷ SlideData → String
        getFileContents = _.fileName ∘ unwrap

        slideState ∷ Maybe String → State
        slideState text = State st { route = Presentation name number, loaded = true, currentSlideContent = wrap text }

