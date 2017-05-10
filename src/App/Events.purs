module App.Events where

import Prelude
import App.Prelude
import App.Routes (Route(..), match, toURL)
import App.State (State(..), SlideData(..))
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.HTML (window)
import DOM.HTML.History (DocumentTitle(..), URL(..), pushState)
import DOM.HTML.Types (HISTORY)
import DOM.HTML.Window (history)
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..), maybe, isNothing)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects, onlyEffects)
import Pux.DOM.Events (DOMEvent)
import Data.Array (index)
import Data.Newtype (unwrap, wrap)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))

data Event = PageView Route
           | PreviousSlide DOMEvent
           | NextSlide DOMEvent
           | Navigate String DOMEvent

type AppEffects fx = (ajax :: AJAX, dom ∷ DOM, history ∷ HISTORY | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) st =
  noEffects $ newState route st

  where newState ∷ Route → State → State
        newState route (State st) = case route of
          Slide name number → makeState (State st) number route
          _ → State st { route = route, loaded = true }

        makeState ∷ State → Int → Route → State
        makeState st number route = (slideState st route) $ _.fileName ∘ unwrap <$> findSlide st number

        slideState ∷ State → Route → Maybe String → State
        slideState (State st) route text = State st { route = route, loaded = true, currentSlideContent = wrap text }

foldp (PreviousSlide ev) (State st) =
  onlyEffects (State st) [ do
    liftEff do
      preventDefault ev
    pure $ navigateToSlideWith decrement st.route ev
  ]
  where decrement = flip sub 1

foldp (NextSlide ev) (State st) =
  onlyEffects (State st) [ do
    liftEff do
      preventDefault ev
    pure $ navigateToSlideWith increment st.route ev
  ]
  where increment = add 1

foldp (Navigate url ev) state =
  onlyEffects state [ do
    liftEff do
      preventDefault ev
      h <- history =<< window
      pushState (toForeign {}) (DocumentTitle "") (URL url) h
    pure $ Just $ PageView $ redirect (match url) state
  ]
  where redirect ∷ Route → State → Route
        redirect route state | shouldRedirect route state = match "/not_found"
                             | otherwise = route

        shouldRedirect ∷ Route → State → Boolean
        shouldRedirect (Slide _ number) state = isNothing $ findSlide state number
        shouldRedirect _ _ = false



findSlide ∷ State → Int → Maybe SlideData
findSlide (State st) number = index st.slides (number - 1)


navigateToSlideWith ∷ (Int → Int) → Route → DOMEvent → Maybe Event
navigateToSlideWith f route event = createNavigate ∘ toURL <$> changeSlideNumber route
  where createNavigate ∷ String → Event
        createNavigate = flip Navigate event

        changeSlideNumber ∷ Route → Maybe Route
        changeSlideNumber (Slide projectName slideNumber) = Just $ Slide projectName $ f slideNumber
        changeSlideNumber _ = Nothing
