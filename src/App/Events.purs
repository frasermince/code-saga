module App.Events where

import Prelude
import App.Prelude
import App.Routes (Route(..), match, toURL)
import App.State (State(..))
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.HTML (window)
import DOM.HTML.History (DocumentTitle(..), URL(..), pushState)
import DOM.HTML.Types (HISTORY)
import DOM.HTML.Window (history)
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects, onlyEffects)
import Pux.DOM.Events (DOMEvent)

data Event = PageView Route
           | PreviousSlide DOMEvent
           | NextSlide DOMEvent
           | Navigate String DOMEvent

type AppEffects fx = (ajax :: AJAX, dom ∷ DOM, history ∷ HISTORY | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) = noEffects $ State st { route = route, loaded = true }

foldp (PreviousSlide ev) (State st) =
  onlyEffects (State st) [ do
    liftEff do
      preventDefault ev
    pure $ navigateToSlideWith (flip sub 1) st.route ev
  ]

foldp (NextSlide ev) (State st) =
  onlyEffects (State st) [ do
    liftEff do
      preventDefault ev
    pure $ navigateToSlideWith (add 1) st.route ev
  ]

foldp (Navigate url ev) state =
  onlyEffects state [ do
    liftEff do
      preventDefault ev
      h <- history =<< window
      pushState (toForeign {}) (DocumentTitle "") (URL url) h
    pure $ Just $ PageView (match url)
  ]

navigateToSlideWith ∷ (Int → Int) → Route → DOMEvent → Maybe Event
navigateToSlideWith f route event = createNavigate ∘ toURL <$> changeSlideNumber route
  where createNavigate ∷ String → Event
        createNavigate = flip Navigate event

        changeSlideNumber ∷ Route → Maybe Route
        changeSlideNumber (Slide projectName slideNumber) = Just $ Slide projectName $ f slideNumber
        changeSlideNumber _ = Nothing
