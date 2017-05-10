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
import Data.Maybe (Maybe(..), maybe)
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
           | NavigateNotFound

type AppEffects fx = (ajax :: AJAX, dom ∷ DOM, history ∷ HISTORY | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) st =
  onlyEffects appliedState $ [ do
    liftEff do
      pure unit
    pure $ redirect $ appliedState
  ]
  where appliedState = newState route st

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
    pure $ Just $ PageView (match url)
  ]

foldp NavigateNotFound state =
  onlyEffects state [ do
    liftEff do
      h <- history =<< window
      pushState (toForeign {}) (DocumentTitle "") (URL url) h
    pure $ Just $ PageView (match url)
  ]
  where url = "/not_found"


newState ∷ Route → State → State
newState route (State st) = case route of
  Slide name number → makeState (State st) number route
  _ → State st { route = route, loaded = true }

makeState ∷ State → Int → Route → State
makeState st number route = (slideState st route) $ _.fileName ∘ unwrap <$> findSlide st number

slideState ∷ State → Route → Maybe String → State
slideState (State st) route text = State st { route = route, loaded = true, currentSlideContent = wrap text }

findSlide ∷ State → Int → Maybe SlideData
findSlide (State st) number = index st.slides (number - 1)

redirect ∷ State → Maybe Event
redirect state | shouldRedirect state = Just $ NavigateNotFound
               | otherwise = Nothing

shouldRedirect ∷ State → Boolean
shouldRedirect (State { route: Slide _ _, currentSlideContent: (NullOrUndefined Nothing) }) = true
shouldRedirect _ = false

navigateToSlideWith ∷ (Int → Int) → Route → DOMEvent → Maybe Event
navigateToSlideWith f route event = createNavigate ∘ toURL <$> changeSlideNumber route
  where createNavigate ∷ String → Event
        createNavigate = flip Navigate event

        changeSlideNumber ∷ Route → Maybe Route
        changeSlideNumber (Slide projectName slideNumber) = Just $ Slide projectName $ f slideNumber
        changeSlideNumber _ = Nothing
