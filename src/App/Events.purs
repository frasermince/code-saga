module App.Events where

import Prelude
import App.Prelude
import App.Routes (Route(..), match, toURL)
import App.State (State(..), SlideData(..))
import App.Util (findSlide)
import Control.Monad.Eff.Class (liftEff)
import DOM.Event.Event (preventDefault)
import DOM.HTML (window)
import DOM.HTML.History (DocumentTitle(..), URL(..), pushState)
import DOM.HTML.Window (history)
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..), isNothing, isJust)
import Pux (EffModel, noEffects, onlyEffects, mapEffects)
import Pux.DOM.Events (DOMEvent)
import Data.Array (index)
import Data.Newtype (unwrap, wrap)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import App.PresentationEvents as PE
import App.Effects (AppEffects)

data Event = PageView Route
           | PreviousSlide DOMEvent
           | NextSlide DOMEvent
           | Navigate String DOMEvent
           | PresentationEvent PE.Event


foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView (Presentation name number)) st =
  onlyEffects st [ do
    pure unit
    pure $ Just $ PresentationEvent $ PE.PageView name number
  ]
foldp (PageView route) (State st) = noEffects $ State st { route = route, loaded = true }

foldp (PresentationEvent event) st = PE.foldp event st # mapEffects PresentationEvent

foldp (PreviousSlide ev) (State st) =
  onlyEffects (State st) [ do
    liftEff do
      preventDefault ev
    pure $ navigateToSlideWith decrement (State st) ev
  ]
  where decrement = flip sub 1

foldp (NextSlide ev) (State st) =
  onlyEffects (State st) [ do
    liftEff do
      preventDefault ev
    pure $ navigateToSlideWith increment (State st) ev
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
        shouldRedirect (Presentation _ number) state = isNothing $ findSlide state number
        shouldRedirect _ _ = false





navigateToSlideWith ∷ (Int → Int) → State → DOMEvent → Maybe Event
navigateToSlideWith f (State st) event = createNavigate ∘ toURL <$> changeSlideNumber st.route
  where createNavigate ∷ String → Event
        createNavigate = flip Navigate event

        changeSlideNumber ∷ Route → Maybe Route
        changeSlideNumber (Presentation projectName slideNumber)
          | isJust $ findSlide (State st) $ f slideNumber = Just $ Presentation projectName $ f slideNumber
          | otherwise                                     = Nothing

        changeSlideNumber _ = Nothing
