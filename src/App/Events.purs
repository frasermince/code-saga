module App.Events where

import Prelude
import App.Prelude
import App.Effects (ClientEffects)
import App.Routes (Route(..), match, toURL)
import App.State (State(..), SlideData(..))
import App.Util (findSlide)
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.HTML (window)
import DOM.HTML.Window (history, document)
import DOM.HTML.History (pushState, DocumentTitle(..), URL(..))
import Signal.Channel (CHANNEL)
import Control.Monad.Eff.Exception (error, EXCEPTION)
import DOM.HTML.Document (body)
import DOM.Node.ParentNode (QuerySelector(..), querySelector)
import DOM.Node.Types (elementToParentNode)
import DOM.HTML.Types (htmlElementToElement, readHTMLElement, HISTORY)
import DOM.HTML.HTMLElement (offsetTop)
import DOM.Node.NodeList (item)
import DOM.Node.Element (setScrollTop, clientHeight)
import Data.Array (index)
import Data.Foreign (toForeign, renderForeignError)
import Data.Foldable (foldl)
import Data.Either (either)
import Control.Monad.Except (runExcept)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Maybe (Maybe(..), isJust, isNothing, maybe')
import Data.Maybe (maybe)
import Data.Newtype (unwrap, wrap)
import Pux (EffModel, noEffects, onlyEffects, mapEffects)
import Pux.DOM.Events (DOMEvent)

data Event = PageView Route
           | PreviousSlide DOMEvent
           | NextSlide DOMEvent
           | Navigate String DOMEvent

-- foldp :: Event -> State -> EffModel State Event ClientEffects
foldp (PageView route) (State st) = noEffects $ State st { route = route, loaded = true }

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
      preventDefault ev
      pushState (toForeign {}) (DocumentTitle "") (URL url) h
    pure $ Just $ PageView $ match url
  ]

navigateToSlideWith ∷ (Int → Int) → State → DOMEvent → Maybe Event
navigateToSlideWith f (State st) event = createNavigate ∘ toURL <$> changeSlideNumber st.route
  where createNavigate ∷ String → Event
        createNavigate = flip Navigate event

        changeSlideNumber ∷ Route → Maybe Route
        changeSlideNumber (Presentation projectName slideNumber)
          | isJust $ findSlide (State st) projectName $ f slideNumber = Just $ Presentation projectName $ f slideNumber
          | otherwise                                     = Nothing

        changeSlideNumber _ = Nothing
