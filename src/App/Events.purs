module App.Events where

import Prelude
import App.Prelude
import App.Effects (AppEffects)
import App.Routes (Route(..), match, toURL)
import App.State (State(..), SlideData(..))
import App.Util (findSlide)
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Eff.Class (liftEff)
import DOM.Event.Event (preventDefault)
import DOM.HTML (window)
import DOM.HTML.Window (history, document)
import DOM.HTML.History (pushState, DocumentTitle(..), URL(..))
import Control.Monad.Eff.Exception (error)
import DOM.HTML.Document (body)
import DOM.Node.ParentNode (QuerySelector(..), querySelector)
import DOM.Node.Types (elementToParentNode)
import DOM.HTML.Types (htmlElementToElement, readHTMLElement)
import DOM.HTML.HTMLElement (offsetTop)
import DOM.Node.NodeList (item)
import DOM.Node.Element (setScrollTop)
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


-- foldp :: Event -> State -> EffModel State Event (ajax :: AJAX)
foldp (PageView (Presentation name number)) (State st) =
  { state: State st { route = (Presentation name number), loaded = true }
  , effects: [ liftEff do
      case st.loaded of
        true → scrollToPoint
        false → pure Nothing
    ]
  }
  where scrollToPoint = do
          w ← window
          d ← document w
          b ← body d
          pre ← maybe bodyError (findPre ∘ htmlElementToElement) b
          line ← maybe bodyError (findLine ∘ htmlElementToElement) b
          offset ← maybe (throwException $ error $ "line is not present") (elementToHTMLElement >=> offsetTop) line
          maybe (throwException $ error $ "code block is not present") (setScrollTop offset) pre
          pure Nothing

        lineSelector lineNumber = ("code:nth-of-type(2) > span:nth-of-type(" ⊕ (show lineNumber) ⊕ ")")
        bodyError = throwException $ error $ "body is not present"
        findPre body = do
          let b = elementToParentNode body
          querySelector (QuerySelector "pre") b
        findLine body = do
          let slide = index st.slides (number - 1)
          lineNumber ← maybe (throwException $ error $ "slide is not present") (pure ∘ _.lineNumber ∘ unwrap) slide
          let b = elementToParentNode body
          querySelector (QuerySelector $ lineSelector lineNumber) b
        elementAsF e = readHTMLElement $ toForeign e
        -- forError ∷ NonEmptyList ForeignError → 
        combineErrors accum error = accum ⊕ " " ⊕ renderForeignError error
        -- forError ∷ NonEmptyList ForeignError → 
        forError errors = throwException $ error $ foldl combineErrors "" errors
        forValue value = pure value
        elementToHTMLElement e = either forError forValue (runExcept $ elementAsF e)

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
      pushState (toForeign {}) (DocumentTitle "") (URL url) h
    pure $ Just $ PageView $ match url
  ]

navigateToSlideWith ∷ (Int → Int) → State → DOMEvent → Maybe Event
navigateToSlideWith f (State st) event = createNavigate ∘ toURL <$> changeSlideNumber st.route
  where createNavigate ∷ String → Event
        createNavigate = flip Navigate event

        changeSlideNumber ∷ Route → Maybe Route
        changeSlideNumber (Presentation projectName slideNumber)
          | isJust $ findSlide (State st) $ f slideNumber = Just $ Presentation projectName $ f slideNumber
          | otherwise                                     = Nothing

        changeSlideNumber _ = Nothing
