module Client where

import App.Prelude
import Data.Show (show)
import App.Effects (ClientEffects)
import App.Events (Event(..), foldp)
import Control.Monad.Eff.Console (log)
import App.Routes (match)
import Data.Foldable (foldl)
import App.State (State, initWithSlides, emptyPres)
import App.View.Layout (view)
import Control.Applicative (pure)
import Control.Bind ((=<<), discard, bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (HISTORY)
import Data.Either (either)
import Data.Foreign (Foreign)
import Data.Foreign.Generic (defaultOptions, genericDecode)
import Data.Function (id, ($))
import Pux (CoreEffects, App, start)
import Pux.DOM.Events (DOMEvent)
import Pux.DOM.History (sampleURL)
import Pux.Renderer.React (renderToDOM)
import Signal ((~>))

type WebApp = App (DOMEvent -> Event) Event State

type ClientEffects = CoreEffects (AppEffects (history :: HISTORY, dom :: DOM))

main ∷ String -> State -> Eff ClientEffects WebApp
main url state = do
  -- | Create a signal of URL changes.
  urlSignal <- sampleURL =<< window

  -- | Map a signal of URL changes to PageView actions.
  let routeSignal = urlSignal ~> \r -> PageView (match r)

  -- | Start the app.
  app <- start
    { initialState: state
    , view
    , foldp
    , inputs: [routeSignal] }

  -- | Render to the DOM
  renderToDOM "#app" app.markup app.input

  -- | Return app to be used for hot reloading logic in support/client.entry.js
  pure app

-- | Used to serialize State from JSON in support/client.entry.js
readState :: Foreign -> State
readState json = either (\_ -> initWithSlides "/" emptyPres) id $ runExcept (genericDecode (defaultOptions { unwrapSingleConstructors = true }) json)
