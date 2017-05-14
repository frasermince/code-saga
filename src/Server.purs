module Server where

import Prelude
import App.Events (Event(..), foldp)
import App.Effects (AppEffects)
import App.Routes (Route(..), match)
import App.State (State(..),  initWithSlides, defaultSlides, PreFetchSlide(..), transform)
import App.View.HTMLWrapper (htmlWrapper)
import App.View.Layout (view)
import Control.IxMonad (ibind)
import Control.Monad.Aff.Class (liftAff, class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff, class MonadEff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Int (fromString)
import Data.Foreign.Generic (defaultOptions, genericEncodeJSON)
import Data.Maybe (fromMaybe)
import Data.Newtype (un, unwrap)
import Hyper.Node.Server (runServer, defaultOptionsWithLogging)
import Hyper.Port (Port(Port))
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware, lift')
import Hyper.Node.FileServer (fileServer)
import Hyper.Request (class Request, getRequestData)
import Hyper.Response (class Response, class ResponseWritable, ResponseEnded, StatusLineOpen, closeHeaders, respond, writeStatus)
import Hyper.Status (statusNotFound, statusOK)
import Node.Buffer (BUFFER)
import Node.FS (FS)
import Node.HTTP (HTTP)
import Node.Process (PROCESS, lookupEnv)
import Pux (CoreEffects, start, waitState)
import Pux.Renderer.React (renderToString, renderToStaticMarkup)
import Data.Array (null)
import Signal (constant)

appHandler
  :: forall m req res c b e
  .  Monad m
  => MonadEff (CoreEffects (AppEffects e)) m
  => MonadAff (CoreEffects (AppEffects e)) m
  => Request req m
  => Response res m b
  => ResponseWritable b m String
  => Array PreFetchSlide →
     Middleware
     m
     (Conn req (res StatusLineOpen) c)
     (Conn req (res ResponseEnded) c)
     Unit
appHandler slides = do
  request ← getRequestData

  let prefetch = if (null slides) then defaultSlides else slides
  slideContents ← lift' $ liftEff $ transform prefetch

  -- slideContents ← liftEff $ _.slides $ unwrap $ state

  app ← liftEff $ start
    { initialState: initWithSlides request.url slideContents
    , view
    , foldp
    , inputs: [constant (PageView (match request.url))]
    }

  state <- lift' $ liftAff $ waitState (\(State st) -> st.loaded) app
  let state_json = "window.__puxInitialState = "
                 <> (genericEncodeJSON (defaultOptions { unwrapSingleConstructors = true }) state)
                 <> ";"

  -- | Set proper response status
  _ <- writeStatus $ case ((un State state).route) of
        (NotFound _) -> statusNotFound
        _ -> statusOK
  _ <- closeHeaders

  -- | Inject state JSON into HTML response.
  app_html <- lift' $ liftEff $ renderToString app.markup
  html <- lift' $ liftEff $ renderToStaticMarkup $ constant (htmlWrapper app_html state_json)

  respond html
  where bind = ibind

-- | Starts server (for development).
main :: Eff (CoreEffects (AppEffects (buffer :: BUFFER, fs :: FS, http :: HTTP, console :: CONSOLE, process :: PROCESS))) Unit
main = testMain []
testMain :: ∀ e. Array PreFetchSlide → Eff (CoreEffects (AppEffects (buffer :: BUFFER, fs :: FS, http :: HTTP, console :: CONSOLE, process :: PROCESS | e))) Unit
testMain slides = do
  port <- (fromMaybe 0 <<< fromString <<< fromMaybe "3000") <$> lookupEnv "PORT"
  let app = fileServer "static" $ appHandler slides
  runServer (defaultOptionsWithLogging { port = Port port }) {} app
