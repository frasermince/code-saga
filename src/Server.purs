module Server where

import Prelude
import Data.Tuple (Tuple(..))
import App.Prelude
import App.Events (Event(..), foldp)
import App.Effects (AppEffects)
import App.Routes (Route(..), match)
import App.State (State(..),  initWithSlides, SlideData(..), transform, Presentations(..))
import App.Presentations.MultiplyMe as MultiplyMe
import App.Presentations.Mal as Mal
import App.View.HTMLWrapper (htmlWrapper)
import Data.Array as List
import App.View.Layout (view)
import Control.IxMonad (ibind)
import Control.Monad.Aff.Class (liftAff, class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff, class MonadEff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.String (length)
import Data.Int (fromString)
import Data.Foreign.Generic (defaultOptions, genericEncodeJSON)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Newtype (un, unwrap)
import Hyper.Node.Server (runServer, defaultOptionsWithLogging)
import Hyper.Node.Server.Options (Port(Port))
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware, lift')
import Hyper.Node.FileServer (fileServer)
import Hyper.Request (class Request, getRequestData)
import Hyper.Response (class Response, class ResponseWritable, ResponseEnded, StatusLineOpen, closeHeaders, respond, writeStatus, writeHeader)
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
  => Maybe Presentations →
     Middleware
     m
     (Conn req (res StatusLineOpen) c)
     (Conn req (res ResponseEnded) c)
     Unit
appHandler presentations = do
  request ← getRequestData

  let defaultPres = Presentations {multiplyMe: MultiplyMe.presentation, mal: Mal.presentation}
  let prefetch = fromMaybe defaultPres presentations
  -- slideContents ← lift' $ liftEff $ transform prefetch

  -- slideContents ← liftEff $ _.slides $ unwrap $ state

  app ← liftEff $ start
    { initialState: initWithSlides request.url prefetch
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
  -- _ <- writeHeader (Tuple "Content-Encoding" "gzip")
  _ <- closeHeaders

  -- | Inject state JSON into HTML response.
  app_html <- lift' $ liftEff $ renderToString app.markup
  html <- lift' $ liftEff $ renderToStaticMarkup $ constant (htmlWrapper app_html state_json)

  respond html
  where bind = ibind

-- | Starts server (for development).
main :: Eff (CoreEffects (AppEffects (buffer :: BUFFER, fs :: FS, http :: HTTP, console :: CONSOLE, process :: PROCESS))) Unit
main = testMain Nothing
testMain :: ∀ e. Maybe Presentations → Eff (CoreEffects (AppEffects (buffer :: BUFFER, fs :: FS, http :: HTTP, console :: CONSOLE, process :: PROCESS | e))) Unit
testMain p = do
  port <- (fromMaybe 0 <<< fromString <<< fromMaybe "3000") <$> lookupEnv "PORT"
  let h = [ Tuple "Content-Encoding" "gzip" ]
  let app = fileServer "static" (appHandler p) h
  runServer (defaultOptionsWithLogging { port = Port port }) {} app
