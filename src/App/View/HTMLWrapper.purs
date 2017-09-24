module App.View.HTMLWrapper where

import App.Config (config)
import Control.Bind (discard)
import Data.Function (($))
import Data.Monoid (mempty)
import Data.Semigroup ((<>))
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (key)
import Pux.Renderer.React (dangerouslySetInnerHTML)
import Text.Smolder.HTML (body, div, head, html, link, meta, script, title)
import Text.Smolder.HTML.Attributes (charset, content, href, id, name, rel, src, type')
import Text.Smolder.Markup (text, (!), MarkupM(..), Markup)
import Control.Monad.Free (liftF)
import Data.Unit (unit, Unit)

-- empty :: Markup Unit
empty = liftF $ Empty unit

htmlWrapper :: ∀ ev. String -> String -> HTML ev
htmlWrapper app_html state_json =
  html do
    head do
      meta ! charset "UTF-8"
      meta ! name "viewport" ! content "width=device-width, initial-scale=1"
      -- meta ! name "viewport" ! content "width= 1281.590, user-scalable=no, maximum-scale=1"
      title $ text config.title
      link ! rel "icon" ! type' "image/x-icon" ! href "/favicon.ico"
    body do
      div ! dangerouslySetInnerHTML "<style>button{text-decoration: none; outline: none; cursor: pointer;}</style>" $ empty
      div ! key "app" ! id "app" ! dangerouslySetInnerHTML app_html $ empty
      script ! key "initial_state" ! type' "text/javascript" ! dangerouslySetInnerHTML state_json $ empty
      script ! key "js_bundle" ! type' "text/javascript" ! src (config.public_path <> "/bundle.js.gz") $ empty
