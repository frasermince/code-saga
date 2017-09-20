module App.View.Layout where

import App.Prelude
import App.Config (config)
import App.View.Homepage as Homepage
import Data.NonEmpty ((:|))
import App.View.NotFound as NotFound
import App.View.Presentation as Presentation
import App.Routes (Route(NotFound, Home, Presentation))
import App.State (State(..))
import App.Util (findSlide)
import App.Events (Event)
import CSS (CSS)
import CSS.Stylesheet (fontFace)
import CSS.FontFace (fontFaceFamily, fontFaceSrc, FontFaceSrc(..), FontFaceFormat(..))
import Control.Bind (discard)
import Data.Function (($))
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!))
import Data.Maybe (maybe, Maybe(..))

view :: State -> HTML Event
view (State st) = do
  style styleSheet
  div ! className "app" $ do
    case st.route of
      (Home) -> Homepage.view (State st)
      (Presentation name number) -> goToPresentation name number
      (NotFound url) -> notFound
  where goToPresentation name number = maybe notFound (Presentation.view) (findSlide (State st) name number)
        notFound = NotFound.view (State st)


styleSheet ∷ CSS
styleSheet = do
  fontFace do
    fontFaceFamily "Alcubierre"
    fontFaceSrc $ (FontFaceSrcUrl (config.public_path ⊕ "/Alcubierre.tff") (Just TrueType)) :| [FontFaceSrcUrl (config.public_path ⊕ "/Alcubierre.woff") (Just WOFF)]
