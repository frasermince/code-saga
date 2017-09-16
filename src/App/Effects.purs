module App.Effects where
import Network.HTTP.Affjax (AJAX)
import DOM (DOM)
import DOM.HTML.Types (HISTORY)
import Node.FS (FS)
import Pux (CoreEffects)
import Control.Monad.Eff.Console (CONSOLE)

type AppEffects fx = (ajax :: AJAX, dom ∷ DOM, history ∷ HISTORY, fs :: FS, console ∷ CONSOLE | fx)

type ClientEffects = CoreEffects (AppEffects ())
