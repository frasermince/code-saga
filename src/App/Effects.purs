module App.Effects where
import Network.HTTP.Affjax (AJAX)
import DOM (DOM)
import DOM.HTML.Types (HISTORY)
import Node.FS (FS)

type AppEffects fx = (ajax :: AJAX, dom ∷ DOM, history ∷ HISTORY, fs :: FS | fx)
