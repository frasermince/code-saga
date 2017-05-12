module App.Effects where
import Network.HTTP.Affjax (AJAX)
import DOM (DOM)
import DOM.HTML.Types (HISTORY)

type AppEffects fx = (ajax :: AJAX, dom ∷ DOM, history ∷ HISTORY | fx)
