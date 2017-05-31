module Test.Feature where
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.Process                (PROCESS)
import Signal.Channel              (CHANNEL)
import Node.Buffer                 (BUFFER)
import Node.FS                     (FS)
import Selenium.Monad              (Selenium)
import Data.Time.Duration          (Milliseconds(..))
import Control.Monad.Eff.Console   as Ec
import DOM                         (DOM)
import DOM.HTML.Types              (HISTORY)
import Control.Monad.Eff.Ref       (REF)
import Selenium.Types              (SELENIUM, Locator, Element)
import Network.HTTP.Affjax         (AJAX)

type FeatureEffects eff =
  ( exception ∷ EXCEPTION
  , process ∷ PROCESS
  , buffer ∷ BUFFER
  , fs ∷ FS
  | eff)

type Feature eff o = Selenium (FeatureEffects eff) o

type Config = { selenium ∷ {waitTime ∷ Milliseconds} }

type ConcreteEffects eff = FeatureEffects
  ( console ∷ Ec.CONSOLE
  , dom ∷ DOM
  , ref ∷ REF
  , selenium ∷ SELENIUM
  | eff)

type TestEffects = ConcreteEffects
  ( history ∷ HISTORY
  , ajax ∷ AJAX
  , channel ∷ CHANNEL
  )

type ConcreteFeature e = Feature e (config ∷ Config)
