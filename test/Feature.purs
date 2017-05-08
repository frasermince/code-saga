module Test.Feature where
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.Process                (PROCESS)
import Node.Buffer                 (BUFFER)
import Node.FS                     (FS)
import Selenium.Monad              (Selenium)
import Data.Time.Duration          (Milliseconds(..))
import Control.Monad.Eff.Console   as Ec
import DOM                         (DOM)
import Control.Monad.Eff.Ref       (REF)
import Selenium.Types              (SELENIUM, Locator, Element)

type FeatureEffects eff =
  ( exception ∷ EXCEPTION
  , process ∷ PROCESS
  , buffer ∷ BUFFER
  , fs ∷ FS
  | eff)

type Feature eff o = Selenium (FeatureEffects eff) o

type Config = { selenium ∷ {waitTime ∷ Milliseconds} }

type ConcreteEffects = FeatureEffects
  (   console ∷ Ec.CONSOLE
    , dom ∷ DOM
    , ref ∷ REF
    , selenium ∷ SELENIUM
  )

type ConcreteFeature = Feature () (config ∷ Config)
