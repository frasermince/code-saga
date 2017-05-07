module Test.Feature where
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.Process                (PROCESS)
import Node.Buffer                 (BUFFER)
import Node.FS                     (FS)
import Selenium.Monad              (Selenium)

type FeatureEffects eff =
  ( exception ∷ EXCEPTION
  , process ∷ PROCESS
  , buffer ∷ BUFFER
  , fs ∷ FS
  | eff)

type Feature eff o = Selenium (FeatureEffects eff) o
