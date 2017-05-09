module Test.Interaction where

import Prelude
import App.Prelude
import Selenium.Monad              (findElement, byClassName, getText, clickEl)
import Selenium.Types              (Element)
import Test.Feature                (ConcreteFeature)
import Data.Maybe                  (Maybe(..))
import Control.Monad.Error.Class   (throwError)
import Control.Monad.Eff.Exception (error)

clickElement ∷ String → ConcreteFeature Unit
clickElement className = do
  element ← getElement className
  clickEl element

getElementText ∷ String → ConcreteFeature String
getElementText className = do
  element ← getElement className
  getText element

getElement ∷ String → ConcreteFeature Element
getElement className = do
  locator ← byClassName className
  element ← findElement locator
  handleMaybe element

    where handleMaybe ∷ Maybe Element → ConcreteFeature Element
          handleMaybe (Just e) = pure e
          handleMaybe Nothing = throwError $ error $ "Element with class: " ⊕ className ⊕ " is not present"

expectChangeOnClick ∷ String → String → ConcreteFeature Unit
expectChangeOnClick contentElement buttonElement = do
  beforeClick ← getElementText contentElement
  clickElement buttonElement
  afterClick ← getElementText contentElement
  expectToNotEqual beforeClick afterClick


expectToNotEqual ∷ ∀ a. Show a ⇒ Eq a ⇒ a → a → ConcreteFeature Unit
expectToNotEqual a b | a ≡ b     = throwError $ error $ "Expected " ⊕ (show a) ⊕ " to not equal " ⊕ (show b)
                     | otherwise = pure unit
