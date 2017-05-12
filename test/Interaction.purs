module Test.Interaction where

import Prelude
import App.Prelude
import Selenium.Monad              (findElement, byClassName, getText, clickEl)
import Selenium.Types              (Element)
import Test.Feature                (ConcreteFeature)
import Data.Maybe                  (Maybe(..))
import Control.Monad.Error.Class   (throwError)
import Control.Monad.Eff.Exception (error)

clickElement ∷ ∀ e. String → ConcreteFeature e Unit
clickElement className = do
  element ← getElement className
  clickEl element

getElementText ∷ ∀ e. String → ConcreteFeature e String
getElementText className = do
  element ← getElement className
  getText element

getElement ∷ ∀ e. String → ConcreteFeature e Element
getElement className = do
  locator ← byClassName className
  element ← findElement locator
  handleMaybe element

    where handleMaybe ∷ ∀ e. Maybe Element → ConcreteFeature e Element
          handleMaybe (Just e) = pure e
          handleMaybe Nothing = throwError $ error $ "Element with class: " ⊕ className ⊕ " is not present"

expectNoChangeOnClick ∷ ∀ e. String → String → ConcreteFeature e Unit
expectNoChangeOnClick = expectChangeOnFunction expectToEqual
expectChangeOnClick ∷ ∀ e. String → String → ConcreteFeature e Unit
expectChangeOnClick = expectChangeOnFunction expectToNotEqual
expectChangeOnFunction ∷ ∀ e. (String → String → ConcreteFeature e Unit) → String → String → ConcreteFeature e Unit
expectChangeOnFunction f contentElement buttonElement = do
  beforeClick ← getElementText contentElement
  clickElement buttonElement
  afterClick ← getElementText contentElement
  f beforeClick afterClick

expectTextToEqual ∷ ∀ e. String → String → ConcreteFeature e Unit
expectTextToEqual klass expected = (getElementText klass) >>= (expectToEqual expected)


expectToEqual ∷ ∀ a e. Show a ⇒ Eq a ⇒ a → a → ConcreteFeature e Unit
expectToEqual = expectCompare (≡)

expectToNotEqual ∷ ∀ a e. Show a ⇒ Eq a ⇒ a → a → ConcreteFeature e Unit
expectToNotEqual = expectCompare (≢)

expectCompare ∷ ∀ a e. Show a ⇒ Eq a ⇒ (a → a → Boolean ) → a → a → ConcreteFeature e Unit
expectCompare f a b | f a b     = pure unit
                    | otherwise = throwError $ error $ "Expected comparison of " ⊕ (show a) ⊕ " and " ⊕ (show b) ⊕ " to be true."
