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

expectNoChangeOnClick ∷ String → String → ConcreteFeature Unit
expectNoChangeOnClick = expectChangeOnFunction expectToEqual
expectChangeOnClick ∷ String → String → ConcreteFeature Unit
expectChangeOnClick = expectChangeOnFunction expectToNotEqual
expectChangeOnFunction ∷ (String → String → ConcreteFeature Unit) → String → String → ConcreteFeature Unit
expectChangeOnFunction f contentElement buttonElement = do
  beforeClick ← getElementText contentElement
  clickElement buttonElement
  afterClick ← getElementText contentElement
  f beforeClick afterClick


expectToEqual ∷ ∀ a. Show a ⇒ Eq a ⇒ a → a → ConcreteFeature Unit
expectToEqual = expectCompare (≡)

expectToNotEqual ∷ ∀ a. Show a ⇒ Eq a ⇒ a → a → ConcreteFeature Unit
expectToNotEqual = expectCompare (≢)

expectCompare ∷ ∀ a. Show a ⇒ Eq a ⇒ (a → a → Boolean ) → a → a → ConcreteFeature Unit
expectCompare f a b | f a b     = pure unit
                    | otherwise = throwError $ error $ "Expected comparison of " ⊕ (show a) ⊕ " and " ⊕ (show b) ⊕ " to be true."
