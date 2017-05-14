module Test.Interaction where

import Prelude
import App.Prelude
import Selenium.Monad              (findElement, byClassName, getText, clickEl)
import Selenium.Types              (Element)
import Test.Feature                (ConcreteFeature)
import Data.Maybe                  (Maybe(..), fromMaybe)
import Control.Monad.Error.Class   (throwError)
import Control.Monad.Eff.Exception (error)
import Data.String (toCharArray, stripPrefix, Pattern(..), trim)
import Data.Array (length, unzip, zip, dropWhile)
import Data.Tuple (fst, snd, Tuple)
import Data.Char (toCharCode)

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


expectToEqual ∷ ∀ e. String → String → ConcreteFeature e Unit
expectToEqual = expectCompare id

-- debugging marker string = trace (marker ⊕ codeArray ) (\_ → codeArray)
--   where codeArray = show $ toCharCode <$> (toCharArray $ show string)

expectToNotEqual ∷ ∀ e. String → String → ConcreteFeature e Unit
expectToNotEqual = expectCompare (not)

expectCompare ∷ ∀ e. (Boolean → Boolean) → String → String → ConcreteFeature e Unit
expectCompare f a b | (f $ (trim a) ≡ (trim b))     = pure unit
                    | otherwise = makeError

  where makeError = throwError $ error $ errorMessage ⊕ findDifference
        errorMessage = "Expected comparison of " ⊕ (show a) ⊕ " and " ⊕ (show b) ⊕ " to be true. Difference being: "
        findDifference |  f $ (length a') ≡ (length b') = (show $ fst compareStrings) ⊕ " and " ⊕ (show $ snd compareStrings)
                       | otherwise = "one string is longer difference is:" ⊕ (fromMaybe "not there" (stripPrefix (Pattern b) a))
        compareStrings = unzip $ dropWhile shouldDrop $ zip a' b'
        shouldDrop ∷ Tuple Char Char → Boolean
        shouldDrop tuple = compareWith (fst tuple) (snd tuple)
        a' = (toCharArray a)
        b' = (toCharArray b)
        compareWith a b = f $ (a ≡ b)
