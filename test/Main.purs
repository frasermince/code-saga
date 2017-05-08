module Test.Main where

import Prelude
import App.Prelude
import Control.Monad.Eff.Console as Ec
import Data.String as S
import Node.Process as Process
import Control.Monad.Aff (Aff, attempt, apathize, runAff, liftEff')
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (throw, message, stack, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe, Maybe(..))
import Selenium (quit)
import Selenium.Browser (Browser(..), browserCapabilities)
import Selenium.Builder (withCapabilities, build)
import Selenium.Monad (get, setWindowSize, byId, findElement, byClassName, getText, clickEl)
import Test.Feature (ConcreteFeature, ConcreteEffects, Config)
import Test.Scenario (scenario)
import Text.Chalky (green, red, yellow)
import Data.Time.Duration (Milliseconds(..))
import Test.Interaction (clickElement, getElementText)


-- import CodeSaga.Server.Test (launchServer)
-- import Control.Monad.Eff (Eff)


main ∷ Eff (ConcreteEffects) Unit
main = do
  void $ runAff errHandler (const $ Process.exit 0) do
    log $ yellow "Starting tests"
    testResults ← attempt $  runTests $ {selenium: {waitTime: Milliseconds(60.0)}}
    case testResults of
      Left e → throwError e
      Right _ → log $ green "Tests passed!"
  where
    errHandler e = do
      Ec.log $ red $ message e
      traverse_ (Ec.log ∘ red) $ S.split (S.Pattern "\n") $ fromMaybe "" $ stack e
      Process.exit 1

runTests ∷ Config → Aff ConcreteEffects Unit
runTests config = do
  driver ← build do
    withCapabilities $ browserCapabilities Chrome

  let
    defaultTimeout = config.selenium.waitTime
    readerInp = { config, defaultTimeout, driver }

  res ← attempt $ flip runReaderT readerInp do
    setWindowSize { height: 800, width: 1024 }
    tests

  apathize $ quit driver
  either throwError (const $ pure unit) res

openSite ∷ ConcreteFeature Unit
openSite = get "http://localhost:3000/presentation/project_name/1"

closeSite ∷ ConcreteFeature Unit
closeSite = pure unit

testScenario
  ∷ ConcreteFeature Unit
   → String
   → Array String
   → ConcreteFeature Unit
   → ConcreteFeature Unit
testScenario = scenario "Concrete Test" (openSite)
tests ∷ ConcreteFeature Unit
tests = do
  testScenario closeSite "View First Slide Of Code" [] do
    expectElementPresent "presentation"
  testScenario closeSite "Clicking Next Changes Contents" [] do
    expectChangeOnClick "presentation" "next"

expectToNotEqual ∷ ∀ a. Show a ⇒ Eq a ⇒ a → a → ConcreteFeature Unit
expectToNotEqual a b | a ≡ b     = throwError $ error $ "Expected " ⊕ (show a) ⊕ " but got " ⊕ (show b)
                     | otherwise = pure unit

expectChangeOnClick ∷ String → String → ConcreteFeature Unit
expectChangeOnClick contentElement buttonElement = do
  beforeClick ← getElementText contentElement
  clickElement buttonElement
  afterClick ← getElementText contentElement
  expectToNotEqual beforeClick afterClick

expectElementPresent ∷ String → ConcreteFeature Unit
expectElementPresent klass = byClassName klass >>= findElement >>= elementExists
  where elementExists (Just x) = pure unit
        elementExists Nothing = throwError $ error $ "Element does not exist with class: " ⊕ klass
