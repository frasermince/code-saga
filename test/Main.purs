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
import Selenium.Monad (get, setWindowSize, byId, findElement, byClassName, getText, clickEl, startSpying)
import Test.Feature (ConcreteFeature, ConcreteEffects, Config, TestEffects)
import Test.Scenario (scenario)
import Text.Chalky (green, red, yellow)
import Data.Time.Duration (Milliseconds(..))
import Test.Interaction (expectChangeOnClick, expectNoChangeOnClick, clickElement, expectToEqual, getElementText)
import Server as Server
import App.State (init)


-- import CodeSaga.Server.Test (launchServer)
-- import Control.Monad.Eff (Eff)


-- main ∷ Eff (TestEffects) Unit
main = do
  void $ runAff errHandler (const $ Process.exit 0) do
    log $ yellow "Starting tests"
    -- _ ← liftEff' Server.testMain
    testResults ← attempt $ runTests $ {selenium: {waitTime: Milliseconds(60.0)}}
    case testResults of
      Left e → throwError e
      Right _ → log $ green "Tests passed!"
  where
    errHandler e = do
      Ec.log $ red $ message e
      traverse_ (Ec.log ∘ red) $ S.split (S.Pattern "\n") $ fromMaybe "" $ stack e
      Process.exit 1

runTests ∷ ∀ e. Config → Aff (ConcreteEffects e) Unit
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

openSite ∷ ∀ e. ConcreteFeature e Unit
openSite = do
  get "http://localhost:3000/presentation/project_name/2"
  startSpying

closeSite ∷ ∀ e. ConcreteFeature e Unit
closeSite = pure unit

testScenario
  ∷ ∀ e. ConcreteFeature e Unit
   → String
   → Array String
   → ConcreteFeature e Unit
   → ConcreteFeature e Unit
testScenario = scenario "Concrete Test" (openSite)
tests ∷ ∀ e. ConcreteFeature e Unit
tests = do
  testScenario closeSite "View First Slide Of Code" [] do
    expectElementPresent "presentation"

  testScenario closeSite "Clicking Next Changes Contents" [] do
    expectChangeOnClick "presentation" "next"

  testScenario closeSite "Clicking Previous Changes Contents" [] do
    expectChangeOnClick "presentation" "previous"

  testScenario closeSite "Clicking Previous Then Next yields the same contents" [] do
    beforeText ← getElementText "presentation"
    clickElement "previous"
    clickElement "next"
    afterText ← getElementText "presentation"
    expectToEqual beforeText afterText

  testScenario closeSite "Clicking Previous On First Slide Does Nothing" [] do
    clickElement "previous"
    expectNoChangeOnClick "presentation" "previous"

  testScenario closeSite "Clicking Next On Last Slide Does Nothing" [] do
    clickElement "next"
    expectNoChangeOnClick "presentation" "next"

  testScenario closeSite "Going To Invalid Url Should Redirect To Not Found" ["Currently does not work"] do
    get "http://localhost:3000/presentation/project_name/5"
    expectElementNotPresent "next"


expectElementNotPresent ∷ ∀ e. String → ConcreteFeature e Unit
expectElementNotPresent klass = byClassName klass >>= findElement >>= elementExists
  where elementExists (Just x) = throwError $ error $ "Element exists with class: " ⊕ klass
        elementExists Nothing = pure unit

expectElementPresent ∷ ∀ e. String → ConcreteFeature e Unit
expectElementPresent klass = byClassName klass >>= findElement >>= elementExists
  where elementExists (Just x) = pure unit
        elementExists Nothing = throwError $ error $ "Element does not exist with class: " ⊕ klass
