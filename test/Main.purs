module Test.Main where

import Prelude
import App.Prelude
import Control.Monad.Eff.Console as Ec
import Data.String as S
import Data.Newtype (unwrap)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))
import Node.Process as Process
import Control.Monad.Aff (Aff, attempt, apathize, runAff, liftEff')
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (throw, message, stack, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe, Maybe(..), fromJust)
import Data.Array (foldl)
import Selenium (quit)
import Selenium.Browser (Browser(..), browserCapabilities)
import Selenium.Builder (withCapabilities, build)
import Selenium.Monad (get, setWindowSize, byId, findElement, byClassName, getText, clickEl)
import Test.Feature (ConcreteFeature, ConcreteEffects, Config, TestEffects)
import Test.Scenario (scenario)
import Text.Chalky (green, red, yellow)
import Data.Time.Duration (Milliseconds(..))
import Test.Interaction (expectChangeOnClick, expectNoChangeOnClick, clickElement, expectToEqual, getElementText, expectTextToEqual, getElementTextByXPath)
import Server as Server
import App.State (defaultSlides, SlideData(..), PreFetchSlide(..))


codeSelector =  "//div[@class=\"presentation\"]//code"
tests ∷ ∀ e. ConcreteFeature e Unit
tests = do
  testScenario closeSite "View First Slide Of Code" [] do
    expectElementPresent "presentation"

  testScenario closeSite "Clicking Next Changes Contents" [] do
    expectChangeOnClick "presentation" "next"

  testScenario closeSite "Clicking Previous Changes Contents" [] do
    expectChangeOnClick "presentation" "previous"

  testScenario closeSite "Clicking Previous Then Next yields the same contents" [] do
    beforeText ← getElementTextByXPath codeSelector
    clickElement "previous"
    clickElement "next"
    afterText ← getElementTextByXPath codeSelector
    expectToEqual beforeText afterText

  testScenarioWithOpen (openSlide 1) closeSite "Clicking Previous On First Slide Does Nothing" [] do
    expectNoChangeOnClick "presentation" "previous"

  testScenarioWithOpen (openSlide 3) closeSite "Clicking Next On Last Slide Does Nothing" [] do
    expectNoChangeOnClick "presentation" "next"

  testScenarioWithOpen (openSlide 5) closeSite "Going To Invalid Url Should Redirect To Not Found" [] do
    expectElementNotPresent "next"

  testScenarioWithOpen (openSlide 1) closeSite "The correct slide should show the code from the file associated with it and the annotation" ["Reason"] do
    foldl compareSlide (pure unit) defaultSlides

compareSlide ∷ ∀ e. ConcreteFeature e Unit → PreFetchSlide → ConcreteFeature e Unit
compareSlide accum (PreFetchSlide s) = accum
                                       *> readFromSlideFile
                                       *> expectTextToEqual getElementText "content" s.annotation
                                       *> clickElement "next"
  where readFromSlideFile ∷ ∀ e. ConcreteFeature e Unit
        readFromSlideFile = do
          text ← liftEff $ readTextFile UTF8 s.fileName
          expectTextToEqual getElementTextByXPath codeSelector text

openSlide ∷ ∀ e. Int → ConcreteFeature e Unit
openSlide number = get $ "http://localhost:3000/presentation/project_name/" ⊕ (show number)



testSlides ∷ Array PreFetchSlide
testSlides =
  [
    PreFetchSlide {fileName: "MultiplyMeApi/app/controllers/api/v1/leader_board_controller.rb", lineNumber: 1, annotation: "HI"}
  , PreFetchSlide {fileName: "MultiplyMeApi/app/controllers/api/v1/donations_controller.rb", lineNumber: 1, annotation: "HI"}
  , PreFetchSlide {fileName: "MultiplyMeApi/app/controllers/api/v1/organizations_controller.rb", lineNumber: 1, annotation: "HI"}
  ]
-- main ∷ Eff (TestEffects) Unit
main = do
  void $ runAff errHandler (const $ Process.exit 0) do
    log $ yellow "Starting tests"
    _ ← liftEff' $ Server.testMain defaultSlides
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
  openSlide 2

closeSite ∷ ∀ e. ConcreteFeature e Unit
closeSite = pure unit

testScenario
  ∷ ∀ e. ConcreteFeature e Unit
   → String
   → Array String
   → ConcreteFeature e Unit
   → ConcreteFeature e Unit
testScenario = scenario "Concrete Test" (openSite)
testScenarioWithOpen = scenario "Concrete Test"

expectElementNotPresent ∷ ∀ e. String → ConcreteFeature e Unit
expectElementNotPresent klass = byClassName klass >>= findElement >>= elementExists
  where elementExists (Just x) = throwError $ error $ "Element exists with class: " ⊕ klass
        elementExists Nothing = pure unit

expectElementPresent ∷ ∀ e. String → ConcreteFeature e Unit
expectElementPresent klass = byClassName klass >>= findElement >>= elementExists
  where elementExists (Just x) = pure unit
        elementExists Nothing = throwError $ error $ "Element does not exist with class: " ⊕ klass
