module Test.Scenario where

import App.Prelude
import Prelude
import Test.Feature   (Feature)
import Data.String    (joinWith)
import Test.Util      (warnMsg, logCurrentScreen, sectionMsg)
import Selenium.Monad (attempt)
import Data.Either    (Either(..))
import Control.Monad.Eff.Class     (liftEff)
import Control.Monad.Eff.Exception (throw, message)

type EpicTitle = String
type Hook eff o = Feature eff o
type ScenarioTitle = String
type KnownIssueUrl = String
type FilePath = String

scenario
  ∷ ∀ eff o
  . EpicTitle
  → Hook eff o Unit
  → Hook eff o Unit
  → ScenarioTitle
  → Array KnownIssueUrl
  → Feature eff o Unit
  → Feature eff o Unit
scenario epic before after title knownIssues actions =
  sectionMsg title' *> runHook before *> actions'
  where
  title' ∷ String
  title' = epic <> ": " <> title

  knownIssuesString ∷ String
  knownIssuesString = separate $ indent <$> knownIssues

  knownIssuesWarning ∷ String
  knownIssuesWarning = "These known issues caused this scenario to fail:\n" <> knownIssuesString

  knownIssuesHookWarning = "These known issues caused this scenario hook to fail:\n" <> knownIssuesString

  separate ∷ Array String → String
  separate = joinWith "\n"

  indent ∷ String → String
  indent s = "  " <> s

  warning ∷ String → String
  warning s = "Warning: " <> s

  warn ∷ String → Feature eff o Unit
  warn = warnMsg ∘ warning

  unexpectedSuccess ∷ Feature eff o Unit
  unexpectedSuccess =
    warn
      $ "Ok despite known issues, if these issues are resolved please remove them\n"
      <> knownIssuesString

  actions' ∷ Feature eff o Unit
  actions' | knownIssues ≡ [] =
    attempt actions >>=
      case _ of
        Left e → logCurrentScreen *> (liftEff $ throw $ message e)
        Right _ → runHook after
  actions' =
    attempt actions >>=
      case _ of
        Left e → logCurrentScreen *> warn (message e) *> warn knownIssuesWarning *> runHook after
        Right _ → runHook after *> unexpectedSuccess

  runHook ∷ Feature eff o Unit → Feature eff o Unit
  runHook hook | knownIssues == [] =
    attempt hook >>=
      case _ of
        Left e → logCurrentScreen *> (liftEff $ throw $ message e)
        Right _ → pure unit
  runHook hook =
    attempt hook >>=
      case _ of
        Left e → logCurrentScreen *> warn (message e) *> warn knownIssuesHookWarning
        Right _ → pure unit
