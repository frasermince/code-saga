module Test.Util where
import Prelude
import App.Prelude ((∘), (⊕))
import Test.Feature                (Feature)
import Node.Path                   (normalize, FilePath)
import Node.Process                as Process
import Control.Monad.Eff.Unsafe          (unsafePerformEff)
import Control.Monad.Eff.Class     (class MonadEff, liftEff)
import Control.Monad.Trans.Class   (lift)
import Control.Monad.Aff.Console   (log)
import Text.Chalky                 (yellow, magenta)
import Selenium.Monad              (saveScreenshot)
import Node.FS.Aff                 (readFile)
import Node.Buffer                 (toString)
import Node.Encoding               (Encoding(..))
import Debug.Trace                 (trace)

appendToCwd
  ∷ ∀ eff m
  . MonadEff (process ∷ Process.PROCESS | eff) m
  ⇒ String
  → m String
appendToCwd s =  liftEff $ normalize ∘ (flip append s ∘ flip append "/") <$> Process.cwd

warnMsg ∷ ∀ eff o. String → Feature eff o Unit
warnMsg msg = void $ lift $ log $ yellow msg

sectionMsg ∷ ∀ eff o. String → Feature eff o Unit
sectionMsg msg = void $ lift $ log $ magenta $ "\n" <> msg

showImageFile ∷ ∀ eff o. FilePath → Feature eff o String
showImageFile =
  showBuffer <=< readBuffer <=< getFullPath
  where
  getFullPath = appendToCwd
  readBuffer w = lift $ readFile w
  showBuffer = liftEff ∘ toString Base64

logCurrentScreen ∷ ∀ eff o. Feature eff o Unit
logCurrentScreen =
  saveScreenshot path *> logScreenshot path
  where
  path = "current.png"
  message = ("Screenshot taken now:\ndata:image/png;base64," <> _)
  logScreenshot p = (message <$> showImageFile p) >>= warnMsg

