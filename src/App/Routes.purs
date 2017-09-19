module App.Routes where

import App.Prelude
import Data.Function (($))
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (fromMaybe)
import Data.Show (class Show, show)
import Pux.Router (end, router, lit, int, str)
import Data.Functor ((<$>), (<$))
import Control.Applicative ((<*>), (<*), (*>))
import Control.Alt ((<|>))


type ProjectName = String
type SlideNumber = Int

data Route = Home | NotFound String | Presentation ProjectName SlideNumber

derive instance genericRoute :: Generic Route _
instance showRoute :: Show Route where show = genericShow
instance decodeRoute :: Decode Route where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoute :: Encode Route where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

match :: String -> Route
match url = fromMaybe (NotFound url) $ router url $
  Home <$ end
  <|>
  Presentation <$> (lit "presentation" *> str) <*> int <* end
  <|>
  NotFound "/not_found" <$ lit "not_found" <* end


toURL :: Route -> String
toURL (NotFound url) = url
toURL (Home) = "/presentation/multiply-me/1"
toURL (Presentation projectName slideNumber) = "/presentation/" ⊕ projectName ⊕ "/" ⊕ show slideNumber
