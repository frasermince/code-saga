module App.State where

import App.Prelude
import App.Config (config)
import App.Routes (Route, match)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.Show (class Show)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (genericEncode, genericDecode, defaultOptions)
import Data.Maybe (Maybe(..))
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Newtype (wrap)

newtype SlideData = SlideData
  { fileName ∷ String
  , lineNumber ∷ Int
  , annotation ∷ String
  }

derive instance genericSlideData :: Generic SlideData _
derive instance newtypeSlideData :: Newtype SlideData _

instance encodeSlide ∷ Encode SlideData where encode = genericEncode defaultOptions
instance decodeSlide ∷ Decode SlideData where decode = genericDecode defaultOptions
instance showSlide ∷ Show SlideData where show = genericShow

newtype State = State
  { title ∷ String
  , route ∷ Route
  , loaded ∷ Boolean
  , slides ∷ Array SlideData
  , currentSlideContent ∷ NullOrUndefined String
  }

derive instance genericState ∷ Generic State _
derive instance newtypeState ∷ Newtype State _
instance showState :: Show State where show = genericShow

defaultSlides ∷ Array SlideData
defaultSlides =
  [
    SlideData {fileName: "MultiplyMeApi/app/controllers/api/v1/accounts_controller.rb", lineNumber: 1, annotation: "HI"}
  , SlideData {fileName: "MultiplyMeApi/app/controllers/api/v1/donations_controller.rb", lineNumber: 1, annotation: "HI"}
  , SlideData {fileName: "MultiplyMeApi/app/controllers/api/v1/organizations_controller.rb", lineNumber: 1, annotation: "HI"}
  ]


initWithSlides ∷ String → Array SlideData → State
initWithSlides url slides = State
  { title: config.title
  , route: match url
  , loaded: false
  , slides: slides
  , currentSlideContent: wrap Nothing
  }


init :: String -> State
init url = initWithSlides url defaultSlides
