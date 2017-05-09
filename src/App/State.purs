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
  }

derive instance genericState :: Generic State _
derive instance newtypeState :: Newtype State _

instance showState :: Show State where show = genericShow

init :: String -> State
init url = State
  { title: config.title
  , route: match url
  , loaded: false
  , slides: [SlideData {fileName: "CODE FOR PRESENTATION 1", lineNumber: 1, annotation: "HI"}, SlideData {fileName: "CODE FOR PRESENTATION 2", lineNumber: 1, annotation: "HI"}, SlideData {fileName: "CODE FOR PRESENTATION 3", lineNumber: 1, annotation: "HI"}]
  }
