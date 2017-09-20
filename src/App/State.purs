module App.State where

import Prelude
import App.Prelude
import App.Config (config)
import App.Routes (Route, match)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (snoc, foldl)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (genericEncode, genericDecode, defaultOptions)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Newtype (wrap)
import Data.Show (class Show)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)

data BeforeOrAfter = Nothing | Before | After
derive instance genericBeforeOrAfter :: Generic BeforeOrAfter _

instance encodeBeforeOrAfter ∷ Encode BeforeOrAfter where encode = genericEncode defaultOptions
instance decodeBeforeOrAfter ∷ Decode BeforeOrAfter where decode = genericDecode defaultOptions
instance showBeforeOrAfter ∷ Show BeforeOrAfter where show = genericShow

newtype SlideData = SlideData
  { fileName ∷ String
  , filePath ∷ String
  , lineNumber ∷ Int
  , annotation ∷ String
  , content ∷ String
  , language ∷ String
  , beforeOrAfter ∷ BeforeOrAfter
  }

derive instance genericSlideData :: Generic SlideData _
derive instance newtypeSlideData :: Newtype SlideData _

instance encodeSlide ∷ Encode SlideData where encode = genericEncode defaultOptions
instance decodeSlide ∷ Decode SlideData where decode = genericDecode defaultOptions
instance showSlide ∷ Show SlideData where show = genericShow

newtype PreFetchSlide = PreFetchSlide
  { fileName ∷ String
  , filePath ∷ String
  , lineNumber ∷ Int
  , annotation ∷ String
  , language ∷ String
  , beforeOrAfter ∷ BeforeOrAfter
  }

newtype Presentations = Presentations
  { multiplyMe ∷ Array SlideData
  , mal ∷ Array SlideData
  }
derive instance genericPresentations :: Generic Presentations _
derive instance newtypePresentations :: Newtype Presentations _

instance encodePresentations ∷ Encode Presentations where encode = genericEncode defaultOptions
instance decodePresentations ∷ Decode Presentations where decode = genericDecode defaultOptions
instance showPresentations ∷ Show Presentations where show = genericShow


emptyPres = Presentations {multiplyMe: [], mal: []}


newtype State = State
  { title ∷ String
  , route ∷ Route
  , loaded ∷ Boolean
  , presentations ∷ Presentations
  }

derive instance genericState ∷ Generic State _
derive instance newtypeState ∷ Newtype State _
instance showState ∷ Show State where show = genericShow

transform ∷ ∀ e. Array PreFetchSlide → Eff (exception ∷ EXCEPTION, fs ∷ FS | e) (Array SlideData)
transform prefetch = foldl toSlideData (pure []) prefetch
  where toSlideData accum (PreFetchSlide p) = snoc <$> accum <*> (fetchSlide p)
        fetchSlide p = (makeSlide p) <$> readTextFile UTF8 p.fileName
        makeSlide {fileName: fileName, filePath: filePath, lineNumber: lineNumber, annotation: annotation, language: language, beforeOrAfter} content =
          SlideData {fileName: fileName, filePath: filePath, lineNumber: lineNumber, annotation: annotation, content: content, language: language, beforeOrAfter: beforeOrAfter}


initWithSlides ∷ String → Presentations → State
initWithSlides url presentations = State
  { title: config.title
  , route: match url
  , loaded: false
  , presentations: presentations
  }
