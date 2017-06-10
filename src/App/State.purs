module App.State where

import Prelude
import App.Prelude
import Data.Array (snoc, foldl)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))
import Control.Monad.Eff (Eff)
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
  , content ∷ String
  }

derive instance genericSlideData :: Generic SlideData _
derive instance newtypeSlideData :: Newtype SlideData _

instance encodeSlide ∷ Encode SlideData where encode = genericEncode defaultOptions
instance decodeSlide ∷ Decode SlideData where decode = genericDecode defaultOptions
instance showSlide ∷ Show SlideData where show = genericShow

newtype PreFetchSlide = PreFetchSlide
  { fileName ∷ String
  , lineNumber ∷ Int
  , annotation ∷ String
  }

newtype State = State
  { title ∷ String
  , route ∷ Route
  , loaded ∷ Boolean
  , slides ∷ Array SlideData
  }

derive instance genericState ∷ Generic State _
derive instance newtypeState ∷ Newtype State _
instance showState ∷ Show State where show = genericShow

defaultSlides ∷ Array PreFetchSlide
defaultSlides =
  [
    PreFetchSlide
      { fileName: "MultiplyMeApi/app/controllers/api/v1/leader_board_controller.rb"
      , lineNumber: 27
      , annotation: "Lorem ipsum dolor sit amet, consectetuer adipiscing elit.  Donec hendrerit tempor tellus.  Donec pretium posuere tellus.  Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus.  Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus.  Nulla posuere.  Donec vitae dolor.  Nullam tristique diam non turpis.  Cras placerat accumsan nulla.  Nullam rutrum.  Nam vestibulum accumsan nisl.Lorem ipsum dolor sit amet, consectetuer adipiscing elit.  Donec hendrerit tempor tellus.  Donec pretium posuere tellus.  Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus.  Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus.  Nulla posuere.  Donec vitae dolor.  Nullam tristique diam non turpis.  Cras placerat accumsan nulla.  Nullam rutrum.  Nam vestibulum accumsan nisl.Lorem ipsum dolor sit amet, consectetuer adipiscing elit.  Donec hendrerit tempor tellus.  Donec pretium posuere tellus.  Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus.  Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus.  Nulla posuere.  Donec vitae dolor.  Nullam tristique diam non turpis.  Cras placerat accumsan nulla.  Nullam rutrum.  Nam vestibulum accumsan nisl.Lorem ipsum dolor sit amet, consectetuer adipiscing elit.  Donec hendrerit tempor tellus.  Donec pretium posuere tellus.  Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus.  Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus.  Nulla posuere.  Donec vitae dolor.  Nullam tristique diam non turpis.  Cras placerat accumsan nulla.  Nullam rutrum.  Nam vestibulum accumsan nisl.Lorem ipsum dolor sit amet, consectetuer adipiscing elit.  Donec hendrerit tempor tellus.  Donec pretium posuere tellus.  Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus.  Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus.  Nulla posuere.  Donec vitae dolor.  Nullam tristique diam non turpis.  Cras placerat accumsan nulla.  Nullam rutrum.  Nam vestibulum accumsan nisl.Lorem ipsum dolor sit amet, consectetuer adipiscing elit.  Donec hendrerit tempor tellus.  Donec pretium posuere tellus.  Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus.  Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus.  Nulla posuere.  Donec vitae dolor.  Nullam tristique diam non turpis.  Cras placerat accumsan nulla.  Nullam rutrum.  Nam vestibulum accumsan nisl."
      }
  , PreFetchSlide
      { fileName: "MultiplyMeApi/app/services/referral_code_service.rb"
      , lineNumber: 18
      , annotation: "second annotation"
      }
  , PreFetchSlide
      { fileName: "MultiplyMeApi/app/controllers/api/v1/organizations_controller.rb"
      , lineNumber: 1
      , annotation: "third annotation"
      }
  ]
transform ∷ ∀ e. Array PreFetchSlide → Eff (exception ∷ EXCEPTION, fs ∷ FS | e) (Array SlideData)
transform prefetch = foldl toSlideData (pure []) prefetch
  where toSlideData accum (PreFetchSlide p) = snoc <$> accum <*> (fetchSlide p)
        fetchSlide p = (makeSlide p) <$> readTextFile UTF8 p.fileName
        makeSlide {fileName: fileName, lineNumber: lineNumber, annotation: annotation} content =
          SlideData {fileName: fileName, lineNumber: lineNumber, annotation: annotation, content: content}


initWithSlides ∷ String → Array SlideData → State
initWithSlides url slides = State
  { title: config.title
  , route: match url
  , loaded: false
  , slides: slides
  }
