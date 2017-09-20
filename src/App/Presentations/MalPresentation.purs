module App.Presentations.Mal where

import Prelude
import App.Prelude
import App.State (SlideData(..), BeforeOrAfter(..))

entry = "\
\module Main where\n\
\\n\
\import Control.Monad\n\
\import System.IO\n\
\import ReadEval                 (readEval)\n\
\import Types                    (runEvalForTuple, Environment(..))\n\
\import Core                     (startingEnv)\n\
\import System.Console.Haskeline (runInputT, getInputLine, defaultSettings, InputT(..), outputStrLn)\n\
\\n\
\main :: IO ()\n\
\main = runInputT defaultSettings $ loop startingEnv\n\
\   where\n\
\       loop :: Environment -> InputT IO ()\n\
\       loop env = do\n\
\           minput <- getInputLine \"user> \"\n\
\           case minput of\n\
\               Nothing -> return ()\n\
\               Just \"quit\" -> return ()\n\
\               Just command -> case (readEval env command) of\n\
\                                 Right (s, e) -> do outputStrLn $ show s\n\
\                                                    loop e\n\
\                                 Left error -> do outputStrLn $ show error\n\
\                                                 loop env"

rep = "\
\module ReadEval (readEval) where\n\
\import Parser (readLang)\n\
\import Types (Sexp(..), Environment(..), Eval(..), runEvalForTuple, MalError(..))\n\
\import Evaluator (evaluate)\n\
\\n\
\readEval :: Environment -> String -> Either MalError (Sexp, Environment)\n\
\readEval env command = do sexp <- readLang command\n\
\                         runEvalForTuple $ evaluate (sexp, env)"

presentation ∷ Array SlideData
presentation =
  [
    SlideData
      { fileName: "Main.hs"
      , filePath: "app/"
      , lineNumber: 1
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "Hello! I want to walk you through one of the first larger Haskell programs I've built. In this project I learned about concepts like monad transformers and started to experiment with what clean code should look like in pure functional programming."
      , content: entry
      }

  , SlideData
      { fileName: "Main.hs"
      , filePath: "app/"
      , lineNumber: 1
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "This project is a very simple implementation of Lisp in Haskell based upon the Make A Lisp project (https://github.com/kanaka/mal). There is an existing version of this project in Haskell but I tried to look at it as little as possible. My version is not quite finished yet, but I've learned so much about Haskell in making this I think it's worth showing off. So I will show you what I learned while building this and what I still have to do."
      , content: entry
      }

  , SlideData
      { fileName: "Main.hs"
      , filePath: "app/"
      , lineNumber: 11
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "To start with this is the entry point for the interpreter. I originally was just using normal IO operations to do my terminal but then discovered the wonderful Haskeline."
      , content: entry
      }

  , SlideData
      { fileName: "Main.hs"
      , filePath: "app/"
      , lineNumber: 19
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "We call a function named readEval with the Lisp command entered and the current environment and this is what does the brunt of the work."
      , content: entry
      }

  , SlideData
      { fileName: "ReadEval.hs"
      , filePath: "src/"
      , lineNumber: 7
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "This function is the entry point into the src portion of my program. While main creates an executable, this creates a library that is used both in the executable and in my test suite."
      , content: rep
      }
  ]
