module App.Presentations.Mal where

import Prelude
import App.Prelude
import App.State (SlideData(..), BeforeOrAfter(..))

types="\
\{-# LANGUAGE TypeSynonymInstances #-}\n\
\{-# LANGUAGE FlexibleInstances #-}\n\
\module Types\n\
\( Sexp(..)\n\
\, Environment(..)\n\
\, Command(..)\n\
\, AppliedCommand(..)\n\
\, Bindings(..)\n\
\, RunTimeError(..)\n\
\, Eval(..)\n\
\, MalError(..)\n\
\, runEval\n\
\, runEvalForTuple\n\
\) where\n\
\import qualified Data.Map as Map\n\
\import Control.Monad.Writer.Lazy\n\
\import Control.Monad.Except\n\
\import Control.Monad.Identity\n\
\import Text.Parsec\n\
\type Params = [Sexp]\n\
\type Bindings = [Sexp]\n\
\type Body = Sexp\n\
\type RunTimeError = String\n\
\\n\
\type Environment = [Map.Map String Sexp]\n\
\type Eval = WriterT Environment (ExceptT MalError Identity) Sexp\n\
\runEval :: Eval -> Either MalError Sexp\n\
\runEval eval = runIdentity $ runExceptT result\n\
\  where result = fst <$> runWriterT eval\n\
\\n\
\runEvalForTuple :: Eval -> Either MalError (Sexp, Environment)\n\
\runEvalForTuple eval = runIdentity $ runExceptT $ runWriterT eval\n\
\\n\
\type AppliedCommand = Bindings -> Environment -> Eval\n\
\type Command = Params -> Body -> AppliedCommand\n\
\\n\
\data Sexp = MalNum Int | MalSymbol String | MalList [Sexp] | MalFunction AppliedCommand | MalBool Bool | MalString String\n\
\            deriving (Eq)\n\
\\n\
\data MalError = MalParseError ParseError | MalEvalError String\n\
\\n\
\instance Show Sexp where\n\
\  show (MalNum x) = show x\n\
\  show (MalSymbol x) = x\n\
\  show (MalBool x) = show x\n\
\  show (MalString string) = \"\\\"\" ++ string ++ \"\\\"\"\n\
\  show (MalFunction appliedCommand) = \"<FN>\"\n\
\  show (MalList sexps) = \"(\" ++ foldl convertToString \"\" sexps ++ \")\"\n\
\\n\
\    where\n\
\      convertToString \"\" sexp = show sexp\n\
\      convertToString accumulator sexp = accumulator ++ \" \" ++ show sexp\n\
\\n\
\instance {-# OVERLAPS #-} Monoid Environment where\n\
\  mempty = []\n\
\  mappend x y = reverse $ mergeLists xList yList\n\
\    where xList = reverse x\n\
\          yList = reverse y\n\
\          mergeLists x [] = x\n\
\          mergeLists [] y = y\n\
\          mergeLists (x:xs) (y:ys) = (y `Map.union` x) : mergeLists xs ys\n\
\\n\
\instance Eq MalError where\n\
\  MalParseError one == MalParseError two = one == two\n\
\  MalEvalError one == MalEvalError two = one == two\n\
\\n\
\instance Show MalError where\n\
\  show (MalParseError error) = show error\n\
\  show (MalEvalError error) = show error\n\
\\n\
\instance Eq AppliedCommand where\n\
\  commandOne == commandTwo = runEval (commandOne bindings mempty ) == runEval (commandTwo bindings mempty)\n\
\    where bindings = [MalNum 1, MalNum 2]\n\
\\n\
\--instance Show Eval where\n\
\  --show eval = show $ runEval eval"

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
      , annotation: "This function is the entry point into the src portion of my program. While main creates an executable, this creates a library that is used both in the executable and in the test suite."
      , content: rep
      }

  , SlideData
      { fileName: "ReadEval.hs"
      , filePath: "src/"
      , lineNumber: 3
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "As you can see I have defined several types here. They are all found in my Types module so let's head over there now!"
      , content: rep
      }

  , SlideData
      { fileName: "Types.hs"
      , filePath: "src/"
      , lineNumber: 37
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "A Sexp is an S expression. This is a lisp expression that is made up of some Lisp type that we store as data constructors. It is recursive so it can hold a list of other Sexps."
      , content: types
      }

  , SlideData
      { fileName: "Types.hs"
      , filePath: "src/"
      , lineNumber: 25
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "The environment is a list of maps that have string keys and sexp values. This is the datastructure we use to get the values of variables. It's in an array so we can have different layers of environment depending on the scope."
      , content: types
      }

  , SlideData
      { fileName: "Types.hs"
      , filePath: "src/"
      , lineNumber: 40
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "The errors are either a parse error or a eval error. These are errors from the respective read and eval steps of the REPL process."
      , content: types
      }

  , SlideData
      { fileName: "ReadEval.hs"
      , filePath: "src/"
      , lineNumber: 6
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "Now that we've looked at the types let's look at this signature again! This function takes an environment (which in practice is a default or the result of a previous command) and the actual lisp code. It then returns either an error or a tuple with the environment and result."
      , content: rep
      }
  ]
