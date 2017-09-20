module App.Presentations.Mal where

import Prelude
import App.Prelude
import App.State (SlideData(..), BeforeOrAfter(..))

parserTests = "\
\module ParserSpec (spec) where\n\
\import Parser (readLang)\n\
\import Test.Hspec\n\
\import Control.Exception (evaluate)\n\
\import Core\n\
\import Data.Either\n\
\import Types (Sexp(..))\n\
\\n\
\spec :: Spec\n\
\spec =\n\
\  describe \"Parser\" $\n\
\    describe \"readLang\" $ do\n\
\      it \"parses a number\" $\n\
\        readLang \"12\" `shouldBe` Right (MalNum 12)\n\
\\n\
\      it \"parses a symbol\" $\n\
\        readLang \"symbol\" `shouldBe` Right (MalSymbol \"symbol\")\n\
\\n\
\      it \"fails to parse a string starting with a number\" $\n\
\        isLeft (readLang \"95letters\") `shouldBe` True\n\
\\n\
\      it \"parses a list\" $\n\
\        readLang \"(1 2)\" `shouldBe` Right (MalList [MalNum 1, MalNum 2])\n\
\\n\
\      it \"parses a nested list\" $\n\
\        readLang \"(1 (2 3))\" `shouldBe` Right (MalList [MalNum 1, MalList [MalNum 2, MalNum 3]])\n\
\\n\
\      it \"fails if parsing a list with parentheses that don't match\" $\n\
\        isLeft (readLang \"(1 2 (3)\") `shouldBe` True\n\
\\n\
\      it \"parses a string\" $\n\
\        readLang \"\\\"Hello\\\"\" `shouldBe` Right (MalString \"Hello\")\n\
\\n\
\      it \"parses a string with whitespace\" $\n\
\        readLang \"\\\" Hi\\\"\" `shouldBe` Right (MalString \" Hi\")\n\
\\n\
\      it \"fails to parse a string with mismatched quotes\" $\n\
\        isLeft (readLang \"\\\"Hello\") `shouldBe` True\n\
\\n\
\      it \"parses a string with escaped quotes\" $\n\
\        readLang \"\\\"Hi\\\\\\\"\\\"\" `shouldBe` Right (MalString \"Hi\\\"\")"

core="\
\module Core\n\
\( makeMalFunction\n\
\, startingEnv\n\
\) where\n\
\import Types\n\
\import Environment as Env\n\
\import Evaluator\n\
\import Control.Monad.Except\n\
\import Control.Monad.Writer.Lazy (listen, tell)\n\
\import Debug.Trace\n\
\import qualified Data.Map as Map\n\
\\n\
\makeError :: AppliedCommand\n\
\makeError [_, _] _ = do throwError (MalEvalError $ \"Parameters are of the wrong type\")\n\
\makeError list _ = do throwError (MalEvalError $ \"Wrong number of parameters. Expected 2 parameters but got \" ++ (show $ length list))\n\
\\n\
\applyNumberAction :: (Int -> Int -> Int) -> AppliedCommand\n\
\applyNumberAction f [MalNum x, MalNum y] _ = return $ MalNum $ f x y\n\
\applyNumberAction _ list env = makeError list env\n\
\\n\
\applyBooleanAction :: (Int -> Int -> Bool) -> AppliedCommand\n\
\applyBooleanAction f [MalNum x, MalNum y] _ = return $ MalBool $ f x y\n\
\applyBooleanAction _ list env = makeError list env\n\
\\n\
\makeMalFunction :: (Int -> Int -> Int) -> Sexp\n\
\makeMalFunction f = MalFunction $ applyNumberAction f\n\
\\n\
\makeMalBooleanFunction :: (Int -> Int -> Bool) -> Sexp\n\
\makeMalBooleanFunction f = MalFunction $ applyBooleanAction f\n\
\\n\
\\n\
\list :: AppliedCommand\n\
\list params env = return $ MalList params\n\
\\n\
\isList :: AppliedCommand\n\
\isList ((MalList x) : _) env = return $ MalBool True\n\
\isList (_ : _) env = return $ MalBool False\n\
\\n\
\isEmpty :: AppliedCommand\n\
\isEmpty ((MalList []) : _) env = return $ MalBool True\n\
\isEmpty ((MalList _) : _) env = return $ MalBool False\n\
\isEmpty (list : _) env = throwError $ MalEvalError $ (show list) ++ \" is not a list\"\n\
\\n\
\count :: AppliedCommand\n\
\count ((MalList list) : _) env = return $ MalNum $ length list\n\
\count (list : _) env = throwError $ MalEvalError $ (show list) ++ \" is not a list\"\n\
\\n\
\equality :: AppliedCommand\n\
\equality (left : right : []) env = equals left right\n\
\  where equals :: Sexp -> Sexp -> Eval\n\
\        equals (MalNum one) (MalNum two) = return $ MalBool $ one == two\n\
\        equals (MalSymbol one) (MalSymbol two) = MalBool <$> ((==) <$> Evaluator.evaluate (MalSymbol one, env) <*> Evaluator.evaluate (MalSymbol two, env))\n\
\        equals (MalBool one) (MalBool two) = return $ MalBool $ one == two\n\
\        equals (MalList one) (MalList two) = return $ MalBool $ one == two\n\
\        equals (MalFunction one) (MalFunction two) = return $ MalBool False\n\
\        equals (MalString one) (MalString two) = return $ MalBool $ one == two\n\
\        equals one two = throwError $ MalEvalError $ \"cannot compare \" ++ show one ++ \" and \" ++ show two\n\
\\n\
\\n\
\startingEnv :: Environment\n\
\startingEnv = [operationMap]\n\
\  where operationMap = Map.fromList [ (\"+\", makeMalFunction (+))\n\
\                                    , (\"-\", makeMalFunction (-))\n\
\                                    , (\"*\", makeMalFunction (*))\n\
\                                    , (\"/\", makeMalFunction div)\n\
\                                    , (\"<\", makeMalBooleanFunction (<))\n\
\                                    , (\"<=\", makeMalBooleanFunction (<=))\n\
\                                    , (\">\", makeMalBooleanFunction (>))\n\
\                                    , (\">=\", makeMalBooleanFunction (>=))\n\
\                                    , (\"list\", MalFunction list)\n\
\                                    , (\"list?\", MalFunction isList)\n\
\                                    , (\"empty?\", MalFunction isEmpty)\n\
\                                    , (\"count\", MalFunction count)\n\
\                                    , (\"=\", MalFunction equality)\n\
\                                    --, (\"prn\", MalFunction)\n\
\                                    ]"

evaluator="\
\module Evaluator\n\
\(evaluate) where\n\
\\n\
\import Types                        (Sexp(..), Environment(..), RunTimeError(..), Eval(..), runEval, MalError(..), runEvalForTuple, Command(..))\n\
\import qualified Environment as Env (get, set, addLayer, removeLayer)\n\
\import Control.Monad.Writer.Lazy (censor, listen, tell)\n\
\import Control.Monad.Except\n\
\\n\
\evaluate :: (Sexp, Environment) -> Eval\n\
\evaluate (MalList (MalSymbol \"let\" : MalList params : expression : []), env) = evaluateFinalExpression newEnv\n\
\  where evaluateFinalExpression (Left error) =\n\
\          throwError error\n\
\        evaluateFinalExpression (Right (expression, environment)) =\n\
\          censor Env.removeLayer $ evaluate (expression, environment)\n\
\\n\
\        newEnv = runEvalForTuple $ bindLetVars params $ Env.addLayer env\n\
\\n\
\        bindLetVars :: [Sexp] -> Environment -> Eval\n\
\        bindLetVars [] env = do tell env\n\
\                                return expression\n\
\        bindLetVars (_ : []) env = do throwError $ MalEvalError \"let param bindings do not match\"\n\
\\n\
\        bindLetVars (MalSymbol key : val : params) env =\n\
\          do unwrappedValue <- evaluate (val, env)\n\
\             bindLetVars params $ Env.set key unwrappedValue env\n\
\\n\
\evaluate (MalList (MalSymbol \"def\" : MalSymbol key : value : []), env)\n\
\  = do solvedValue <- evaluate (value, env)\n\
\       tell $ Env.set key solvedValue env\n\
\       return solvedValue\n\
\\n\
\                                                                            --add case statements for error handling\n\
\evaluate (MalList (MalSymbol \"fn\" : MalList params : functionBody : []), env)\n\
\  = return $ MalFunction $ createFunction params functionBody\n\
\  where createFunction :: Command\n\
\        createFunction params body bindings environment = evaluate (body, functionEnvironment params bindings environment)\n\
\        functionEnvironment params bindings environment = foldl setToEnv environment $ zip params bindings\n\
\        setToEnv currentEnv (MalSymbol k, v) = Env.set k v currentEnv\n\
\                        -- setToEnv currentEnv (k, v) = \n\
\\n\
\evaluate (MalList (MalSymbol \"do\" : params), env) = foldl foldEval initialValue params\n\
\  where initialValue :: Eval\n\
\        initialValue = do tell env\n\
\                          return $ MalList []\n\
\        foldEval :: Eval -> Sexp -> Eval\n\
\        foldEval accumulator sexp = do (_, resultingEnv) <- listen accumulator\n\
\                                       evaluate (sexp, resultingEnv)\n\
\\n\
\evaluate (MalList (MalSymbol \"if\" : condition : positive : negative : []), env) =\n\
\  do (sexp, newEnv) <- listen $ evaluate (condition, env)\n\
\     evaluate (chooseEvaluation sexp, newEnv)\n\
\  where chooseEvaluation (MalBool True)  = positive\n\
\        chooseEvaluation (MalBool False) = negative\n\
\        chooseEvaluation _               = positive\n\
\\n\
\\n\
\evaluate (command, env)\n\
\  = do tell env\n\
\       result <- evalAst (command, env)\n\
\       case result of\n\
\         MalList (MalFunction f : list) -> f list env\n\
\         MalList (f : list) -> throwError $ MalEvalError $ (show f) ++ \" is not a function\"\n\
\         _ -> return result\n\
\\n\
\  where evalAst :: (Sexp, Environment) -> Eval\n\
\        evalAst (MalSymbol symbol, env) =\n\
\          do tell env\n\
\             case Env.get symbol env of\n\
\               Nothing -> throwError (MalEvalError $ \"unbound variable \" ++ symbol)\n\
\               Just val -> return $ val\n\
\\n\
\        evalAst (MalList list, env) =  foldr f (return $ MalList []) list\n\
\          where f sexp accum =  do tell env\n\
\                                   resultingSexp <- evaluate (sexp, env)\n\
\                                   MalList accumulatedSexp <- accum\n\
\                                   return $ MalList $ resultingSexp : accumulatedSexp\n\
\\n\
\        evalAst (sexp, env) = do tell env\n\
\                                 return sexp"

parser="\
\module Parser\n\
\( readLang\n\
\) where\n\
\import Text.Parsec         (many, parse, ParseError, eof, oneOf, noneOf, (<|>), many1, digit, string, try)\n\
\import Text.Parsec.Char    (char)\n\
\import Text.Parsec.String  (Parser)\n\
\import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))\n\
\import Control.Monad       (void, ap, (>>))\n\
\import Types               (Sexp(..), MalError(..))\n\
\\n\
\readLang :: String -> Either MalError Sexp\n\
\readLang command = either (Left . MalParseError) (Right) (parseWithWhitespace sexp command)\n\
\\n\
\parseWithWhitespace :: Parser a -> String -> Either ParseError a\n\
\parseWithWhitespace p = parseWithEof (whitespace >> p)\n\
\\n\
\parseWithEof :: Parser a -> String -> Either ParseError a\n\
\parseWithEof p = parse (p <* eof) \"\"\n\
\\n\
\sexp :: Parser Sexp\n\
\sexp = list <|> atom\n\
\\n\
\atom :: Parser Sexp\n\
\atom = num <|> bool <|> malString <|> symbol\n\
\\n\
\malString :: Parser Sexp\n\
\malString = MalString <$> (char '\"' *> stringChars <* lexeme (char '\"'))\n\
\\n\
\stringChars = many $ escapedChars <|> noneOf \"\\\\\\\"\"\n\
\\n\
\-- I took this function from the existing example. One of the only things I've copied in this project.\n\
\escapedChars = do char '\\\\'\n\
\                  x <- oneOf \"\\\\\\\"n\"\n\
\                  case x of\n\
\                    'n' -> return '\\n'\n\
\                    _   -> return x\n\
\\n\
\\n\
\whitespace :: Parser ()\n\
\whitespace = void $ many $ oneOf \" \\n\\t\"\n\
\\n\
\lexeme :: Parser a -> Parser a\n\
\lexeme p = p <* whitespace\n\
\\n\
\bool :: Parser Sexp\n\
\bool = MalBool <$> (==) \"true\" <$> (lexeme $ (string \"true\") <|> (try $ string \"false\"))\n\
\\n\
\num :: Parser Sexp\n\
\num = (MalNum . Prelude.read) <$> lexeme(many1 digit)\n\
\\n\
\symbol :: Parser Sexp\n\
\symbol = MalSymbol <$> lexeme (many1 $ noneOf \" ()\")\n\
\\n\
\list :: Parser Sexp\n\
\list = MalList <$> (lexeme (char '(')\n\
\                 *> many sexp\n\
\                 <* lexeme (char ')'))"

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
      , annotation: "Hello! I want to walk you through one of the first larger Haskell programs I've built. In this project I learned about concepts like monad transformers and started to experiment with what clean code should look like in pure functional programming. I did most of this work a couple of years ago but it remains a good example of a semi-larger Haskell application I have built."
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
      , annotation: "To start with this is the main function for the interpreter. I originally was just using normal IO operations to do my repl but then discovered the wonderful Haskeline."
      , content: entry
      }

  , SlideData
      { fileName: "Main.hs"
      , filePath: "app/"
      , lineNumber: 19
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "This function does the brunt of the work. We call it with the Lisp command entered and the current environment."
      , content: entry
      }

  , SlideData
      { fileName: "ReadEval.hs"
      , filePath: "src/"
      , lineNumber: 7
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "This is the entry point into the src portion of my program. While main creates an executable, this creates a library that is used both in the executable and in the test suite."
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
      , lineNumber: 37
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "You will notice we define a MalFunction as an AppliedCommand. Let's look at what that is!"
      , content: types
      }

  , SlideData
      { fileName: "Types.hs"
      , filePath: "src/"
      , lineNumber: 34
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "An AppliedCommand is actually a function. So I'm just storing my MalFunctions as a partially applied function. Bindings and Environment are both lists of Sexps. They just represent the lists of names and values for function parameters."
      , content: types
      }

  , SlideData
      { fileName: "Types.hs"
      , filePath: "src/"
      , lineNumber: 25
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "Now let's look at the environment. It is a list of maps that have string keys and sexp values. This is the datastructure we use to get the values of variables. It's in an array so we can have different layers of environment depending on the scope."
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
      , annotation: "Now that we've examined our types let's look at this signature again! This function takes an environment and the actual lisp code. It then returns either an error or a tuple with the environment and result."
      , content: rep
      }

  , SlideData
      { fileName: "Core.hs"
      , filePath: "src/"
      , lineNumber: 61
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "In practice the environment that evaluate takes in is either the result of a previous computation or a default. This default is defined here. Here we define a bunch of basic functions and built the outer layer of the environment."
      , content: core
      }

  , SlideData
      { fileName: "ReadEval.hs"
      , filePath: "src/"
      , lineNumber: 7
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "Now back to readEval. readLang is the first call and it is used to parse our Lisp code. Let's take a look at it!"
      , content: rep
      }

  , SlideData
      { fileName: "Parser.hs"
      , filePath: "src/"
      , lineNumber: 11
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "Here is the source for the readLang function. It uses the Parsec library to build a Parser."
      , content: parser
      }

  , SlideData
      { fileName: "Parser.hs"
      , filePath: "src/"
      , lineNumber: 14
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "I create a function called parseWithWhitespace that takes a Parser and the String to be parsed. It uses the Parser to parse the String after removing all end of file characters and whitespace from the end of the String."
      , content: parser
      }

  , SlideData
      { fileName: "Parser.hs"
      , filePath: "src/"
      , lineNumber: 21
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "The Parser itself tries to parse out a Sexp. Any lisp code can either be a list or an atom."
      , content: parser
      }

  , SlideData
      { fileName: "Parser.hs"
      , filePath: "src/"
      , lineNumber: 24
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "An atom in this context is one of the data types number, boolean, string, or symbol."
      , content: parser
      }

  , SlideData
      { fileName: "Parser.hs"
      , filePath: "src/"
      , lineNumber: 55
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "A list is just a bunch sexps surrounded by parentheses."
      , content: parser
      }

  , SlideData
      { fileName: "Parser.hs"
      , filePath: "src/"
      , lineNumber: 11
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "readLang itself returns a Sexp. Which is returned to readEval."
      , content: parser
      }

  , SlideData
      { fileName: "ReadEval.hs"
      , filePath: "src/"
      , lineNumber: 8
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "We now take that Sexp and our current Environment and pass them to our evaluate function which we will look at next!"
      , content: rep
      }

  , SlideData
      { fileName: "Evaluator.hs"
      , filePath: "src/"
      , lineNumber: 9
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "And here is the source of our evaluator function. It takes a tuple of a Sexp and a Env and returns an Eval. Before we dig into this function let's look at what an Eval actually is."
      , content: evaluator
      }

  , SlideData
      { fileName: "Types.hs"
      , filePath: "src/"
      , lineNumber: 26
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "And here I started to realize one monad should not handle the effects I needed. At this point I learned how to use monad transformers. So I built myself a small stack of monads with the needed effects. I used WriterT to keep up with the environment and ExceptT to propegate errors through the result. The contained value is just another Sexp."
      , content: types
      }

  , SlideData
      { fileName: "Evaluator.hs"
      , filePath: "src/"
      , lineNumber: 10
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "So back to eval! In this function we handle several special forms. The first of which is defined here. We pattern match on the Sexp we receive to see if it is a special form. I will not dig into the code too much for these forms but feel free to see how I have implemented them."
      , content: evaluator
      }

  , SlideData
      { fileName: "Evaluator.hs"
      , filePath: "src/"
      , lineNumber: 57
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "The general case is defined here. We call a function named evalAst and either use the result to call the parameter as a function or throw an error."
      , content: evaluator
      }

  , SlideData
      { fileName: "Evaluator.hs"
      , filePath: "src/"
      , lineNumber: 66
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "evalAst is a function that when presented with a symbol looks it up in the environment."
      , content: evaluator
      }

  , SlideData
      { fileName: "Evaluator.hs"
      , filePath: "src/"
      , lineNumber: 72
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "When it gets a list it recursively calls evaluate on the elements of that list."
      , content: evaluator
      }

  , SlideData
      { fileName: "Evaluator.hs"
      , filePath: "src/"
      , lineNumber: 78
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "Otherwise it just returns that element."
      , content: evaluator
      }

  , SlideData
      { fileName: "ParserSpec.hs"
      , filePath: "test/"
      , lineNumber: 1
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "Just as a final note I wrote tests for the whole program and as much as possible tried to do test driven development. Here is one such file."
      , content: parserTests
      }

  , SlideData
      { fileName: ""
      , filePath: ""
      , lineNumber: 1
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "I hope you enjoyed this presentation! Feel free to checkout my github (https://github.com/frasermince) and look at the other projects I have done. You might be interested in some of the work I've done in Haskell (https://github.com/frasermince/PortfolioAPI), Ruby (https://github.com/frasermince/MultiplyMeApi), or Javascript (https://github.com/frasermince/CodePortfolio, https://github.com/frasermince/MultiplyMe)"
      , content: "\n"
      }

    , SlideData
      { fileName: ""
      , filePath: ""
      , lineNumber: 1
      , language: "haskell"
      , beforeOrAfter: Nothing
      , annotation: "And finally I hope you have enjoyed trying out my newest product. This code portfolio software that I wrote in Purescript. If you're interested in learning more about it or making presentations like this yourself shoot me an email at frasermince@gmail.com."
      , content: "\n"
      }
]
