{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Forming.Run where

import System.Environment (getArgs)

import Text.Syntactical

import Control.Arrow (first, right)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.HashMap.Strict as H (toList)
import Data.Aeson (decode, Value)
import qualified Data.Aeson as A (Value(Bool, Number, Object, String))
import Data.List (nub)
import Data.Scientific (floatingOrInteger)
import qualified Data.Text as T
import qualified Options.Applicative as A
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Hypered.Html
  ( cAddWrapper, cFont, cStaticPath, Font(IbmPlex)
  , defaultConfig
  , generate')

import Forming.Command ( Command(..), Command'(..)
  , Run(..), ExprOrFilePath(..), parserRunInfo, parserInfo' )
import Forming.Core
import Forming.IO
import qualified Forming.Lexer as Lexer
import Forming.Html (pageComputation)
import qualified Forming.Parser as Parser
import Forming.Servant (runServant)
import Forming.Server (runServer)


--------------------------------------------------------------------------------
-- Simple command-line program
-- --token         just show the result of the tokenizing (indentation)
-- --token --file  idem on a file
-- --sexpr         apply the shunting yard to the argument
-- --sexpr --file  apply the shunting yard to the file
run :: Command -> IO ()
run command = do
  case command of
    Tokenize s -> case Lexer.tokenize s of
      Right a -> putStrLn . unwords $ map toString a
      Left err -> putStrLn $ "indentation error: " ++ show err

    TokenizeFile fp -> do
      s <- readFile fp
      case Lexer.tokenize s of
        Right a -> putStrLn . unwords $ map toString a
        Left err -> putStrLn $ "indentation error: " ++ show err

    SExpr s -> case Lexer.tokenize s of
      Right a -> case Parser.parse a of
        Right e -> putStrLn $ showSExpr e
        Left f -> putStrLn $ showFailure f
      Left err -> putStrLn $ "indentation error: " ++ show err

    SExprFile fp -> do
      s <- readFile fp
      case Lexer.tokenize s of
        Right a -> case Parser.parse a of
          Right e -> putStrLn $ showSExpr e
          Left f -> putStrLn $ showFailure f
        Left err -> putStrLn $ "indentation error: " ++ show err

    Serve s -> execute (\c -> runServer [c]) s

    ServeFile fp -> do
      s <- readFile fp
      execute (\c -> runServer [c]) s

    Servant s -> execute (\c -> runServant [c]) s

    ServantFile fp -> do
      s <- readFile fp
      execute (\c -> runServant [c]) s

    Run r e -> case e of
      ExprString s -> execute (\c -> runComputation c r) s
      ExprFilePath fp -> do
        s <- readFile fp
        execute (\c -> runComputation c r) s

execute :: (Computation -> IO ()) -> String -> IO ()
execute f s = do
  case Lexer.tokenize s of
    Right a -> case Parser.parse a of
      Right e -> case Parser.parseRules e of
        Right rules -> do
          let comp = Computation "TODO" "TODO" "main" rules
          f comp
        Left err -> print err
      Left f -> putStrLn $ showFailure f
    Left err -> putStrLn $ "indentation error: " ++ show err


--------------------------------------------------------------------------------
defaultMain :: [Computation] -> IO ()
defaultMain cs = do
  r <- A.execParser parserInfo'
  case r of
    RunInfo' -> do
      putStrLn "Available computations:"
      mapM_ (putStrLn . (\c -> "  " ++ cSlug c ++ "  " ++ cName c)) cs

    Serve' -> runServer cs

    Servant' -> runServant cs

    Run' slug r -> case lookup slug (map (\c -> (cSlug c, c)) cs) of
      Just c -> runComputation c r
      Nothing -> do
        putStrLn "ERROR: the given computation slug doesn't exist."
        exitFailure

defaultMainOne :: Computation -> IO ()
defaultMainOne c = do
  r <- A.execParser parserRunInfo
  runComputation c r


--------------------------------------------------------------------------------
runComputation :: Computation -> Run -> IO ()
runComputation c@Computation{..} mode = case mode of

  -- Show a help message.
  RunInfo -> do
    putStrLn cName
    -- Evaluate without input to give a hint of possible user inputs.
    case evaluate [] cMain cRules [] of
      UnsetVariables names -> printUnsetVariables names
      Error _ (NoSuchRule _) -> putStrLn ("The rule \"" ++ cMain ++ "\" doesn't exist.")
      Result _ -> putStrLn "This computation doesn't require any user input."

  -- Generate an HTML page with technical comments.
  -- This uses Hypered's design-system Haskell code.
  RunHtml -> do
    let conf' = defaultConfig
          { cAddWrapper = False, cFont = IbmPlex, cStaticPath = "../../static" }
    generate' "form.html" "Forming"
      conf' (const (pageComputation c))

  -- List all rules.
  RunList -> mapM_ print cRules

  -- List unset names (possibly limited to a specific rule), and their types
  -- if possible.
  RunUnset mname -> print (gatherUnsets Nothing (maybe cMain id mname) cRules)

  -- Parse inputs given as JSON and evaluate one rule.
  RunJsonInput s mname -> printOutputAsJson $ compute c mname $ makeInputsFromJson s

  -- Parse inputs of the form `--set a 1` (with makeInputs) and evaluate one
  -- rule. Before creating the inputs, gatherUnsets is called to compute
  -- types, which are necessary to correctly parse inputs.
  -- TODO Note that cMain and the name given by makeInputs can be different.
  -- So it is possible that `gatherUnsets` will infer a type (or Nothing)
  -- differently that what `compute` will use.
  -- Maybe I should have a check to make sure that all the rules together
  -- are consistent, and maybe I should force that every inputs are typed.
  -- This is not the case in e.g. the "trivial-a" example.
  RunInputs pairs mname -> case gatherUnsets Nothing cMain cRules of
    Right unsets -> printOutput $ compute c mname $ makeInputs unsets pairs []
    Left err -> print err
