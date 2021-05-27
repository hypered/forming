-- This file started as a copy of Syntactical/indent.hs.
{-# Language OverloadedStrings #-}
module Main where

import System.Environment (getArgs)

import Text.Syntactical

import Forming (run, Computation(..))
import Forming.Server (runServer)

import qualified Forming.Lexer as Lexer
import qualified Forming.Parser as Parser


--------------------------------------------------------------------------------
-- Simple command-line program
-- --token         just show the result of the tokenizing (indentation)
-- --token --file  idem on a file
-- --sexpr         apply the shunting yard to the argument
-- --sexpr --file  apply the shunting yard to the file
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--token", s] -> case Lexer.tokenize s of
      Right a -> putStrLn . unwords $ map toString a
      Left err -> putStrLn $ "indentation error: " ++ show err

    ["--token", "--file", fn] -> do
      s <- readFile fn
      case Lexer.tokenize s of
        Right a -> putStrLn . unwords $ map toString a
        Left err -> putStrLn $ "indentation error: " ++ show err

    ["--sexpr", s] -> case Lexer.tokenize s of
      Right a -> case Parser.parse a of
        Right e -> putStrLn $ showSExpr e
        Left f -> putStrLn $ showFailure f
      Left err -> putStrLn $ "indentation error: " ++ show err

    ["--sexpr", "--file", fn] -> do
      s <- readFile fn
      case Lexer.tokenize s of
        Right a -> case Parser.parse a of
          Right e -> putStrLn $ showSExpr e
          Left f -> putStrLn $ showFailure f
        Left err -> putStrLn $ "indentation error: " ++ show err

    ["--serve", "--expr", s] -> do
      execute (\c -> runServer [c]) s

    ["--serve", filename] -> do
      s <- readFile filename
      execute (\c -> runServer [c]) s

    "--expr" : s : rest -> do
      execute (flip run rest) s

    filename : rest -> do
      s <- readFile filename
      execute (flip run rest) s

    _ -> putStrLn "Usage: (TODO)"

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
