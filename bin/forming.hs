-- This file started as a copy of Syntactical/indent.hs.
{-# Language OverloadedStrings #-}
module Main where

import System.Environment (getArgs)

import Text.Syntactical

import Forming (run, Computation(..))

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
    "--expr" : s : rest -> do
      execute s rest
    filename : rest -> do
      s <- readFile filename
      execute s rest
    _ -> putStrLn "Usage: (TODO)"

execute s rest = do
  case Lexer.tokenize s of
    Right a -> case Parser.parse a of
      Right e -> case Parser.parseDeclarations e of
        Right rules -> do
          let comp = Computation "TODO" "TODO" "main" rules
          run comp rest
        Left err -> print err
      Left f -> putStrLn $ showFailure f
    Left err -> putStrLn $ "indentation error: " ++ show err
