-- This file started as a copy of Syntactical/indent.hs.
{-# Language OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec

import GHC.Exts (IsString(..))

import Text.Syntactical hiding (Token)
import Text.Syntactical.Indent (Tree(..), strides')

import qualified Text.Syntactical as Syntactical

import Forming
import Forming.Type

import qualified Forming.Syntax as Syntax

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
    ["--token", s] -> case tokenize s of
      Right a -> putStrLn . unwords $ map toString a
      Left err -> putStrLn $ "indentation error: " ++ show err
    ["--token", "--file", fn] -> do
      s <- readFile fn
      case tokenize s of
        Right a -> putStrLn . unwords $ map toString a
        Left err -> putStrLn $ "indentation error: " ++ show err
    ["--sexpr", s] -> case tokenize s of
      Right a -> case shunt table0 . map Atom $ a of
        Right e -> putStrLn $ showSExpr e
        Left f -> putStrLn $ showFailure f
      Left err -> putStrLn $ "indentation error: " ++ show err
    ["--sexpr", "--file", fn] -> do
      s <- readFile fn
      case tokenize s of
        Right a -> case shunt table0 . map Atom $ a of
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
  case tokenize s of
    Right a -> case shunt table0 . map Atom $ a of
      Right e -> case parseDeclarations e of
        Right rules -> do
          let comp = Computation "TODO" "TODO" "main" rules
          run comp rest
        Left err -> putStrLn $ show err
      Left f -> putStrLn $ showFailure f
    Left err -> putStrLn $ "indentation error: " ++ show err

parseDeclarations expr = case expr of
  List (Atom (Token _ "declarations") : decls) -> traverse parseDeclaration decls
  _ -> Left "TODO"

parseDeclaration expr = case expr of
  List (Atom (Token _ "=") : Atom (Token _ name) : Atom (Token _ "input") : []) ->
    Right (Rule name Unset)
  List (Atom (Token _ "=") : Atom (Token _ name) : [body]) -> do
    body' <- parseExpression body
    Right (Rule name (Exp body'))
  _ -> Left "TODO"

parseExpression expr = case expr of
  Atom (Token _ a) -> do
    return (Syntax.Name a)
  Atom (Int _ a) -> do
    return (Syntax.Int a)
  List [Atom (Token _ op), Atom a, Atom b] -> case op of
    "+" -> do
      a' <- parseExpression (Atom a)
      b' <- parseExpression (Atom b)
      return (Syntax.Add a' b')
  _ -> Left "TODO"


----------------------------------------------------------------------
-- The token type for Syntactical
----------------------------------------------------------------------

data Token =
    Token Source String
  | Int Source Int
  deriving Show

data Source = Source Int Int -- source file line and column
            | Internal       -- generated
  deriving Show

instance IsString Token where
  fromString = Token Internal

instance Syntactical.Token Token where
  toString (Token _ t) = t
  toString (Int _ t) = show t
  operator = myOperator
  consider (Token _ a) (Token _ b) = a == b
  consider (Int _ a) (Int _ b) = a == b
  consider _ _ = False

-- Rewrite the sub-expressions as we apply the operator.
myOperator o as = case pts of
  "()" -> case as of
    [List [Atom (Token Internal ","),a,b]] ->
      tuple [] (head as)
    [as'] -> as'
  "[]" -> case as of
    [as'] -> list "," [Atom (Token Internal "list")] as'
  "{}" -> case as of
    [as'] -> list ";" [Atom (Token Internal "declarations")] as'
  "``" -> case as of
    [a,b,c] -> List [b,a,c]
  _ -> List $ (Atom $ Token Internal pts):as
  where pts = concatMap toString $ symbols o

tuple xs (List [Atom (Token Internal ","),a,b]) = tuple (a:xs) b
tuple xs b = List (a : reverse (b:xs))
  where a = Atom (Token Internal $ ',' : show (length xs + 1))

list c xs (List [Atom (Token Internal c'),a,b]) | c == c' = list c (a:xs) b
list _ xs b = List (reverse (b:xs))

----------------------------------------------------------------------
-- The operator table for Syntactical
----------------------------------------------------------------------

table0 :: Table Token
table0 = buildTable
 [ [ closed "(" Distfix ")"
   , closed "{" Distfix "}"
   ]
 , [ infx LeftAssociative "*"
   , infx LeftAssociative "/"
   ]
 , [ infx LeftAssociative "+"
   , infx LeftAssociative "-"
   ]
 , [ infx LeftAssociative "|"
   ]
 , [ prefx "if" `distfix` "then" `distfix` "else"
   ]
 , [ infx RightAssociative ":"
   , infx RightAssociative "="
   ]
 , [ infx RightAssociative ";"
   ]
 ]

----------------------------------------------------------------------
-- Tokenizing, using a simple indentation scheme (provided by the
-- Indent module).
----------------------------------------------------------------------

type P a = GenParser Char () a

source :: P Source
source = do
  p <- getPosition
  let l = sourceLine p
      c = sourceColumn p
  return $ Source l c

keywords :: [String]
keywords = words "let where of"

tokenize = strides' atom intro "{" "}" ";"

-- Parse an atom.
atom :: P (Tree Token)
atom = empty <|> str <|> ssym <|> sym

-- Parse a keyword that introduces an indentation level.
intro :: P Token
intro = do
  src <- source
  str <- choice (map string keywords)
  return $ Token src str

-- Parse a symbol. A symbol is any consecutive list of non-blank
-- characters except for `,()⟨⟩[], which are each a single symbol.
sym :: P (Tree Token)
sym = try $ do
  src <- source
  x <- noneOf "\t\n "
  if x `elem` ("`,()⟨⟩[]" :: String)
    then spaces >> return (Sym $ Token src [x])
    else do
      xs <- manyTill anyChar (lookAhead $ (oneOf "`,()⟨⟩[]\t\n " >> return ()) <|> eof)
      if (x:xs) `elem` keywords
        then pzero
        else do
          spaces
          if all (`elem` ("0123456789" :: String)) (x:xs)
            then return (Sym . Int src $ read (x:xs))
            else return (Sym . Token src $ x:xs)

-- Parse a symbol delimited by forward slashes (to allow spaces).
ssym :: P (Tree Token)
ssym = try $ do
  src <- source
  _ <- char '/'
  x <- many1 (noneOf "\t\n/")
  _ <- char '/'
  spaces
  return . Sym $ Token src ('/' : x ++ "/")

-- Parse the empty-list symbol.
empty :: P (Tree Token)
empty = try $ do
  src <- source
  _ <- char '['
  spaces
  _ <- char ']'
  spaces
  return . Sym $ Token src "[]"

-- Parse a string literal.
str :: P (Tree Token)
str = try $ do
  src <- source
  _ <- char '"'
  x <- many (noneOf "\t\n\"")
  _ <- char '"'
  spaces
  return . Sym $ Token src ('"' : x ++ "\"")
