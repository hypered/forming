{-# Language OverloadedStrings #-}
module Forming.Lexer where

import qualified Data.Decimal as Decimal
import GHC.Exts (IsString(..))
import Text.ParserCombinators.Parsec

import Text.Syntactical hiding (Token)
import Text.Syntactical.Indent (Tree(..), strides')

import qualified Text.Syntactical as Syntactical


----------------------------------------------------------------------
-- The token type for Syntactical
----------------------------------------------------------------------

data Token =
    Token Source String
  | Int Source Int
  | Decimal Source Decimal.Decimal
  | String Source String
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
  _ -> List $ Atom (Token Internal pts) : as
  where pts = concatMap toString $ symbols o

tuple xs (List [Atom (Token Internal ","),a,b]) = tuple (a:xs) b
tuple xs b = List (a : reverse (b:xs))
  where a = Atom (Token Internal $ ',' : show (length xs + 1))

list c xs (List [Atom (Token Internal c'),a,b]) | c == c' = list c (a:xs) b
list _ xs b = List (reverse (b:xs))


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
    then do
      spaces
      return (Sym $ Token src [x])
    else do
      xs <- manyTill anyChar (lookAhead $ (oneOf "`,()⟨⟩[]\t\n " >> return ()) <|> eof)
      let chars = x:xs
      case chars of
        _ | chars `elem` keywords -> do
          pzero
        _ | all (`elem` ("0123456789" :: String)) chars -> do
          spaces
          return (Sym (Int src (read chars)))
        _ | all (`elem` ("0123456789." :: String)) chars -> do
          spaces
          return (Sym (Decimal src (read chars)))
        _ -> do
          spaces
          return (Sym (Token src chars))

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
  return . Sym $ String src x
