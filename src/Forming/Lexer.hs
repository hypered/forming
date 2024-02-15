{-# Language OverloadedStrings #-}
module Forming.Lexer where

import qualified Data.Decimal as Decimal
import Data.List (head)
import Data.String (String)
import qualified Data.Text as T
import GHC.Exts (IsString(..))
import Forming.Type (Type)
import qualified Forming.Type as Type
import Protolude hiding (empty, head, list, many, sourceColumn, sourceLine, sym, try, Type, (<|>))
import Text.ParserCombinators.Parsec
import Text.Syntactical hiding (Token)
import Text.Syntactical.Indent (Tree(..), strides')
import qualified Text.Syntactical as Syntactical


----------------------------------------------------------------------
-- The token type for Syntactical
----------------------------------------------------------------------

data Token =
    Token Source Text
  | Bool Source Bool
  | Int Source Int
  | Decimal Source Decimal.Decimal
  | String Source Text
  | Type Source Type
  deriving Show

data Source = Source Int Int -- source file line and column
            | Internal       -- generated
  deriving Show

instance IsString Token where
  fromString = Token Internal . T.pack

instance Syntactical.Token Token where
  toString (Token _ t) = T.unpack t
  toString (Bool _ a) = show a
  toString (Int _ t) = show t
  toString (Decimal _ a) = show a
  toString (String _ a) = show a
  toString (Type _ a) = show a
  operator = myOperator
  consider (Token _ a) (Token _ b) = a == b
  consider (Bool _ a) (Bool _ b) = a == b
  consider (Int _ a) (Int _ b) = a == b
  consider (Decimal _ a) (Decimal _ b) = a == b
  consider (String _ a) (String _ b) = a == b
  consider (Type _ a) (Type _ b) = a == b
  consider _ _ = False

-- Rewrite the sub-expressions as we apply the operator.
myOperator :: Syntactical.Token a => Op a -> [SExpr Token] -> SExpr Token
myOperator o as = case pts of
  "()" -> case as of
    [List [Atom (Token Internal ","), _, _]] ->
      tuple [] (head as)
    [as'] -> as'
    _ -> panic $ "myOperator: " <> show as
  "[]" -> case as of
    [as'] -> list "," [Atom (Token Internal "list")] as'
    _ -> panic $ "myOperator: " <> show as
  "{}" -> case as of
    [as'] -> list ";" [Atom (Token Internal "declarations")] as'
    _ -> panic $ "myOperator: " <> show as
  "``" -> case as of
    [a,b,c] -> List [b,a,c]
    _ -> panic $ "myOperator: " <> show as
  _ -> List $ Atom (Token Internal $ T.pack pts) : as
  where pts = concatMap toString $ symbols o

tuple :: [SExpr Token] -> SExpr Token -> SExpr Token
tuple xs (List [Atom (Token Internal ","),a,b]) = tuple (a:xs) b
tuple xs b = List (a : reverse (b:xs))
  where a = Atom (Token Internal $ "," <> show (length xs + 1))

list :: Text -> [SExpr Token] -> SExpr Token -> SExpr Token
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

keywords :: [Text]
keywords = words "let where of"

tokenize :: String -> Either ParseError [Token]
tokenize = strides' atom intro "{" "}" ";"

-- Parse an atom.
atom :: P (Tree Token)
atom = empty <|> str <|> ssym <|> sym

-- Parse a keyword that introduces an indentation level.
intro :: P Token
intro = do
  src <- source
  s <- choice (map (string . T.unpack) keywords)
  return $ Token src $ T.pack s

-- Parse a symbol. A symbol is any consecutive list of non-blank
-- characters except for `,()⟨⟩[], which are each a single symbol.
sym :: P (Tree Token)
sym = try $ do
  src <- source
  x <- noneOf "\t\n "
  if x `elem` ("`,()⟨⟩[]" :: [Char])
    then do
      spaces
      return (Sym $ Token src $ T.singleton x)
    else do
      xs <- manyTill anyChar (lookAhead $ (oneOf "`,()⟨⟩[]\t\n " >> return ()) <|> eof)
      let chars = x:xs
      case chars of
        "True" -> do
          spaces
          return (Sym (Bool src True))
        "False" -> do
          spaces
          return (Sym (Bool src False))
        "Bool" -> do
          spaces
          return (Sym (Type src Type.TBool))
        "Int" -> do
          spaces
          return (Sym (Type src Type.TInt))
        "Decimal" -> do
          spaces
          return (Sym (Type src Type.TDecimal))
        "String" -> do
          spaces
          return (Sym (Type src Type.TString))
        _ | T.pack chars `elem` keywords -> do
          pzero
        _ | all (`elem` ("0123456789" :: [Char])) chars -> do
          spaces
          return (Sym (Int src (readInt chars)))
        _ | all (`elem` ("0123456789." :: [Char])) chars -> do
          spaces
          return (Sym (Decimal src (readDecimal chars)))
        _ -> do
          spaces
          return (Sym (Token src $ T.pack chars))

readInt :: [Char] -> Int
readInt s =
  case reads s of
    [(x, "")] -> x
    _ -> panic $ "readInt: " <> T.pack s

readDecimal :: [Char] -> Decimal.Decimal
readDecimal s =
  case reads s of
    [(x, "")] -> x
    _ -> panic $ "readDecimal: " <> T.pack s

-- Parse a symbol delimited by forward slashes (to allow spaces).
ssym :: P (Tree Token)
ssym = try $ do
  src <- source
  _ <- char '/'
  x <- many1 (noneOf "\t\n/")
  _ <- char '/'
  spaces
  return . Sym $ Token src ("/" <> T.pack x <> "/")

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
  return . Sym $ String src $ T.pack x
