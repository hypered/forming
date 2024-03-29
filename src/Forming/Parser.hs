-- | This module implements a concrete syntax for Forming. This makes it
-- possible to have Forming scripts and run or serve them using `forming.hs`
-- (as opposed to use Forming as a library with hard-coded Forming
-- expressions).
{-# Language OverloadedStrings #-}
module Forming.Parser where

import Forming.Core
import Forming.Lexer (Token(..))
import Forming.Syntax (Syntax)
import qualified Forming.Syntax as Syntax
import qualified Forming.Type as Type
import Protolude hiding (LeftAssociative, RightAssociative)
import Text.Syntactical hiding (Token)


--------------------------------------------------------------------------------
parse :: [Token] -> Either (Failure Token) (SExpr Token)
parse = shunt table . map Atom

--------------------------------------------------------------------------------
parseRules :: SExpr Token -> Either Text [Rule]
parseRules expr = case expr of
  List (Atom (Token _ "declarations") : decls) -> traverse parseRule decls

  _ -> Left "TODO parseRules"

parseRule :: SExpr Token -> Either Text Rule
parseRule expr = case expr of

  List [Atom (Token _ "="), Atom (Token _ name), Atom (Token _ "input")] ->
    Right (Unset name Nothing)

  List [Atom (Token _ "="), Atom (Token _ name),
      List [Atom (Token _ ":"), Atom (Token _ "input"), Atom (Type _ b)]] ->
    Right (Unset name (Just b))

  List [Atom (Token _ "="), Atom (Token _ name),
      List [Atom (Token _ ":"), Atom (Token _ "input"),
        List (Atom (Token src "|") : rest)]] -> do
    b <- parseEnumeration (List (Atom (Token src "|") : rest))
    Right (Unset name (Just (Type.TEnum b)))

  List (Atom (Token _ "=") : Atom (Token _ name) : [body]) -> do
    body' <- parseExpression body
    Right (Binding name body')

  _ -> do
    expr' <- parseExpression expr
    Right (Naked expr')

parseDefinition :: SExpr Token -> Either Text (Text, Syntax)
parseDefinition expr = case expr of
  List (Atom (Token _ "=") : Atom (Token _ name) : [body]) -> do
    body' <- parseExpression body
    Right (name, body')

  _ -> Left "TODO parseDefinition"

parseInherit :: [SExpr Token] -> Either Text [Text]
parseInherit (Atom (Token _ a) : Atom (Token _ ",") : rest) = do
  members <- parseInherit rest
  return (a : members)
parseInherit [Atom (Token _ a)] = return [a]
parseInherit _ = Left "TODO parseInherit"

parseEnumeration :: SExpr Token -> Either Text [Text]
parseEnumeration (List [Atom (Token _ "|"), as, Atom (Token _ b)]) = do
  items <- parseEnumeration as
  return (items ++ [b])
parseEnumeration (Atom (Token _ a)) = return [a]
parseEnumeration expr = Left ("TODO parseEnumeration: " <> show expr)

parseExpression :: SExpr Token -> Either Text Syntax
parseExpression expr = case expr of
  Atom (Token _ "{}") -> do
    return (Syntax.Object [])
  Atom (Token _ a) -> do
    return (Syntax.Name a)
  Atom (Bool _ a) -> do
    return (Syntax.Bool a)
  Atom (Int _ a) -> do
    return (Syntax.Int a)
  Atom (Decimal _ a) -> do
    return (Syntax.Decimal a)
  Atom (String _ a) -> do
    return (Syntax.String a)
  Atom (Type _ _) -> do
    panic "A type should only appear as an annotation."
    -- ... which is parsed directly below.
  List [Atom (Token _ "ifthenelse"), a, b, c] -> do
    a' <- parseExpression a
    b' <- parseExpression b
    c' <- parseExpression c
    return (Syntax.Cond a' b' c')
  List [Atom (Token _ "declarations"), List (Atom (Token _ "inherit") : names)] -> do
    members <- parseInherit names
    return (Syntax.Names members)
  List (Atom (Token _ "declarations") : decls) -> do
  -- Similar to parseRules but doens't allow to define inputs. Used to define
  -- objects.
    members <- traverse parseDefinition decls
    return (Syntax.Object members)
  List [Atom (Token _ op), a, Atom (Type _ b)] -> case op of
    ":" -> do
      a' <- parseExpression a
      return (Syntax.Annotation a' b)
    _ -> panic $ "parseExpression: " <> show op
  List [Atom (Token _ op), a, List (Atom (Token src "|") : rest)] -> case op of
    ":" -> do
      a' <- parseExpression a
      b <- parseEnumeration (List (Atom (Token src "|") : rest))
      return (Syntax.Annotation a' (Type.TEnum b))
    _ -> panic $ "parseExpression: " <> show op
  List [Atom (Token _ op), a, b] -> do
    op' <- case op of
      "+" -> return Syntax.Add
      "-" -> return Syntax.Sub
      "*" -> return Syntax.Mul
      "/" -> return Syntax.Div
      "==" -> return Syntax.Equal
      "union" -> return Syntax.Union
      _ -> Left ("TODO Unsupported operator " <> op <> ": " <> show expr)
    a' <- parseExpression a
    b' <- parseExpression b
    return (op' a' b')
  _ -> Left ("TODO parseExpression : " <> show expr)


--------------------------------------------------------------------------------
-- The operator table for Syntactical
table :: Table Token
table = buildTable
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
 , [ infx LeftAssociative "=="
   ]
 , [ prefx "if" `distfix` "then" `distfix` "else"
   ]
 , [ infx RightAssociative ":"
   , infx RightAssociative "="
   ]
 , [ infx RightAssociative ";"
   ]
 ]
