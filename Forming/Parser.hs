{-# Language OverloadedStrings #-}
module Forming.Parser where

import Text.Syntactical hiding (Token)

import Forming
import Forming.Lexer (Token(..))
import Forming.Syntax (Syntax)

import qualified Forming.Syntax as Syntax
import qualified Forming.Type as Type


--------------------------------------------------------------------------------
parse :: [Token] -> Either (Failure Token) (SExpr Token)
parse = shunt table . map Atom

--------------------------------------------------------------------------------
parseRules :: SExpr Token -> Either String [Rule]
parseRules expr = case expr of
  List (Atom (Token _ "declarations") : decls) -> traverse parseRule decls
  _ -> Left "TODO parseRules"

parseRule expr = case expr of
  List [Atom (Token _ "="), Atom (Token _ name), Atom (Token _ "input")] ->
    Right (Rule name Unset)
  List (Atom (Token _ "=") : Atom (Token _ name) : [body]) -> do
    body' <- parseExpression body
    Right (Rule name (Exp body'))
  _ -> Left "TODO parseRule"

parseDefinition expr = case expr of
  List (Atom (Token _ "=") : Atom (Token _ name) : [body]) -> do
    body' <- parseExpression body
    Right (name, body')
  _ -> Left "TODO parseDefinition"

parseInherit (Atom (Token _ a) : Atom (Token _ ",") : rest) = do
  members <- parseInherit rest
  return (a : members)
parseInherit [Atom (Token _ a)] = return [a]
parseInherit _ = Left "TODO parseInherit"

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
  Atom (Type _ a) -> do
    error "A type should only appear as an annotation."
    -- ... which is parsed directly below.
  List [Atom (Token _ op), Atom a, Atom (Type _ b)] -> case op of
    ":" -> do
      a' <- parseExpression (Atom a)
      return (Syntax.Annotation a' b)
  List [Atom (Token _ op), Atom a, Atom b] -> case op of
    "+" -> do
      a' <- parseExpression (Atom a)
      b' <- parseExpression (Atom b)
      return (Syntax.Add a' b')
  List [Atom (Token _ "ifthenelse"), Atom a, Atom b, Atom c] -> do
    a' <- parseExpression (Atom a)
    b' <- parseExpression (Atom b)
    c' <- parseExpression (Atom c)
    return (Syntax.Cond a' b' c')
  List [Atom (Token _ "declarations"), List (Atom (Token _ "inherit") : names)] -> do
    members <- parseInherit names
    return (Syntax.Names members)
  List (Atom (Token _ "declarations") : decls) -> do
  -- Similar to parseRules but doens't allow to define inputs. Used to define
  -- objects.
    members <- traverse parseDefinition decls
    return (Syntax.Object members)
  _ -> Left ("TODO parseExpression : " ++ show expr)


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
 , [ prefx "if" `distfix` "then" `distfix` "else"
   ]
 , [ infx RightAssociative ":"
   , infx RightAssociative "="
   ]
 , [ infx RightAssociative ";"
   ]
 ]
