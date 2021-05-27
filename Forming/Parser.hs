{-# Language OverloadedStrings #-}
module Forming.Parser where

import Text.Syntactical hiding (Token)

import Forming
import Forming.Lexer (Token(..))

import qualified Forming.Syntax as Syntax
import qualified Forming.Type as Type


--------------------------------------------------------------------------------
parse :: [Token] -> Either (Failure Token) (SExpr Token)
parse = shunt table . map Atom

--------------------------------------------------------------------------------
parseDeclarations expr = case expr of
  List (Atom (Token _ "declarations") : decls) -> traverse parseDeclaration decls
  _ -> Left "TODO parseDeclarations"

parseDeclaration expr = case expr of
  List [Atom (Token _ "="), Atom (Token _ name), Atom (Token _ "input")] ->
    Right (Rule name Unset)
  List (Atom (Token _ "=") : Atom (Token _ name) : [body]) -> do
    body' <- parseExpression body
    Right (Rule name (Exp body'))
  _ -> Left "TODO parseDeclaration"

parseExpression expr = case expr of
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
