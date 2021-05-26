{-# Language OverloadedStrings #-}
module Forming.Parser where

import Text.Syntactical hiding (Token)

import Forming
import Forming.Lexer (Token(..))

import qualified Forming.Syntax as Syntax


--------------------------------------------------------------------------------
parse :: [Token] -> Either (Failure Token) (SExpr Token)
parse = shunt table . map Atom

--------------------------------------------------------------------------------
parseDeclarations expr = case expr of
  List (Atom (Token _ "declarations") : decls) -> traverse parseDeclaration decls
  _ -> Left "TODO"

parseDeclaration expr = case expr of
  List [Atom (Token _ "="), Atom (Token _ name), Atom (Token _ "input")] ->
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
