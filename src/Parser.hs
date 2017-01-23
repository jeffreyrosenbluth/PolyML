{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Parser
  ( contents
  , modl
  , parseExpr
  , parseModule
  ) where

import           Data.Text.Lazy            (Text)
import           Text.Megaparsec
import           Text.Megaparsec.Expr
import           Text.Megaparsec.Text.Lazy (Parser)

import           Lexer
import           Syntax

type Binding = (String, Expr)

variable :: Parser Expr
variable = Var <$> identifier

number :: Parser Expr
number = (Lit . LInt . fromIntegral) <$> integer

bool :: Parser Expr
bool = Lit (LBool True)  <$ reserved "True"
   <|> Lit (LBool False) <$ reserved "False"

fix :: Parser Expr
fix = Fix <$> (reserved "fix" *> expr)

lambda :: Parser Expr
lambda = flip (foldr Lam)
            <$> (symbol "\\" *> many identifier)
            <*> (symbol "->" *> expr)

letin :: Parser Expr
letin = Let <$> (reserved "let" *> identifier)
            <*> (symbol "=" *> expr)
            <*> (reserved "in" *> expr)

letrecin :: Parser Expr
letrecin = Let <$> (reserved "let" *> reserved "rec" *> identifier)
               <*> (symbol "=" *> expr)
               <*> (reserved "in" *> expr)

ifthen :: Parser Expr
ifthen = If <$> (reserved "if" *> expr)
            <*> (reserved "then" *> expr)
            <*> (reserved "else" *> expr)

table :: [[Operator Parser Expr]]
table =
  [ [ InfixL (App <$ symbol "")]
    ,
    [ InfixL (Op Mul <$ symbol "*") ]
    ,
    [ InfixL (Op Add <$ symbol "+")
    , InfixL (Op Sub <$ symbol "-")
    ]
    ,
    [ InfixL (Op Eql <$ symbol "==") ]
  ]

atom :: Parser Expr
atom = parens expr
   <|> bool
   <|> number
   <|> ifthen
   <|> fix
   <|> try letrecin
   <|> letin
   <|> lambda
   <|> variable

expr :: Parser Expr
expr = makeExprParser atom table

letdecl :: Parser Binding
letdecl = (\name idents body -> (name, foldr Lam body idents))
      <$> (reserved "let" *> identifier)
      <*> many identifier
      <*> (symbol "=" *> expr)

letrecdecl :: Parser (String, Expr)
letrecdecl = (\name args body -> (name, Fix $ foldr Lam body (name : args)))
         <$> (reserved "let" *> reserved "rec" *> identifier)
         <*> many identifier
         <*> (symbol "=" *> expr)

val :: Parser Binding
val = ("it", ) <$> expr

decl :: Parser Binding
decl = try letrecdecl <|> letdecl <|> val

top :: Parser Binding
top = decl <* optional semi

modl ::  Parser [Binding]
modl = many top

parseExpr :: Text -> Either (ParseError Char Dec) Expr
parseExpr = parse (contents expr) "<stdin>"

parseModule ::  FilePath -> Text
             -> Either (ParseError Char Dec)  [(String, Expr)]
parseModule  = parse (contents modl)
