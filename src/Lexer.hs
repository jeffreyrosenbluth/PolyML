module Lexer  where

import           Control.Monad             (void)
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer     as L
import           Text.Megaparsec.Text.Lazy


sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "--"
        blockCmnt = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- | 'integer' parses an integer.
integer :: Parser Integer
integer = lexeme L.integer

-- | 'parens' parses something between parenthesis.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | 'semi' parses a semicolon.
semi :: Parser String
semi = symbol ";"

reservedNames :: [String]
reservedNames = [
    "let",
    "in",
    "fix",
    "rec",
    "if",
    "then",
    "else"
  ]

reserved :: String -> Parser ()
reserved w = string w *> notFollowedBy alphaNumChar *> sc

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many (alphaNumChar <|> oneOf "_'")
    check x = if x `elem` reservedNames
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

contents :: Parser a -> Parser a
contents p = sc *> p <* eof
