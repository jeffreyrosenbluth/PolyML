{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Text.Megaparsec
import           Data.Text.Lazy

import Syntax
import Lexer
import Parser

main :: IO ()
main = putStrLn "Hello PolyML"

parseFile :: FilePath -> IO (Either (ParseError Char Dec)  [(String, Expr)])
parseFile fp = do
  f <- readFile fp
  return $ parse (contents modl) "<stdin>" (pack f)
