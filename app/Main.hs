module Main where

import Lexer
import Parser

main :: IO ()
main = interact $ show <$> (parse . scanTokens)