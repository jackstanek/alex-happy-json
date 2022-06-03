module Main where

import Control.Monad (forever)

import qualified Lexer as L
-- import Parser

lexLine :: IO (Either String [L.Token L.Pos])
lexLine = L.scanTokens <$> getLine

main :: IO ()
main = forever (lexLine >>= print)
