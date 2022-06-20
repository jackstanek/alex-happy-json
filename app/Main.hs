module Main where

import Control.Monad

import qualified Lexer as L
import qualified Parser as P

lexLine :: String -> Either String [L.Token L.Pos]
lexLine = L.scanTokens

parseLine :: [L.Token L.Pos] -> Either String (P.JsonExpr L.Pos)
parseLine = Right . P.parse

parse :: String -> Either String (P.JsonExpr L.Pos)
parse = lexLine >=> parseLine

main :: IO ()
main = forever (getLine >>= print . parse)
