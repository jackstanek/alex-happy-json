module Main where

import Control.Monad

import qualified Lexer as L
import qualified Parser as P
import Control.Monad.Except (runExceptT)

prettyParse i = case P.runParser i of
  Right result -> show result
  Left err -> "error: " ++ err

main :: IO ()
main = forever (getLine >>= putStrLn . prettyParse)
