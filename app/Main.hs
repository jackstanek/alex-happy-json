module Main where

import Control.Monad

import qualified Lexer as L
import qualified Parser as P
import Control.Monad.Except (runExceptT)


main :: IO ()
main = forever (getLine >>= print . P.runParser)
