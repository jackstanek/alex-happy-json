{
module Lexer (Token(..), Pos, scanTokens) where

import Data.Char
import Numeric (readHex)
}

%wrapper "monadUserState"
%token "Token Pos"

$digit = 0-9
$hex = [0-9a-fA-F]

tokens :-
  <0>       $white+         { skip                 }
  <0>       "{"             { symbol LBrace        }
  <0>       "}"             { symbol RBrace        }
  <0>       "["             { symbol LBracket      }
  <0>       "]"             { symbol RBracket      }
  <0>       ":"             { symbol Colon         }
  <0>       ","             { symbol Comma         }
  <0>       $digit+         { digit                }
  <0>       \"              { begin string         }
  <string>  \\b             { appendChar '\b'      }
  <string>  \\f             { appendChar '\f'      }
  <string>  \\n             { appendChar '\n'      }
  <string>  \\r             { appendChar '\r'      }
  <string>  \\t             { appendChar '\t'      }
  <string>  \\\\            { appendChar '\\'      }
  <string>  \\ \/           { appendChar '/'       }
  <string>  \\ \"           { appendChar '"'       }
  <string>  \\u $hex{4}     { unicodeChar          }
  <string>  [^\"]           { strChar              }
  <string>  \"              { emitStr `andBegin` 0 }

{

data Token a = LBrace a
             | RBrace a
             | LBracket a
             | RBracket a
             | Colon a
             | Comma a
             | StringLit String a
             | NumLit Integer a
             | EOF
  deriving Show

type Pos = (Int, Int)

unpackAlexPosn :: AlexPosn -> Pos
unpackAlexPosn (AlexPn off row col) = (row, col)

symbol tok (pos, _, _, _) _ = return $ tok (unpackAlexPosn pos)

digit (pos, _, _, input) len = return $ NumLit (read $ take len input) (unpackAlexPosn pos)


addChar c = alexGetUserState >>= alexSetUserState . (c:) >> alexMonadScan

appendChar c _ _ = addChar c
strChar (_, _, _, input) _ = addChar $ head input

unicodeChar (_, _, _, input) _ = case readHex $ drop 2 input of
  [(value, _)] -> addChar $ chr value
  _ -> alexError "invalid escape sequence"

emitStr (pos, _, _, input) _ = do
  strAcc <- alexGetUserState
  alexSetUserState ""
  return $ StringLit (reverse strAcc) (unpackAlexPosn pos)

alexEOF = return EOF

type AlexUserState = String
alexInitUserState = ""

-- loop :: Alex [Token Pos]
loop = do
  tok' <- alexMonadScan
  case tok' of
    EOF -> return []
    t -> (t :) <$> loop

scanTokens :: String -> Either String [Token Pos]
scanTokens = flip runAlex loop

}
