{
module Lexer (Token(..), Pos, scanTokens) where

import Data.Char
import Numeric (readHex)
}

%wrapper "monadUserState"
%token "Maybe (Token Pos)"

$digit = 0-9
$hex = [0-9a-fA-F]
@esc_seq = \\ ( [bfnrt\"\/\\] | u $hex{4} )
@str_contents = ( [^\"] | @esc_seq )

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
  <string>  @esc_seq        { escSeq               }
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

symbol tok (pos, _, _, _) _ = return $ Just $ tok (unpackAlexPosn pos)

digit (pos, _, _, input) len = return $ Just $ NumLit (read $ take len input) (unpackAlexPosn pos)

appendChar c = alexGetUserState >>= alexSetUserState . (c:)

strChar (pos, _, _, input) _ = do
  appendChar $ head input
  return Nothing

escSeq (pos, _, _, input) len =
  let inputStr = take len input in do
    appendChar $ case tail inputStr of
      "\"" -> '"'
      "/"  -> '/'
      "\\" -> '\\'
      "b"  -> '\b'
      "f"  -> '\f'
      "n"  -> '\n'
      "r"  -> '\r'
      "t"  -> '\t'
      ('u':codepoint) -> chr . fst . head . readHex $ codepoint
    return Nothing

emitStr (pos, _, _, input) _ = do
  strAcc <- alexGetUserState
  alexSetUserState ""
  return $ Just $ StringLit (reverse strAcc) (unpackAlexPosn pos)

alexEOF = return (Just EOF) :: Alex (Maybe (Token Pos))

type AlexUserState = String
alexInitUserState = ""

-- loop :: Alex [Token Pos]
loop = do
  tok' <- alexMonadScan
  startcode <- alexGetStartCode
  case tok' of
    Nothing -> loop
    Just EOF -> return []
    Just t -> (t :) <$> loop

scanTokens :: String -> Either String [Token Pos]
scanTokens input = runAlex input loop

}
