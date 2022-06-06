{
module Lexer (Token(..), Pos, scanTokens) where
}

%wrapper "monadUserState"
%token "Token Pos"

$digit = 0-9
$hex = [0-9a-fA-F]
@esc_seq = \\ ( [bfnrt\"\/] | u $hex{4} )
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
  <string>  [^\"]           { stringchar           }
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

symbol :: (Pos -> Token Pos) -> AlexAction (Token Pos)
symbol tok (pos, _, _, _) _ = return $ tok (unpackAlexPosn pos)

digit :: AlexAction (Token Pos)
digit (pos, _, _, input) len = return $ NumLit (read $ take len input) (unpackAlexPosn pos)

stringchar (pos, _, _, input) _ = do
  curr <- alexGetUserState
  let new = ((head input):curr)
  alexSetUserState curr

emitStr :: AlexAction (Token Pos)
emitStr (pos, _, _, input) _ = do
  strAcc <- alexGetUserState
  alexSetUserState ""
  return $ StringLit strAcc (unpackAlexPosn pos)

alexEOF = return EOF :: Alex (Token Pos)

type AlexUserState = String
alexInitUserState = ""

loop :: Alex [Token Pos]
loop = do
  tok' <- alexMonadScan
  startcode <- alexGetStartCode
  case tok' of
    EOF -> return []
    _ -> (tok' :) <$> loop

scanTokens :: String -> Either String [Token Pos]
scanTokens input = runAlex input loop

}
