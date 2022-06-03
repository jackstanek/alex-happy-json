{
module Lexer (Token(..), Pos, scanTokens) where
}

%wrapper "monad"
%token "Token Pos"

$digit = 0-9
@string = \" ($printable # \")* \"

tokens :-
  $white+ { skip            }
  "{"     { symbol LBrace   }
  "}"     { symbol RBrace   }
  "["     { symbol LBracket }
  "]"     { symbol RBracket }
  ":"     { symbol Colon    }
  ","     { symbol Comma    }
  @string { stringlit       }
  $digit+ { digit           }

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
stringlit (pos, _, _, input) len = return $ StringLit (take (len - 2) (tail input)) (unpackAlexPosn pos)
digit (pos, _, _, input) len = return $ NumLit (read $ take len input) (unpackAlexPosn pos)

alexEOF = return EOF :: Alex (Token Pos)

loop = do
  tok' <- alexMonadScan
  case tok' of
    EOF -> return []
    _ -> (tok' :) <$> loop

scanTokens :: String -> Either String [Token Pos]
scanTokens input = runAlex input loop

}
