{
module Lexer (Token(..), scanTokens) where
}

%wrapper "basic"

$digit = 0-9
@string = \" ($printable # \")* \"

tokens :-
  $white+ ;
  "{"  { const LBrace   }
  "}"  { const RBrace   }
  "["  { const LBracket }
  "]"  { const RBracket }
  ":"  { const Colon    }
  ","  { const Comma    }
  @string { \s -> StringLit (take (length s - 2) (tail s)) }
  $digit+ { \d -> NumLit $ read d }

{

data Token = LBrace
           | RBrace
           | LBracket
           | RBracket
           | Colon
           | Comma
           | StringLit String
           | NumLit Integer
  deriving Show

scanTokens = alexScanTokens

}
